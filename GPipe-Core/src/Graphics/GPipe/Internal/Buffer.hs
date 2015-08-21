{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Arrows, TypeFamilies, ScopedTypeVariables,
  FlexibleContexts, FlexibleInstances , TypeSynonymInstances #-}

module Graphics.GPipe.Internal.Buffer 
(
    BufferFormat(..),
    BufferColor,
    Buffer(),
    ToBuffer(),
    B(..), B2(..), B3(..), B4(..),
    toB22, toB21, toB12, toB11,
    Uniform(..), Normalized(..), BPacked(),
    BInput(..),
    newBuffer,
    writeBuffer,
    copyBuffer,
    bufSize, bufName, bufElementSize, bufferLength, bufBElement, bufferWriteInternal, makeBuffer, getUniformAlignment, UniformAlignment
) where

import Graphics.GPipe.Internal.Context

import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc

import Prelude hiding ((.), id)
import Control.Monad.Trans.State
import Control.Category
import Control.Arrow
import Control.Monad (void)
import Foreign.Storable
import Foreign.Ptr
import Control.Monad.IO.Class
import Data.Word
import Data.Int
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Data.IORef
import Control.Applicative ((<$>))
import Control.Monad.Trans.Writer.Lazy
import Linear.V4
import Linear.V3
import Linear.V2
import Linear.V1
import Linear.V0

class BufferFormat f where
    type HostFormat f
    toBuffer :: ToBuffer (HostFormat f) f
    getGlType :: f -> GLenum
    peekPixel :: f -> Ptr () -> IO (HostFormat f)
    getGlPaddedFormat :: f -> GLenum  
    getGlType = error "This is only defined for BufferColor types"
    peekPixel = error "This is only defined for BufferColor types"
    getGlPaddedFormat = error "This is only defined for BufferColor types"

data Buffer os b = Buffer {
                    bufName :: BufferName,                   
                    bufElementSize :: Int,
                    bufferLength :: Int,
                    bufBElement :: BInput -> b,
                    bufWriter :: Ptr () -> HostFormat b -> IO ()
                    }

instance Eq (Buffer os b) where
    a == b = bufName a == bufName b

bufSize :: forall os b. Buffer os b -> Int
bufSize b = bufElementSize b * bufferLength b

type BufferName = IORef GLuint
type Offset = Int
type Stride = Int

data BInput = BInput {bInSkipElems :: Int, bInInstanceDiv :: Int}

type ToBufferInput = (BufferName, Stride, BInput)

type UniformAlignment = Int

data AlignmentMode = Align4 | AlignUniform | AlignPackedIndices | AlignUnknown deriving (Eq)

data ToBuffer a b = ToBuffer
    (Kleisli (StateT Offset (WriterT [Int] (Reader (ToBufferInput, UniformAlignment, AlignmentMode)))) a b) -- Normal = aligned to 4 bytes
    (Kleisli (StateT (Ptr (), [Int]) IO) a b) -- Normal = aligned to 4 bytes
    AlignmentMode 

instance Category ToBuffer where
    id = ToBuffer id id AlignUnknown
    ToBuffer a b m1 . ToBuffer x y m2 = ToBuffer (a.x) (b.y) (comb m1 m2)
        where
            -- If only one uniform or one PackedIndices, use that, otherwise use Align4  
            comb AlignUniform AlignUnknown = AlignUniform
            comb AlignUnknown AlignUniform = AlignUniform
            comb AlignUnknown AlignPackedIndices = AlignPackedIndices 
            comb AlignPackedIndices AlignUnknown = AlignPackedIndices 
            comb AlignUnknown AlignUnknown = AlignUnknown 
            comb _ _ = Align4
    
instance Arrow ToBuffer where
    arr f = ToBuffer (arr f) (arr f) AlignUnknown
    first (ToBuffer a b m) = ToBuffer (first a) (first b) m
    
data B a = B { bName :: IORef GLuint, bOffset :: Int, bStride :: Int, bSkipElems :: Int, bInstanceDiv :: Int}

          
newtype B2 a = B2 { unB2 :: B a } -- Internal
newtype B3 a = B3 { unB3 :: B a } -- Internal
newtype B4 a = B4 { unB4 :: B a } -- Internal

toB22 :: forall a. (Storable a, BufferFormat (B2 a)) => B4 a -> (B2 a, B2 a)
toB21 :: forall a. (Storable a, BufferFormat (B a)) => B3 a -> (B2 a, B a)
toB12 :: forall a. (Storable a, BufferFormat (B a)) => B3 a -> (B a, B2 a)
toB11 :: forall a. (Storable a, BufferFormat (B a)) => B2 a -> (B a, B a)

toB22 (B4 b) = (B2 b, B2 $ b { bOffset = bOffset b + 2 * sizeOf (undefined :: a) }) 
toB21 (B3 b) = (B2 b, b { bOffset = bOffset b + 2*sizeOf (undefined :: a) }) 
toB12 (B3 b) = (b, B2 $ b { bOffset = bOffset b + sizeOf (undefined :: a) }) 
toB11 (B2 b) = (b, b { bOffset = bOffset b + sizeOf (undefined :: a) }) 


newtype Uniform a = Uniform a
newtype Normalized a = Normalized a
newtype BPacked a = BPacked (B a) 

toBufferBUnaligned :: forall a. Storable a => ToBuffer a (B a)
toBufferBUnaligned = ToBuffer 
                (Kleisli $ const static)
                (Kleisli writer)
                Align4
            where
                size = sizeOf (undefined :: a)
                static = do ((name, stride, bIn),_,_) <- lift $ lift ask
                            offset <- get
                            put $ offset + size
                            return $ B name offset stride (bInSkipElems bIn) (bInInstanceDiv bIn)
                writer a = do (ptr,pads) <- get
                              put (ptr `plusPtr` size, pads)
                              liftIO $ poke (castPtr ptr) a
                              return undefined

toBufferB :: forall a. Storable a => ToBuffer a (B a)
toBufferB = toBufferBUnaligned -- Will always be 4 aligned, only 4 size types defined for B1                                
                              
toBufferB2 :: forall a. Storable a => ToBuffer (V2 a) (B2 a)
toBufferB2 = proc ~(V2 a b) -> do
        (if sizeOf (undefined :: a) >= 4 then alignWhen [(AlignUniform, 2 * sizeOf (undefined :: a))] else id) -< () -- Small optimization if someone puts non-usable types in a uniform
        a' <- toBufferBUnaligned  -< a
        toBufferBUnaligned -< b
        returnA -< B2 a' -- Will always be 4 aligned, only 4 size types defined for B2
toBufferB3 :: forall a. Storable a => ToBuffer (V3 a) (B3 a)
toBufferB3 = proc ~(V3 a b c) -> do
        (if sizeOf (undefined :: a) >= 4 then alignWhen [(AlignUniform, 4 * sizeOf (undefined :: a))] else id) -< () -- Small optimization if someone puts non-usable types in a uniform 
        a' <- toBufferBUnaligned -< a
        toBufferBUnaligned -< b
        toBufferBUnaligned -< c
        (if sizeOf (undefined :: a) < 4 then alignWhen [(Align4, 4), (AlignUniform, 4)] else id) -< () -- For types smaller than 4 we need to pad 
        returnA -< B3 a'
toBufferB4 :: forall a. Storable a => ToBuffer (V4 a) (B4 a)
toBufferB4 = proc ~(V4 a b c d) -> do
        (if sizeOf (undefined :: a) >= 4 then alignWhen [(AlignUniform, 4 * sizeOf (undefined :: a))] else id) -< () -- Small optimization if someone puts non-usable types in a uniform 
        a' <- toBufferBUnaligned -< a
        toBufferBUnaligned -< b
        toBufferBUnaligned -< c
        toBufferBUnaligned -< d
        returnA -< B4 a' -- Will always be 4 aligned

instance BufferFormat a => BufferFormat (Uniform a) where
    type HostFormat (Uniform a) = HostFormat a
    toBuffer = arr Uniform . ToBuffer 
                    (Kleisli elementBuilderA)
                    (Kleisli writerA)
                    AlignUniform
        where
            ToBuffer (Kleisli elementBuilderA') (Kleisli writerA') _ = toBuffer :: ToBuffer (HostFormat a) a
            elementBuilderA a = do (_,x,_) <- lift $ lift ask
                                   a' <- elementBuilderA' a
                                   setElemAlignM [(AlignUniform, x)] ()                                   
                                   return a'
            writerA a = do a' <- writerA' a
                           setWriterAlignM ()
                           return a'
instance BufferFormat a => BufferFormat (Normalized a) where
    type HostFormat (Normalized a) = HostFormat a
    toBuffer = arr Normalized . toBuffer                                   
    getGlType (Normalized a) = getGlType a
    getGlPaddedFormat (Normalized a) = case getGlPaddedFormat a of
                                            GL_RGBA_INTEGER -> GL_RGBA
                                            GL_RGB_INTEGER -> GL_RGB
                                            GL_RG_INTEGER -> GL_RG
                                            GL_RED_INTEGER -> GL_RED
                                            x -> x

instance BufferFormat a => BufferFormat (V0 a) where
    type HostFormat (V0 a) = V0 (HostFormat a)
    toBuffer = arr (const V0)
instance BufferFormat a => BufferFormat (V1 a) where
    type HostFormat (V1 a) = V1 (HostFormat a)
    toBuffer = proc ~(V1 a) -> do
                a' <- toBuffer -< a
                returnA -< V1 a'
instance BufferFormat a => BufferFormat (V2 a) where
    type HostFormat (V2 a) = V2 (HostFormat a)
    toBuffer = proc ~(V2 a b) -> do
                (a', b') <- toBuffer -< (a,b)
                returnA -< V2 a' b'
instance BufferFormat a => BufferFormat (V3 a) where
    type HostFormat (V3 a) = V3 (HostFormat a)
    toBuffer = proc ~(V3 a b c) -> do
                (a', b', c') <- toBuffer -< (a, b, c)
                returnA -< V3 a' b' c'
instance BufferFormat a => BufferFormat (V4 a) where
    type HostFormat (V4 a) = V4 (HostFormat a)
    toBuffer = proc ~(V4 a b c d) -> do
                (a', b', c', d') <- toBuffer -< (a, b, c, d)
                returnA -< V4 a' b' c' d'

   
instance (BufferFormat a, BufferFormat b) => BufferFormat (a, b) where
    type HostFormat (a,b) = (HostFormat a, HostFormat b)
    toBuffer = proc ~(a, b) -> do
                a' <- toBuffer -< a
                b' <- toBuffer -< b
                returnA -< (a', b')
instance (BufferFormat a, BufferFormat b, BufferFormat c) => BufferFormat (a, b, c) where
    type HostFormat (a,b,c) = (HostFormat a, HostFormat b, HostFormat c)
    toBuffer = proc ~(a, b, c) -> do
                ((a', b'), c') <- toBuffer -< ((a, b), c)
                returnA -< (a', b', c')
instance (BufferFormat a, BufferFormat b, BufferFormat c, BufferFormat d) => BufferFormat (a, b, c, d) where
    type HostFormat (a,b,c,d) = (HostFormat a, HostFormat b, HostFormat c, HostFormat d)
    toBuffer = proc ~(a, b, c, d) -> do
                ((a', b', c'), d') <- toBuffer -< ((a, b, c), d)
                returnA -< (a', b', c', d')


newBuffer :: (MonadIO m, BufferFormat b) => Int -> ContextT os f m (Buffer os b)
newBuffer elementCount | elementCount < 0 = error "newBuffer, length out of bounds" 
                       | otherwise = do
    (buffer, nameRef, name) <- liftContextIO $ do
                       name <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr) 
                       nameRef <- newIORef name
                       uniAl <- getUniformAlignment
                       let buffer = makeBuffer nameRef elementCount uniAl
                       bname <- readIORef $ bufName buffer
                       glBindBuffer GL_COPY_WRITE_BUFFER bname  
                       glBufferData GL_COPY_WRITE_BUFFER (fromIntegral $ bufSize buffer) nullPtr GL_STREAM_DRAW
                       return (buffer, nameRef, name)
    addContextFinalizer nameRef $ with name (glDeleteBuffers 1)
    addVAOBufferFinalizer nameRef
    return buffer 

bufferWriteInternal :: Buffer os f -> Ptr () -> [HostFormat f] -> IO (Ptr ())         
bufferWriteInternal b ptr (x:xs) = do bufWriter b ptr x
                                      bufferWriteInternal b (ptr `plusPtr` bufElementSize b) xs
bufferWriteInternal _ ptr [] = return ptr

writeBuffer :: MonadIO m => [HostFormat b] -> Buffer os b -> Int -> ContextT os f m ()
writeBuffer elems buffer offset | offset < 0 || offset >= bufferLength buffer = error "writeBuffer, offset out of bounds"
                                | otherwise = 
    let maxElems = max 0 $ bufferLength buffer - offset
        elemSize = bufElementSize buffer
        off = fromIntegral $ offset * elemSize
        
    in liftContextIOAsync $ do 
                          bname <- readIORef $ bufName buffer
                          glBindBuffer GL_COPY_WRITE_BUFFER bname
                          ptr <- glMapBufferRange GL_COPY_WRITE_BUFFER off (fromIntegral $maxElems * elemSize) (GL_MAP_WRITE_BIT + GL_MAP_FLUSH_EXPLICIT_BIT)
                          end <- bufferWriteInternal buffer ptr (take maxElems elems)
                          glFlushMappedBufferRange GL_COPY_WRITE_BUFFER off (fromIntegral $ end `minusPtr` ptr) 
                          void $ glUnmapBuffer GL_COPY_WRITE_BUFFER 

copyBuffer :: MonadIO m => Buffer os b -> Int -> Buffer os b -> Int -> Int -> ContextT os f m ()
copyBuffer bFrom from bTo to len | from < 0 || from >= bufferLength bFrom = error "writeBuffer, source offset out of bounds"
                                 | to < 0 || to >= bufferLength bTo = error "writeBuffer, destination offset out of bounds"
                                 | len < 0 = error "writeBuffer, length out of bounds"
                                 | len + from > bufferLength bFrom = error "writeBuffer, source buffer too small"
                                 | len + to > bufferLength bTo = error "writeBuffer, destination buffer too small"
                                 | otherwise = liftContextIOAsync $ do 
                                                      bnamef <- readIORef $ bufName bFrom
                                                      bnamet <- readIORef $ bufName bTo
                                                      glBindBuffer GL_COPY_READ_BUFFER bnamef
                                                      glBindBuffer GL_COPY_WRITE_BUFFER bnamet
                                                      let elemSize = bufElementSize bFrom -- same as for bTo
                                                      glCopyBufferSubData GL_COPY_READ_BUFFER GL_COPY_WRITE_BUFFER (fromIntegral $ from * elemSize) (fromIntegral $ to * elemSize) (fromIntegral $ len * elemSize)   

----------------------------------------------

alignWhen :: [(AlignmentMode, Int)] -> ToBuffer a a
alignWhen x = ToBuffer (Kleisli $ setElemAlignM x) (Kleisli setWriterAlignM) AlignUniform where

setElemAlignM :: [(AlignmentMode, Int)] -> b -> StateT Offset (WriterT [Int] (Reader (ToBufferInput, UniformAlignment, AlignmentMode))) b
setElemAlignM x a = do
                     (_,_,m) <- lift $ lift ask
                     pad <- case lookup m x of
                                Nothing -> return 0
                                Just al -> do
                                    offset <- get
                                    let pad = al - 1 - ((offset - 1) `mod` al)
                                    put $ offset + pad
                                    return pad
                     lift $ tell [pad]
                     return a   
setWriterAlignM :: b -> StateT (Ptr a, [Int]) IO b
setWriterAlignM a = do (ptr, pad:pads) <- get
                       put (ptr `plusPtr` pad, pads)
                       return a
        


getUniformAlignment :: IO Int
getUniformAlignment = fromIntegral <$> alloca (\ ptr -> glGetIntegerv GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT ptr >> peek ptr)
            
makeBuffer :: forall os b. BufferFormat b => BufferName -> Int -> UniformAlignment -> Buffer os b
makeBuffer name elementCount uniformAlignment  = do
    let ToBuffer a b m = toBuffer :: ToBuffer (HostFormat b) b
        err = error "toBuffer or toVertex are creating values that are dependant on the actual HostFormat values, this is not allowed since it doesn't allow static creation of shaders" :: HostFormat b
        elementM = runWriterT (runStateT (runKleisli a err) 0)
        ((_,elementSize),pads) = runReader elementM ((name, undefined, undefined), uniformAlignment, m)
        elementF bIn = (fst . fst) $ runReader elementM ((name, elementSize, bIn), uniformAlignment, m)
        writer ptr x = void $ runStateT (runKleisli b x) (ptr,pads)
    Buffer name elementSize elementCount elementF writer

type family BufferColor c h where
    BufferColor Float Int32 = Normalized (B Int32)
    BufferColor Float Float = B Float
    BufferColor Int Int32   = B Int32

    BufferColor Word Word32 = B Word32
    BufferColor Word Word16 = BPacked Word16
    BufferColor Word Word8  = BPacked Word8

    BufferColor (V2 Float) (V2 Int32) = Normalized (B2 Int32) 
    BufferColor (V2 Float) (V2 Int16) = Normalized (B2 Int16) 
    BufferColor (V2 Float) (V2 Float) = B2 Float

    BufferColor (V2 Int) (V2 Int32) = B2 Int32
    BufferColor (V2 Int) (V2 Int16) = B2 Int16

    BufferColor (V2 Word) (V2 Word32) = B2 Word32
    BufferColor (V2 Word) (V2 Word16) = B2 Word16

    BufferColor (V3 Float) (V3 Int32) = Normalized (B3 Int32) 
    BufferColor (V3 Float) (V3 Int16) = Normalized (B3 Int16)
    BufferColor (V3 Float) (V3 Int8)    = Normalized (B3 Int8)
    BufferColor (V3 Float) (V3 Float) = B3 Float

    BufferColor (V3 Int) (V3 Int32) = B3 Int32
    BufferColor (V3 Int) (V3 Int16) = B3 Int16
    BufferColor (V3 Int) (V3 Int8)    = B3 Int8

    BufferColor (V3 Word) (V3 Word32) = B3 Word32
    BufferColor (V3 Word) (V3 Word16) = B3 Word16
    BufferColor (V3 Word) (V3 Word8)    = B3 Word8

    BufferColor (V4 Float) (V4 Int32) = Normalized (B4 Int32)
    BufferColor (V4 Float) (V4 Int16) = Normalized (B4 Int16)
    BufferColor (V4 Float) (V4 Int8)     = Normalized (B4 Int8)
    BufferColor (V4 Float) (V4 Float) = B4 Float

    BufferColor (V4 Int) (V4 Int32) = B4 Int32
    BufferColor (V4 Int) (V4 Int16) = B4 Int16
    BufferColor (V4 Int) (V4 Int8)     = B4 Int8

    BufferColor (V4 Word) (V4 Word32) = B4 Word32
    BufferColor (V4 Word) (V4 Word16) = B4 Word16
    BufferColor (V4 Word) (V4 Word8)     = B4 Word8

peekPixel1 :: Storable a => t -> Ptr x -> IO a
peekPixel1 _ = peek . castPtr 
peekPixel2 :: (Storable a) => t -> Ptr x -> IO (V2 a)
peekPixel2 _ ptr = do x <- peek (castPtr ptr)
                      y <- peekElemOff (castPtr ptr ) 1
                      return (V2 x y) 
peekPixel3 :: (Storable a) => t -> Ptr x -> IO (V3 a)
peekPixel3 _ ptr = do x <- peek (castPtr ptr)
                      y <- peekElemOff (castPtr ptr ) 1
                      z <- peekElemOff (castPtr ptr ) 2
                      return (V3 x y z) 
peekPixel4 :: (Storable a) => t -> Ptr x -> IO (V4 a)
peekPixel4 _ ptr = do x <- peek (castPtr ptr)
                      y <- peekElemOff (castPtr ptr ) 1
                      z <- peekElemOff (castPtr ptr ) 2
                      w <- peekElemOff (castPtr ptr ) 3
                      return (V4 x y z w) 


instance BufferFormat (B Int32) where
    type HostFormat (B Int32) = Int32
    toBuffer = toBufferB
    getGlType _ = GL_INT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = GL_RED_INTEGER

instance BufferFormat (B Word32) where
    type HostFormat (B Word32) = Word32
    toBuffer = toBufferB
    getGlType _ = GL_UNSIGNED_INT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = GL_RED_INTEGER
   
instance BufferFormat (BPacked Word16) where
    type HostFormat (BPacked Word16) = Word16
    toBuffer = let ToBuffer a b _ = toBufferB :: ToBuffer Word16 (B Word16) in arr BPacked . ToBuffer a b AlignPackedIndices 
    getGlType _ = GL_UNSIGNED_SHORT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = GL_RED_INTEGER

instance BufferFormat (BPacked Word8) where
    type HostFormat (BPacked Word8) = Word8
    toBuffer = let ToBuffer a b _ = toBufferB :: ToBuffer Word8 (B Word8) in arr BPacked . ToBuffer a b AlignPackedIndices 
    getGlType _ = GL_UNSIGNED_BYTE
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = GL_RED_INTEGER      

instance BufferFormat (B Float) where
    type HostFormat (B Float) = Float
    toBuffer = toBufferB
    getGlType _ = GL_FLOAT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = GL_RED

instance BufferFormat (B2 Int32) where
    type HostFormat (B2 Int32) = V2 Int32
    toBuffer = toBufferB2
    getGlType _ = GL_INT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = GL_RG_INTEGER

instance BufferFormat (B2 Int16) where
    type HostFormat (B2 Int16) = V2 Int16
    toBuffer = toBufferB2
    getGlType _ = GL_SHORT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = GL_RG_INTEGER

instance BufferFormat (B2 Word32) where
    type HostFormat (B2 Word32) = V2 Word32
    toBuffer = toBufferB2
    getGlType _ = GL_UNSIGNED_INT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = GL_RG_INTEGER

instance BufferFormat (B2 Word16) where
    type HostFormat (B2 Word16) = V2 Word16
    toBuffer = toBufferB2
    getGlType _ = GL_UNSIGNED_SHORT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = GL_RG_INTEGER

instance BufferFormat (B2 Float) where
    type HostFormat (B2 Float) = V2 Float
    toBuffer = toBufferB2
    getGlType _ = GL_FLOAT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = GL_RG

instance BufferFormat (B3 Int32) where
    type HostFormat (B3 Int32) = V3 Int32
    toBuffer = toBufferB3
    getGlType _ = GL_INT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = GL_RGB_INTEGER

instance BufferFormat (B3 Int16) where
    type HostFormat (B3 Int16) = V3 Int16
    toBuffer = toBufferB3
    getGlType _ = GL_SHORT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B3 Int8) where
    type HostFormat (B3 Int8) = V3 Int8
    toBuffer = toBufferB3
    getGlType _ = GL_BYTE
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B3 Word32) where
    type HostFormat (B3 Word32) = V3 Word32
    toBuffer = toBufferB3
    getGlType _ = GL_UNSIGNED_INT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = GL_RGB_INTEGER

instance BufferFormat (B3 Word16) where
    type HostFormat (B3 Word16) = V3 Word16
    toBuffer = toBufferB3
    getGlType _ = GL_UNSIGNED_SHORT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B3 Word8) where
    type HostFormat (B3 Word8) = V3 Word8
    toBuffer = toBufferB3
    getGlType _ = GL_UNSIGNED_BYTE
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B3 Float) where
    type HostFormat (B3 Float) = V3 Float
    toBuffer = toBufferB3
    getGlType _ = GL_FLOAT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = GL_RGB

instance BufferFormat (B4 Int32) where
    type HostFormat (B4 Int32) = V4 Int32
    toBuffer = toBufferB4
    getGlType _ = GL_INT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B4 Int16) where
    type HostFormat (B4 Int16) = V4 Int16
    toBuffer = toBufferB4
    getGlType _ = GL_SHORT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B4 Int8) where
    type HostFormat (B4 Int8) = V4 Int8
    toBuffer = toBufferB4
    getGlType _ = GL_BYTE
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B4 Word32) where
    type HostFormat (B4 Word32) = V4 Word32
    toBuffer = toBufferB4
    getGlType _ = GL_UNSIGNED_INT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B4 Word16) where
    type HostFormat (B4 Word16) = V4 Word16
    toBuffer = toBufferB4
    getGlType _ = GL_UNSIGNED_SHORT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B4 Word8) where
    type HostFormat (B4 Word8) = V4 Word8
    toBuffer = toBufferB4
    getGlType _ = GL_UNSIGNED_BYTE
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = GL_RGBA_INTEGER

instance BufferFormat (B4 Float) where
    type HostFormat (B4 Float) = V4 Float
    toBuffer = toBufferB4
    getGlType _ = GL_FLOAT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = GL_RGBA
    

    