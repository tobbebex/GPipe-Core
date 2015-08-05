{-# LANGUAGE Arrows, TypeFamilies, ScopedTypeVariables,
  FlexibleContexts, FlexibleInstances , TypeSynonymInstances #-}

module Graphics.GPipe.Internal.Buffer 
(
    BufferFormat(..),
    BufferColor,
    Buffer(),
    ToBuffer(),
    B(..), B2(..), B3(..), B4(..),
    BUniform(..), BNormalized(..), BPacked(),
    BInput(..),
    newBuffer,
    writeBuffer,
    copyBuffer,
    bufSize, bufName, bufElementSize, bufElementCount, bufBElement, bufferWriteInternal, makeBuffer, getUniformAlignment, UniformAlignment
) where

import Graphics.GPipe.Internal.Context

import Graphics.Rendering.OpenGL.Raw.Core33
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc

import Prelude hiding ((.), id)
import Control.Monad.Trans.State
import Control.Category
import Control.Arrow
import Control.Monad (void, when)
import Foreign.Storable
import Foreign.Ptr
import Control.Monad.IO.Class
import Data.Word
import Data.Int
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Foreign.C.Types
import Data.IORef
import Control.Applicative ((<$>))

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
                    bufElementCount :: Int,
                    bufBElement :: BInput -> b,
                    bufWriter :: Ptr () -> HostFormat b -> IO ()
                    }

instance Eq (Buffer os b) where
    a == b = bufName a == bufName b

bufSize :: forall os b. Buffer os b -> Int
bufSize b = bufElementSize b * bufElementCount b

type BufferName = IORef CUInt
type Offset = Int
type Stride = Int

data BInput = BInput {bInSkipElems :: Int, bInInstanceDiv :: Int}

type ToBufferInput = (BufferName, Stride, BInput)

type UniformAlignment = Int

data AlignmentMode = Align4 | AlignUniform | AlignPackedIndices | AlignUnknown deriving (Eq)

data ToBuffer a b = ToBuffer
    (Kleisli (StateT Offset (Reader (ToBufferInput, UniformAlignment, AlignmentMode))) a b) -- Normal = aligned to 4 bytes
    (Kleisli (StateT (Ptr ()) (ReaderT (Ptr (), UniformAlignment, AlignmentMode) IO)) a b) -- Normal = aligned to 4 bytes
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
    
data B a = B { bName :: IORef CUInt, bOffset :: Int, bStride :: Int, bSkipElems :: Int, bInstanceDiv :: Int}

          
newtype B2 a = B2 { unB2 :: B a } -- Internal
newtype B3 a = B3 { unB3 :: B a } -- Internal
newtype B4 a = B4 { unB4 :: B a } -- Internal

newtype BUniform a = BUniform a
newtype BNormalized a = BNormalized a
newtype BPacked a = BPacked (B a) 

toBufferBUnaligned :: forall a. Storable a => ToBuffer a (B a)
toBufferBUnaligned = ToBuffer 
                (Kleisli $ const static)
                (Kleisli writer)
                Align4
            where
                size = sizeOf (undefined :: a)
                static = do ((name, stride, bIn),_,_) <- lift ask
                            offset <- get
                            put $ offset + size
                            return $ B name offset stride (bInSkipElems bIn) (bInInstanceDiv bIn)
                writer a = do ptr <- get
                              put $ ptr `plusPtr` size
                              liftIO $ poke (castPtr ptr) a
                              return undefined

toBufferB :: forall a. Storable a => ToBuffer a (B a)
toBufferB = alignWhen AlignUniform (sizeOf (undefined :: a)) >>> toBufferBUnaligned >>> alignWhen Align4 4                               
                              
toBufferB2 :: forall a. (BufferFormat (B a), a ~ HostFormat (B a), Storable a) => ToBuffer (a,a) (B2 a)
toBufferB2 = proc (a, b) -> do
        alignWhen AlignUniform (2 * sizeOf (undefined :: a)) -< () -- For align4, we are always at 4 alignment here 
        a' <- toBufferBUnaligned  -< a
        toBufferBUnaligned -< b
        alignWhen Align4 4 -< () -- For uniform, here we are already aligned to 2*size 
        returnA -< B2 a'
toBufferB3 :: forall a. (BufferFormat (B a), a ~ HostFormat (B a), Storable a) => ToBuffer (a,a,a) (B3 a)
toBufferB3 = proc (a, b, c) -> do
        alignWhen AlignUniform (4 * sizeOf (undefined :: a)) -< () -- For align4, we are always at 4 alignment here
        a' <- toBufferBUnaligned -< a
        toBufferBUnaligned -< b
        toBufferBUnaligned -< c
        alignWhen Align4 4 -< ()
        alignWhen AlignUniform (4 * sizeOf (undefined :: a)) -< ()
        returnA -< B3 a'
toBufferB4 :: forall a. (BufferFormat (B a), a ~ HostFormat (B a), Storable a) => ToBuffer (a,a,a,a) (B4 a)
toBufferB4 = proc (a, b, c, d) -> do
        alignWhen AlignUniform (4 * sizeOf (undefined :: a)) -< () -- For align4, we are always at 4 alignment here
        a' <- toBufferBUnaligned -< a
        toBufferBUnaligned -< b
        toBufferBUnaligned -< c
        toBufferBUnaligned -< d
        alignWhen Align4 4 -< () -- For uniform, here we are already aligned to 4*size 
        returnA -< B4 a'

instance BufferFormat a => BufferFormat (BUniform a) where
    type HostFormat (BUniform a) = HostFormat a
    toBuffer = arr BUniform . ToBuffer 
                    (Kleisli elementBuilderA)
                    (Kleisli writerA)
                    AlignUniform
        where
            ToBuffer (Kleisli elementBuilderA') (Kleisli writerA') _ = toBuffer :: ToBuffer (HostFormat a) a
            elementBuilderA a = do (_,x,_) <- lift ask
                                   a' <- elementBuilderA' a                                   
                                   let ToBuffer (Kleisli m) _ _ = alignWhen AlignUniform x
                                   m ()
                                   return a'
            writerA a = do (_,x,_) <- lift ask
                           a' <- writerA' a
                           let ToBuffer _ (Kleisli m) _ = alignWhen AlignUniform x
                           m ()    
                           return a'
instance BufferFormat a => BufferFormat (BNormalized a) where
    type HostFormat (BNormalized a) = HostFormat a
    toBuffer = arr BNormalized . toBuffer                                   
    getGlType (BNormalized a) = getGlType a
    getGlPaddedFormat (BNormalized a) = case getGlPaddedFormat a of
                                            x | x == gl_RGBA_INTEGER -> gl_RGBA
                                              | x == gl_RGB_INTEGER -> gl_RGB
                                              | x == gl_RG_INTEGER -> gl_RG
                                              | x == gl_RED_INTEGER -> gl_RED
                                              | otherwise -> x
   
instance (BufferFormat a, BufferFormat b) => BufferFormat (a, b) where
    type HostFormat (a,b) = (HostFormat a, HostFormat b)
    toBuffer = proc (a, b) -> do
                a' <- toBuffer -< a
                b' <- toBuffer -< b
                returnA -< (a', b')
instance (BufferFormat a, BufferFormat b, BufferFormat c) => BufferFormat (a, b, c) where
    type HostFormat (a,b,c) = (HostFormat a, HostFormat b, HostFormat c)
    toBuffer = proc (a, b, c) -> do
                ((a', b'), c') <- toBuffer -< ((a, b), c)
                returnA -< (a', b', c')
instance (BufferFormat a, BufferFormat b, BufferFormat c, BufferFormat d) => BufferFormat (a, b, c, d) where
    type HostFormat (a,b,c,d) = (HostFormat a, HostFormat b, HostFormat c, HostFormat d)
    toBuffer = proc (a, b, c, d) -> do
                ((a', b', c'), d') <- toBuffer -< ((a, b, c), d)
                returnA -< (a', b', c', d')

-- TODO: Add packed pixelformats with special packing writers                

newBuffer :: (MonadIO m, BufferFormat b) => Int -> ContextT os f m (Buffer os b)
newBuffer elementCount = do
    (buffer, nameRef, name) <- liftContextIO $ do
                       name <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr) 
                       nameRef <- newIORef name
                       uniAl <- getUniformAlignment
                       let buffer = makeBuffer nameRef elementCount uniAl
                       bname <- readIORef $ bufName buffer
                       glBindBuffer gl_COPY_WRITE_BUFFER (fromIntegral bname )  
                       glBufferData gl_COPY_WRITE_BUFFER (fromIntegral $ bufSize buffer) nullPtr gl_DYNAMIC_DRAW
                       return (buffer, nameRef, name)
    addContextFinalizer nameRef $ with name (glDeleteBuffers 1)
    addVAOBufferFinalizer nameRef
    return buffer 

bufferWriteInternal :: Buffer os f -> Ptr () -> [HostFormat f] -> IO (Ptr ())         
bufferWriteInternal b ptr (x:xs) = do bufWriter b ptr x
                                      bufferWriteInternal b (ptr `plusPtr` bufElementSize b) xs
bufferWriteInternal _ ptr [] = return ptr

writeBuffer :: MonadIO m => [HostFormat f] -> Int -> Buffer os f -> ContextT os f2 m ()
writeBuffer elems offset buffer = 
    let maxElems = max 0 $ bufElementCount buffer - offset
        elemSize = bufElementSize buffer
        off = fromIntegral $ offset * elemSize
        
    in liftContextIOAsync $ do 
                          bname <- readIORef $ bufName buffer
                          glBindBuffer gl_COPY_WRITE_BUFFER bname
                          ptr <- glMapBufferRange gl_COPY_WRITE_BUFFER off (fromIntegral $ maxElems * elemSize) (gl_MAP_WRITE_BIT + gl_MAP_FLUSH_EXPLICIT_BIT)
                          end <- bufferWriteInternal buffer ptr (take maxElems elems)
                          glFlushMappedBufferRange gl_COPY_WRITE_BUFFER off (fromIntegral $ end `minusPtr` ptr) 
                          void $ glUnmapBuffer gl_COPY_WRITE_BUFFER 

{-
readBuffer :: MonadIO m => Int -> Int -> Buffer os f -> (HostFormat f -> a -> m a) -> a ->  ContextT os f2 m a
readBuffer = undefined
-}

copyBuffer :: MonadIO m => Int -> Int -> Buffer os f -> Int -> Buffer os f -> ContextT os f2 m ()
copyBuffer len from bFrom to bTo = liftContextIOAsync $ do 
                                                      bnamef <- readIORef $ bufName bFrom
                                                      bnamet <- readIORef $ bufName bTo
                                                      glBindBuffer gl_COPY_READ_BUFFER bnamef
                                                      glBindBuffer gl_COPY_WRITE_BUFFER bnamet
                                                      let elemSize = bufElementSize bFrom -- same as for bTo
                                                      glCopyBufferSubData gl_COPY_READ_BUFFER gl_COPY_WRITE_BUFFER (fromIntegral $ from * elemSize) (fromIntegral $ to * elemSize) (fromIntegral $ len * elemSize)   

----------------------------------------------

alignWhen :: AlignmentMode -> Int -> ToBuffer a a
alignWhen m x = ToBuffer (Kleisli setElemAlignM) (Kleisli setWriterAlignM) AlignUniform where
        setElemAlignM a = do (_,_,m') <- lift ask
                             when (m == m') $ do
                                offset <- get
                                put $ alignedTo x offset
                             return a   
        setWriterAlignM a = do (basePtr, _,m') <- lift ask
                               when (m == m') $ do
                                    ptr <- get
                                    let base = ptrToWordPtr basePtr
                                        p = ptrToWordPtr ptr
                                    put $ wordPtrToPtr $ base + alignedTo (fromIntegral x) (p - base)
                               return a
        alignedTo a b  = b + a - 1 - ((b - 1) `mod` a)


getUniformAlignment :: IO Int
getUniformAlignment = fromIntegral <$> alloca (\ ptr -> glGetIntegerv gl_UNIFORM_BUFFER_OFFSET_ALIGNMENT ptr >> peek ptr)
            
makeBuffer :: forall os b. BufferFormat b => BufferName -> Int -> UniformAlignment -> Buffer os b
makeBuffer name elementCount uniformAlignment  = do
    let ToBuffer a b m = toBuffer :: ToBuffer (HostFormat b) b
        err = error "toBuffer or toVertex are creating values that are dependant on the actual HostFormat values, this is not allowed since it doesn't allow static creation of shaders" :: HostFormat b
        elementM = runStateT (runKleisli a err) 0
        elementSize = snd $ runReader elementM ((name, undefined, undefined), uniformAlignment, m)
        elementF bIn = fst $ runReader elementM ((name, elementSize, bIn), uniformAlignment, m)
        writer ptr x = void $ runReaderT (runStateT (runKleisli b x) ptr) (ptr, uniformAlignment, m)
    Buffer name elementSize elementCount elementF writer

type family BufferColor f where
    BufferColor (BNormalized (B Int32)) = Float
    BufferColor (BNormalized (B Int16)) = Float
    BufferColor (BNormalized (B Int8)) = Float
    BufferColor (B Float) = Float

    BufferColor (B Int32) = Int
    BufferColor (B Int16) = Int
    BufferColor (B Int8) = Int

    BufferColor (B Word32) = Word
    BufferColor (B Word16) = Word
    BufferColor (B Word8) = Word
    BufferColor (BPacked Word16) = Word
    BufferColor (BPacked Word8) = Word

    BufferColor (BNormalized (B2 Int32)) = (Float, Float)
    BufferColor (BNormalized (B2 Int16)) = (Float, Float)
    BufferColor (BNormalized (B2 Int8)) = (Float, Float)
    BufferColor (B2 Float) = (Float, Float)

    BufferColor (B2 Int32) = (Int, Int)
    BufferColor (B2 Int16) = (Int, Int)
    BufferColor (B2 Int8) = (Int, Int)

    BufferColor (B2 Word32) = (Word, Word)
    BufferColor (B2 Word16) = (Word, Word)
    BufferColor (B2 Word8) = (Word, Word)

    BufferColor (BNormalized (B3 Int32)) = (Float, Float, Float)
    BufferColor (BNormalized (B3 Int16)) = (Float, Float, Float)
    BufferColor (BNormalized (B3 Int8)) = (Float, Float, Float)
    BufferColor (B3 Float) = (Float, Float, Float)

    BufferColor (B3 Int32) = (Int, Int, Int)
    BufferColor (B3 Int16) = (Int, Int, Int)
    BufferColor (B3 Int8) = (Int, Int, Int)

    BufferColor (B3 Word32) = (Word, Word, Word)
    BufferColor (B3 Word16) = (Word, Word, Word)
    BufferColor (B3 Word8) = (Word, Word, Word)

    BufferColor (BNormalized (B4 Int32)) = (Float, Float, Float, Float)
    BufferColor (BNormalized (B4 Int16)) = (Float, Float, Float, Float)
    BufferColor (BNormalized (B4 Int8)) = (Float, Float, Float, Float)
    BufferColor (B4 Float) = (Float, Float, Float, Float)

    BufferColor (B4 Int32) = (Int, Int, Int, Int)
    BufferColor (B4 Int16) = (Int, Int, Int, Int)
    BufferColor (B4 Int8) = (Int, Int, Int, Int)

    BufferColor (B4 Word32) = (Word, Word, Word, Word)
    BufferColor (B4 Word16) = (Word, Word, Word, Word)
    BufferColor (B4 Word8) = (Word, Word, Word, Word)

peekPixel1 :: Storable a => t -> Ptr x -> IO a
peekPixel1 _ = peek . castPtr 
peekPixel2 :: (Storable a) => t -> Ptr x -> IO (a, a)
peekPixel2 _ ptr = do x <- peek (castPtr ptr)
                      y <- peekElemOff (castPtr ptr ) 1
                      return (x,y) 
peekPixel3 :: (Storable a) => t -> Ptr x -> IO (a, a, a)
peekPixel3 _ ptr = do x <- peek (castPtr ptr)
                      y <- peekElemOff (castPtr ptr ) 1
                      z <- peekElemOff (castPtr ptr ) 2
                      return (x,y,z) 
peekPixel4 :: (Storable a) => t -> Ptr x -> IO (a, a, a, a)
peekPixel4 _ ptr = do x <- peek (castPtr ptr)
                      y <- peekElemOff (castPtr ptr ) 1
                      z <- peekElemOff (castPtr ptr ) 2
                      w <- peekElemOff (castPtr ptr ) 3
                      return (x,y,z,w) 


instance BufferFormat (B Int32) where
    type HostFormat (B Int32) = Int32
    toBuffer = toBufferB
    getGlType _ = gl_INT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = gl_RED_INTEGER

instance BufferFormat (B Int16) where
    type HostFormat (B Int16) = Int16
    toBuffer = toBufferB
    getGlType _ = gl_SHORT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = gl_RG_INTEGER

instance BufferFormat (B Int8) where
    type HostFormat (B Int8) = Int8
    toBuffer = toBufferB
    getGlType _ = gl_BYTE
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B Word32) where
    type HostFormat (B Word32) = Word32
    toBuffer = toBufferB
    getGlType _ = gl_UNSIGNED_INT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = gl_RED_INTEGER

instance BufferFormat (B Word16) where
    type HostFormat (B Word16) = Word16
    toBuffer = toBufferB
    getGlType _ = gl_UNSIGNED_SHORT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = gl_RG_INTEGER

instance BufferFormat (B Word8) where
    type HostFormat (B Word8) = Word8
    toBuffer = toBufferB
    getGlType _ = gl_UNSIGNED_BYTE
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = gl_RGBA_INTEGER
    
instance BufferFormat (BPacked Word16) where
    type HostFormat (BPacked Word16) = Word16
    toBuffer = let ToBuffer a b _ = toBufferB :: ToBuffer Word16 (B Word16) in arr BPacked . ToBuffer a b AlignPackedIndices 
    getGlType _ = gl_SHORT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = gl_RED_INTEGER

instance BufferFormat (BPacked Word8) where
    type HostFormat (BPacked Word8) = Word8
    toBuffer = let ToBuffer a b _ = toBufferB :: ToBuffer Word8 (B Word8) in arr BPacked . ToBuffer a b AlignPackedIndices 
    getGlType _ = gl_BYTE
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = gl_RED_INTEGER      

instance BufferFormat (B Float) where
    type HostFormat (B Float) = Float
    toBuffer = toBufferB
    getGlType _ = gl_FLOAT
    peekPixel = peekPixel1 
    getGlPaddedFormat _ = gl_RED

instance BufferFormat (B2 Int32) where
    type HostFormat (B2 Int32) = (Int32, Int32)
    toBuffer = toBufferB2
    getGlType _ = gl_INT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = gl_RG_INTEGER

instance BufferFormat (B2 Int16) where
    type HostFormat (B2 Int16) = (Int16, Int16)
    toBuffer = toBufferB2
    getGlType _ = gl_SHORT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = gl_RG_INTEGER

instance BufferFormat (B2 Int8) where
    type HostFormat (B2 Int8) = (Int8, Int8)
    toBuffer = toBufferB2
    getGlType _ = gl_BYTE
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B2 Word32) where
    type HostFormat (B2 Word32) = (Word32, Word32)
    toBuffer = toBufferB2
    getGlType _ = gl_UNSIGNED_INT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = gl_RG_INTEGER

instance BufferFormat (B2 Word16) where
    type HostFormat (B2 Word16) = (Word16, Word16)
    toBuffer = toBufferB2
    getGlType _ = gl_UNSIGNED_SHORT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = gl_RG_INTEGER

instance BufferFormat (B2 Word8) where
    type HostFormat (B2 Word8) = (Word8, Word8)
    toBuffer = toBufferB2
    getGlType _ = gl_UNSIGNED_BYTE
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B2 Float) where
    type HostFormat (B2 Float) = (Float, Float)
    toBuffer = toBufferB2
    getGlType _ = gl_FLOAT
    peekPixel = peekPixel2 
    getGlPaddedFormat _ = gl_RG

instance BufferFormat (B3 Int32) where
    type HostFormat (B3 Int32) = (Int32, Int32, Int32)
    toBuffer = toBufferB3
    getGlType _ = gl_INT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = gl_RGB_INTEGER

instance BufferFormat (B3 Int16) where
    type HostFormat (B3 Int16) = (Int16, Int16, Int16)
    toBuffer = toBufferB3
    getGlType _ = gl_SHORT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B3 Int8) where
    type HostFormat (B3 Int8) = (Int8, Int8, Int8)
    toBuffer = toBufferB3
    getGlType _ = gl_BYTE
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B3 Word32) where
    type HostFormat (B3 Word32) = (Word32, Word32, Word32)
    toBuffer = toBufferB3
    getGlType _ = gl_UNSIGNED_INT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = gl_RGB_INTEGER

instance BufferFormat (B3 Word16) where
    type HostFormat (B3 Word16) = (Word16, Word16, Word16)
    toBuffer = toBufferB3
    getGlType _ = gl_UNSIGNED_SHORT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B3 Word8) where
    type HostFormat (B3 Word8) = (Word8, Word8, Word8)
    toBuffer = toBufferB3
    getGlType _ = gl_UNSIGNED_BYTE
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B3 Float) where
    type HostFormat (B3 Float) = (Float, Float, Float)
    toBuffer = toBufferB3
    getGlType _ = gl_FLOAT
    peekPixel = peekPixel3 
    getGlPaddedFormat _ = gl_RGB

instance BufferFormat (B4 Int32) where
    type HostFormat (B4 Int32) = (Int32, Int32, Int32, Int32)
    toBuffer = toBufferB4
    getGlType _ = gl_INT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B4 Int16) where
    type HostFormat (B4 Int16) = (Int16, Int16, Int16, Int16)
    toBuffer = toBufferB4
    getGlType _ = gl_SHORT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B4 Int8) where
    type HostFormat (B4 Int8) = (Int8, Int8, Int8, Int8)
    toBuffer = toBufferB4
    getGlType _ = gl_BYTE
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B4 Word32) where
    type HostFormat (B4 Word32) = (Word32, Word32, Word32, Word32)
    toBuffer = toBufferB4
    getGlType _ = gl_UNSIGNED_INT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B4 Word16) where
    type HostFormat (B4 Word16) = (Word16, Word16, Word16, Word16)
    toBuffer = toBufferB4
    getGlType _ = gl_UNSIGNED_SHORT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B4 Word8) where
    type HostFormat (B4 Word8) = (Word8, Word8, Word8, Word8)
    toBuffer = toBufferB4
    getGlType _ = gl_UNSIGNED_BYTE
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = gl_RGBA_INTEGER

instance BufferFormat (B4 Float) where
    type HostFormat (B4 Float) = (Float, Float, Float, Float)
    toBuffer = toBufferB4
    getGlType _ = gl_FLOAT
    peekPixel = peekPixel4 
    getGlPaddedFormat _ = gl_RGBA
    

    