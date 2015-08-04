{-# LANGUAGE Arrows, TypeFamilies, ScopedTypeVariables,
  FlexibleContexts, FlexibleInstances , TypeSynonymInstances #-}

module Graphics.GPipe.Internal.Buffer 
(
    BufferFormat(..),
    BufferColor,
    Buffer(),
    ToBuffer(),
    B(..), B2(..), B3(..), B4(..),
    toB3B1, toB2B2, toB1B3, toB2B1, toB1B2, toB1B1,
    BUniform(..), BNormalized(..),
    BInput(..),
    newBuffer,
    writeBuffer,
    readBuffer,
    copyBuffer,
    bufSize, bufName, bufElementSize, bufElementCount, bufBElement, makeBuffer
) where

import Graphics.GPipe.Internal.Context

import Graphics.Rendering.OpenGL.Raw.Core33
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
import Foreign.C.Types
import Data.IORef

class BufferFormat f where
    type HostFormat f
    toBuffer :: ToBuffer (HostFormat f) f
    glType :: f -> GLenum
    pixelByteSize :: f -> Int
    glType = error "This is only defined for BufferColor types"
    pixelByteSize = error "This is only defined for BufferColor types"                                 

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

data ToBuffer a b = ToBuffer
    (Kleisli (StateT Offset (Reader ToBufferInput)) a b) -- Normal = packed
    (Kleisli (StateT (Ptr ()) (ReaderT (Ptr ()) IO)) a b) -- Normal = packed
    (Kleisli (StateT Offset (Reader ToBufferInput)) a b) -- Aligned
    (Kleisli (StateT (Ptr ()) (ReaderT (Ptr ()) IO)) a b) -- Aligned

instance Category ToBuffer where
    id = ToBuffer id id id id
    ToBuffer a b c d . ToBuffer x y z w = ToBuffer (a.x) (b.y) (c.z) (d.w)
    
instance Arrow ToBuffer where
    arr f = ToBuffer (arr f) (arr f) (arr f) (arr f)
    first (ToBuffer a b c d) = ToBuffer (first a) (first b) (first c) (first d)
    
data B a = B { bName :: IORef CUInt, bOffset :: Int, bStride :: Int, bSkipElems :: Int, bInstanceDiv :: Int}

          
newtype B2 a = B2 { unB2 :: B a } -- Internal
newtype B3 a = B3 { unB3 :: B a } -- Internal
newtype B4 a = B4 { unB4 :: B a } -- Internal

toB2B2 :: forall a. Storable a => B4 a -> (B2 a, B2 a)
toB3B1 :: forall a. Storable a => B4 a -> (B3 a, B a)
toB1B3 :: forall a. Storable a => B4 a -> (B a, B3 a)
toB2B1 :: forall a. Storable a => B3 a -> (B2 a, B a)
toB1B2 :: forall a. Storable a => B3 a -> (B a, B2 a)
toB1B1 :: forall a. Storable a => B2 a -> (B a, B a)

toB2B2 (B4 b) = (B2 b, B2 $ b { bOffset = bOffset b + 2 * sizeOf (undefined :: a) }) 
toB3B1 (B4 b) = (B3 b, b { bOffset = bOffset b + 3 * sizeOf (undefined :: a) }) 
toB1B3 (B4 b) = (b, B3 $ b { bOffset = bOffset b + sizeOf (undefined :: a) }) 
toB2B1 (B3 b) = (B2 b, b { bOffset = bOffset b + 2 * sizeOf (undefined :: a) }) 
toB1B2 (B3 b) = (b, B2 $ b { bOffset = bOffset b + sizeOf (undefined :: a) }) 
toB1B1 (B2 b) = (b, b { bOffset = bOffset b + sizeOf (undefined :: a) }) 

newtype BUniform a = BUniform a
newtype BNormalized a = BNormalized a

toBufferB :: forall a. Storable a => ToBuffer a (B a)
toBufferB = ToBuffer 
                (Kleisli $ const static)
                (Kleisli writer)
                (Kleisli $ const static)
                (Kleisli writer)
           . align size
            where
                size = sizeOf (undefined :: a)
                static = do (name, stride, bIn) <- lift ask
                            offset <- get
                            put $ offset + size
                            return $ B name offset stride (bInSkipElems bIn) (bInInstanceDiv bIn)
                writer a = do ptr <- get
                              put $ ptr `plusPtr` size
                              liftIO $ poke (castPtr ptr) a
                              return undefined
toBufferB2 :: forall a. (BufferFormat (B a), a ~ HostFormat (B a), Storable a) => ToBuffer (a,a) (B2 a)
toBufferB2 = proc (a, b) -> do
        (a', _::B a) <- toBuffer . align (2 * sizeOf (undefined :: a)) -< (a, b)
        returnA -< B2 a'
toBufferB3 :: forall a. (BufferFormat (B a), a ~ HostFormat (B a), Storable a) => ToBuffer (a,a,a) (B3 a)
toBufferB3 = proc (a, b, c) -> do
        (a', _::B a, _::B a) <- toBuffer . align (4 * sizeOf (undefined :: a)) -< (a, b, c)
        returnA -< B3 a'
toBufferB4 :: forall a. (BufferFormat (B a), a ~ HostFormat (B a), Storable a) => ToBuffer (a,a,a,a) (B4 a)
toBufferB4 = proc (a, b, c, d) -> do
        (a', _::B a, _::B a, _::B a) <- toBuffer . align (4 * sizeOf (undefined :: a)) -< (a, b, c, d)
        returnA -< B4 a'

instance BufferFormat a => BufferFormat (BUniform a) where
    type HostFormat (BUniform a) = HostFormat a
    toBuffer = arr BUniform . ToBuffer 
                    elementBuilderA
                    writerA
                    elementBuilderA
                    writerA
        where
            ToBuffer _ _ elementBuilderA writerA = align (fromIntegral gl_UNIFORM_BUFFER_OFFSET_ALIGNMENT) . (toBuffer :: ToBuffer (HostFormat a) a)
    
instance BufferFormat a => BufferFormat (BNormalized a) where
    type HostFormat (BNormalized a) = HostFormat a
    toBuffer = arr BNormalized . toBuffer                                   
    glType (BNormalized a) = glType a
    pixelByteSize (BNormalized a) = pixelByteSize a
   
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
                       let buffer = makeBuffer nameRef elementCount
                       bname <- readIORef $ bufName buffer
                       glBindBuffer gl_COPY_WRITE_BUFFER (fromIntegral bname )  
                       glBufferData gl_COPY_WRITE_BUFFER (fromIntegral $ bufSize buffer) nullPtr gl_DYNAMIC_DRAW
                       return (buffer, nameRef, name)
    addContextFinalizer nameRef $ with name (glDeleteBuffers 1)
    addVAOBufferFinalizer nameRef
    return buffer 

writeBuffer :: MonadIO m => [HostFormat f] -> Int -> Buffer os f -> ContextT os f2 m ()
writeBuffer elems offset buffer = 
    let maxElems = max 0 $ bufElementCount buffer - offset
        elemSize = bufElementSize buffer
        off = fromIntegral $ offset * elemSize
        writeElem = bufWriter buffer 
        write ptr 0 _ = return ptr
        write ptr n (x:xs) = do writeElem ptr x
                                write (ptr `plusPtr` elemSize) (n-1) xs
        write ptr _ [] = return ptr
    in liftContextIOAsync $ do 
                          bname <- readIORef $ bufName buffer
                          glBindBuffer gl_COPY_WRITE_BUFFER bname
                          ptr <- glMapBufferRange gl_COPY_WRITE_BUFFER off (fromIntegral $ maxElems * elemSize) (gl_MAP_WRITE_BIT + gl_MAP_FLUSH_EXPLICIT_BIT)
                          end <- write ptr maxElems elems
                          glFlushMappedBufferRange gl_COPY_WRITE_BUFFER off (fromIntegral $ end `minusPtr` ptr) 
                          void $ glUnmapBuffer gl_COPY_WRITE_BUFFER 

readBuffer :: MonadIO m => Int -> Int -> Buffer os f -> (HostFormat f -> a -> m a) -> a ->  ContextT os f2 m a
readBuffer = undefined

copyBuffer :: MonadIO m => Int -> Int -> Buffer os f -> Int -> Buffer os f -> ContextT os f2 m ()
copyBuffer len from bFrom to bTo = liftContextIOAsync $ do 
                                                      bnamef <- readIORef $ bufName bFrom
                                                      bnamet <- readIORef $ bufName bTo
                                                      glBindBuffer gl_COPY_READ_BUFFER bnamef
                                                      glBindBuffer gl_COPY_WRITE_BUFFER bnamet
                                                      let elemSize = bufElementSize bFrom -- same as for bTo
                                                      glCopyBufferSubData gl_COPY_READ_BUFFER gl_COPY_WRITE_BUFFER (fromIntegral $ from * elemSize) (fromIntegral $ to * elemSize) (fromIntegral $ len * elemSize)   

----------------------------------------------

align :: Int -> ToBuffer a a
align x = ToBuffer (Kleisli return) (Kleisli return) (Kleisli setElemAlignM) (Kleisli setWriterAlignM) where
            setElemAlignM a = do offset <- get
                                 put $ alignedTo x offset
                                 return a   
            setWriterAlignM a = do ptr <- get
                                   basePtr <- lift ask
                                   let base = ptrToWordPtr basePtr
                                       p = ptrToWordPtr ptr
                                   put $ wordPtrToPtr $ base + alignedTo (fromIntegral x) (p - base)
                                   return a
            alignedTo a b  = b + a - 1 - ((b - 1) `mod` a)
            
makeBuffer :: forall os b. BufferFormat b => BufferName -> Int -> Buffer os b
makeBuffer name elementCount =
    let ToBuffer a b _ _ = toBuffer :: ToBuffer (HostFormat b) b
        err = error "toBuffer or toVertex are creating values that are dependant on the actual HostFormat values, this is not allowed since it doesn't allow static creation of shaders" :: HostFormat b
        elementM = runStateT (runKleisli a err) 0
        elementSize = snd $ runReader elementM (name, undefined, undefined)
        elementF bIn = fst $ runReader elementM (name, elementSize, bIn)
        writer ptr x = void $ runReaderT (runStateT (runKleisli b x) ptr) ptr
    in Buffer name elementSize elementCount elementF writer

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

instance BufferFormat (B Int32) where
    type HostFormat (B Int32) = Int32
    toBuffer = toBufferB
    glType _ = gl_INT
    pixelByteSize _ = 4

instance BufferFormat (B Int16) where
    type HostFormat (B Int16) = Int16
    toBuffer = toBufferB
    glType _ = gl_SHORT
    pixelByteSize _ = 2

instance BufferFormat (B Int8) where
    type HostFormat (B Int8) = Int8
    toBuffer = toBufferB
    glType _ = gl_BYTE
    pixelByteSize _ = 1

instance BufferFormat (B Word32) where
    type HostFormat (B Word32) = Word32
    toBuffer = toBufferB
    glType _ = gl_UNSIGNED_INT
    pixelByteSize _ = 4

instance BufferFormat (B Word16) where
    type HostFormat (B Word16) = Word16
    toBuffer = toBufferB
    glType _ = gl_UNSIGNED_SHORT
    pixelByteSize _ = 2

instance BufferFormat (B Word8) where
    type HostFormat (B Word8) = Word8
    toBuffer = toBufferB
    glType _ = gl_UNSIGNED_BYTE
    pixelByteSize _ = 1

instance BufferFormat (B Float) where
    type HostFormat (B Float) = Float
    toBuffer = toBufferB
    glType _ = gl_FLOAT
    pixelByteSize _ = 4

instance BufferFormat (B2 Int32) where
    type HostFormat (B2 Int32) = (Int32, Int32)
    toBuffer = toBufferB2
    glType _ = gl_INT
    pixelByteSize _ = 4*2

instance BufferFormat (B2 Int16) where
    type HostFormat (B2 Int16) = (Int16, Int16)
    toBuffer = toBufferB2
    glType _ = gl_SHORT
    pixelByteSize _ = 2*2

instance BufferFormat (B2 Int8) where
    type HostFormat (B2 Int8) = (Int8, Int8)
    toBuffer = toBufferB2
    glType _ = gl_BYTE
    pixelByteSize _ = 1*2

instance BufferFormat (B2 Word32) where
    type HostFormat (B2 Word32) = (Word32, Word32)
    toBuffer = toBufferB2
    glType _ = gl_UNSIGNED_INT
    pixelByteSize _ = 4*2

instance BufferFormat (B2 Word16) where
    type HostFormat (B2 Word16) = (Word16, Word16)
    toBuffer = toBufferB2
    glType _ = gl_UNSIGNED_SHORT
    pixelByteSize _ = 2*2

instance BufferFormat (B2 Word8) where
    type HostFormat (B2 Word8) = (Word8, Word8)
    toBuffer = toBufferB2
    glType _ = gl_UNSIGNED_BYTE
    pixelByteSize _ = 1*2

instance BufferFormat (B2 Float) where
    type HostFormat (B2 Float) = (Float, Float)
    toBuffer = toBufferB2
    glType _ = gl_FLOAT
    pixelByteSize _ = 4*2

instance BufferFormat (B3 Int32) where
    type HostFormat (B3 Int32) = (Int32, Int32, Int32)
    toBuffer = toBufferB3
    glType _ = gl_INT
    pixelByteSize _ = 4*3

instance BufferFormat (B3 Int16) where
    type HostFormat (B3 Int16) = (Int16, Int16, Int16)
    toBuffer = toBufferB3
    glType _ = gl_SHORT
    pixelByteSize _ = 2*3

instance BufferFormat (B3 Int8) where
    type HostFormat (B3 Int8) = (Int8, Int8, Int8)
    toBuffer = toBufferB3
    glType _ = gl_BYTE
    pixelByteSize _ = 1*3

instance BufferFormat (B3 Word32) where
    type HostFormat (B3 Word32) = (Word32, Word32, Word32)
    toBuffer = toBufferB3
    glType _ = gl_UNSIGNED_INT
    pixelByteSize _ = 4*3

instance BufferFormat (B3 Word16) where
    type HostFormat (B3 Word16) = (Word16, Word16, Word16)
    toBuffer = toBufferB3
    glType _ = gl_UNSIGNED_SHORT
    pixelByteSize _ = 2*3

instance BufferFormat (B3 Word8) where
    type HostFormat (B3 Word8) = (Word8, Word8, Word8)
    toBuffer = toBufferB3
    glType _ = gl_UNSIGNED_BYTE
    pixelByteSize _ = 1*3

instance BufferFormat (B3 Float) where
    type HostFormat (B3 Float) = (Float, Float, Float)
    toBuffer = toBufferB3
    glType _ = gl_FLOAT
    pixelByteSize _ = 4*3

instance BufferFormat (B4 Int32) where
    type HostFormat (B4 Int32) = (Int32, Int32, Int32, Int32)
    toBuffer = toBufferB4
    glType _ = gl_INT
    pixelByteSize _ = 4*4

instance BufferFormat (B4 Int16) where
    type HostFormat (B4 Int16) = (Int16, Int16, Int16, Int16)
    toBuffer = toBufferB4
    glType _ = gl_SHORT
    pixelByteSize _ = 2*4

instance BufferFormat (B4 Int8) where
    type HostFormat (B4 Int8) = (Int8, Int8, Int8, Int8)
    toBuffer = toBufferB4
    glType _ = gl_BYTE
    pixelByteSize _ = 1*4

instance BufferFormat (B4 Word32) where
    type HostFormat (B4 Word32) = (Word32, Word32, Word32, Word32)
    toBuffer = toBufferB4
    glType _ = gl_UNSIGNED_INT
    pixelByteSize _ = 4*4

instance BufferFormat (B4 Word16) where
    type HostFormat (B4 Word16) = (Word16, Word16, Word16, Word16)
    toBuffer = toBufferB4
    glType _ = gl_UNSIGNED_SHORT
    pixelByteSize _ = 2*4

instance BufferFormat (B4 Word8) where
    type HostFormat (B4 Word8) = (Word8, Word8, Word8, Word8)
    toBuffer = toBufferB4
    glType _ = gl_UNSIGNED_BYTE
    pixelByteSize _ = 1*4

instance BufferFormat (B4 Float) where
    type HostFormat (B4 Float) = (Float, Float, Float, Float)
    toBuffer = toBufferB4
    glType _ = gl_FLOAT
    pixelByteSize _ = 4*4
    

    