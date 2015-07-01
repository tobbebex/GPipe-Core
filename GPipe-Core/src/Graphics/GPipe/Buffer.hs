{-# LANGUAGE Arrows, TypeFamilies, ScopedTypeVariables,
  FlexibleContexts, FlexibleInstances , TypeSynonymInstances #-}

module Graphics.GPipe.Buffer 
(
    BufferFormat(..),
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
    BFloat, BInt32, BInt16, BInt8, BWord32, BWord16, BWord8, 
    BInt32Norm, BInt16Norm, BInt8Norm, BWord32Norm, BWord16Norm, BWord8Norm,
    bufSize, bufName, bufElementSize, bufElementCount, bufBElement, makeBuffer, unB2, unB3, unB4
) where

import Graphics.GPipe.Context

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

class BufferFormat f where
    type HostFormat f
    toBuffer :: ToBuffer (HostFormat f) f

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

type BufferName = Int
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
    
data B a = B { bName :: Int, bOffset :: Int, bStride :: Int, bSkipElems :: Int, bInstanceDiv :: Int}

           
type BFloat = B Float
type BInt32 = B Int32
type BInt16 = B Int16
type BInt8 = B Int8
type BWord32 = B Word32
type BWord16 = B Word16
type BWord8 = B Word8

type BInt32Norm = BNormalized BInt32
type BInt16Norm = BNormalized BInt16
type BInt8Norm = BNormalized BInt8
type BWord32Norm = BNormalized BWord32
type BWord16Norm = BNormalized BWord16
type BWord8Norm = BNormalized BWord8

newtype B2 a = B2 { unB2 :: a } -- Internal
newtype B3 a = B3 { unB3 :: a } -- Internal
newtype B4 a = B4 { unB4 :: a } -- Internal

toB2B2 :: forall a. Storable a => B4 (B a) -> (B2 (B a), B2 (B a))
toB3B1 :: forall a. Storable a => B4 (B a) -> (B3 (B a), B a)
toB1B3 :: forall a. Storable a => B4 (B a) -> (B a, B3 (B a))
toB2B1 :: forall a. Storable a => B3 (B a) -> (B2 (B a), B a)
toB1B2 :: forall a. Storable a => B3 (B a) -> (B a, B2 (B a))
toB1B1 :: forall a. Storable a => B2 (B a) -> (B a, B a)

toB2B2 (B4 b) = (B2 b, B2 $ b { bOffset = bOffset b + 2 * sizeOf (undefined :: a) }) 
toB3B1 (B4 b) = (B3 b, b { bOffset = bOffset b + 3 * sizeOf (undefined :: a) }) 
toB1B3 (B4 b) = (b, B3 $ b { bOffset = bOffset b + sizeOf (undefined :: a) }) 
toB2B1 (B3 b) = (B2 b, b { bOffset = bOffset b + 2 * sizeOf (undefined :: a) }) 
toB1B2 (B3 b) = (b, B2 $ b { bOffset = bOffset b + sizeOf (undefined :: a) }) 
toB1B1 (B2 b) = (b, b { bOffset = bOffset b + sizeOf (undefined :: a) }) 

newtype BUniform a = BUniform a
newtype BNormalized a = BNormalized a

instance Storable a => BufferFormat (B a) where
    type HostFormat (B a) = a
    toBuffer = ToBuffer 
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
                                 

instance BufferFormat a => BufferFormat (BUniform a) where
    type HostFormat (BUniform a) = HostFormat a
    toBuffer = arr BUniform . ToBuffer 
                    elementBuilderA
                    writerA
                    elementBuilderA
                    writerA
        where
            ToBuffer _ _ elementBuilderA writerA = align glUNIFORM_BUFFER_ALIGNMENT . (toBuffer :: ToBuffer (HostFormat a) a)
    
instance BufferFormat a => BufferFormat (BNormalized a) where
    type HostFormat (BNormalized a) = HostFormat a
    toBuffer = arr BNormalized . toBuffer
                                   
instance Storable a => BufferFormat (B2 (B a)) where
    type HostFormat (B2 (B a)) = (a, a)
    toBuffer = proc (a, b) -> do
            (a', _::B a) <- toBuffer . align (2 * sizeOf (undefined :: a)) -< (a, b)
            returnA -< B2 a'
instance Storable a => BufferFormat (B3 (B a)) where
    type HostFormat (B3 (B a)) = (a, a, a)
    toBuffer = proc (a, b, c) -> do
            (a', _::B a, _::B a) <- toBuffer . align (4 * sizeOf (undefined :: a)) -< (a, b, c)
            returnA -< B3 a'
instance Storable a => BufferFormat (B4 (B a)) where
    type HostFormat (B4 (B a)) = (a, a, a, a)
    toBuffer = proc (a, b, c, d) -> do
            (a', _::B a, _::B a, _::B a) <- toBuffer . align (4 * sizeOf (undefined :: a)) -< (a, b, c, d)
            returnA -< B4 a'
    
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
    (buffer, name) <- liftContextIO $ do
                       name <- genBufferGl
                       let buffer = makeBuffer name elementCount  
                       setBufferStorageGl name (bufSize buffer)
                       return (buffer, name)
    addContextFinalizer buffer $ glDeleteBuffer name
    return buffer 

writeBuffer :: MonadIO m => [HostFormat f] -> Int -> Buffer os f -> ContextT os f2 m ()
writeBuffer elems offset buffer = 
    let maxElems = max 0 $ bufElementCount buffer - offset
        elemSize = bufElementSize buffer
        off = offset * elemSize
        writeElem = bufWriter buffer 
        write ptr 0 _ = return ptr
        write ptr n (x:xs) = do writeElem ptr x
                                write (ptr `plusPtr` elemSize) (n-1) xs
        write ptr _ [] = return ptr
    in liftContextIOAsync $ do 
                          glBindBuffer glCOPY_WRITE_BUFFER (bufName buffer)
                          ptr <- glMapBufferRange glCOPY_WRITE_BUFFER off (maxElems * elemSize) (glMAP_WRITE_BIT + glMAP_FLUSH_EXPLICIT_BIT)
                          end <- write ptr maxElems elems
                          glFlushMappedBufferRange glCOPY_WRITE_BUFFER off (end `minusPtr` ptr) 
                          glUnmapBuffer glCOPY_WRITE_BUFFER 

readBufferM :: MonadIO m => Int -> Int -> Buffer os f -> (HostFormat f -> a -> m a) -> a ->  ContextT os f2 m a
readBufferM = undefined

readBuffer :: MonadIO m => Int -> Int -> Buffer os f -> (HostFormat f -> a -> a) -> a -> ContextT os f2 m a
readBuffer x y z f = readBufferM x y z (\ a b -> return $ f a b)

copyBuffer :: MonadIO m => Int -> Int -> Buffer os f -> Int -> Buffer os f -> ContextT os f2 m ()
copyBuffer len from bFrom to bTo = liftContextIOAsync $ do 
                                                      glBindBuffer glCOPY_READ_BUFFER (bufName bFrom)
                                                      glBindBuffer glCOPY_WRITE_BUFFER (bufName bTo)
                                                      let elemSize = bufElementSize bFrom -- same as for bTo
                                                      glCopyBufferSubData glCOPY_READ_BUFFER glCOPY_WRITE_BUFFER (from * elemSize) (to * elemSize) (len * elemSize)   

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
        elementM = runStateT (runKleisli a (undefined :: HostFormat b)) 0
        elementSize = snd $ runReader elementM (name, undefined, undefined)
        elementF bIn = fst $ runReader elementM (name, elementSize, bIn)
        writer ptr x = void $ runReaderT (runStateT (runKleisli b x) ptr) ptr
    in Buffer name elementSize elementCount elementF writer

    
glBindBuffer :: Int -> Int -> IO ()
glBindBuffer _ _ = return ()                                

glCOPY_READ_BUFFER :: Int
glCOPY_READ_BUFFER = 0

glCOPY_WRITE_BUFFER :: Int
glCOPY_WRITE_BUFFER = 0 

glCopyBufferSubData :: Int -> Int -> Int -> Int -> Int -> IO ()
glCopyBufferSubData _ _ _ _ _ = return ()

glStoreBufferGl :: Int -> Ptr () -> Int -> Int -> IO () 
glStoreBufferGl _ _ _ _ = return ()
                       
genBufferGl :: IO Int
genBufferGl = do putStrLn "genBuffer"
                 return 987

glDeleteBuffer :: Int -> IO ()     
glDeleteBuffer n = putStrLn $ "gldelbuffer " ++ show n

setBufferStorageGl :: Int -> Int -> IO ()
setBufferStorageGl _ s = putStrLn $ "setBufferStorageGl " ++ show s


glUNIFORM_BUFFER_ALIGNMENT :: Int
glUNIFORM_BUFFER_ALIGNMENT = 256

glMapBufferRange :: Int -> Int -> Int -> Int -> IO (Ptr()) 
glMapBufferRange _ _ _ _ = return nullPtr                        
glFlushMappedBufferRange :: Int -> Int -> Int -> IO ()
glFlushMappedBufferRange _ _ _ = return ()
glUnmapBuffer :: Int -> IO ()
glUnmapBuffer _ = return ()
glMAP_WRITE_BIT :: Int
glMAP_WRITE_BIT = 0
glMAP_FLUSH_EXPLICIT_BIT :: Int
glMAP_FLUSH_EXPLICIT_BIT = 0                          
    