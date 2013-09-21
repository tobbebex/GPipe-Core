{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows, TypeFamilies, GeneralizedNewtypeDeriving,
  ScopedTypeVariables, FlexibleContexts, TypeSynonymInstances #-}

module Graphics.GPipe.Buffer 
(
    BufferFormat(..),
    Buffer(),
    ToBuffer(),
    B(..), B2(..), B3(..), B4(..),
    BInput(..),
    newBuffer,
    storeBuffer,
    BFloat, BInt32, BInt16, BInt8, BWord32, BWord16, BWord8, 
    BInt32Norm(), BInt16Norm(), BInt8Norm(), BWord32Norm(), BWord16Norm(), BWord8Norm(),
    bufSize, bufName, bufElementSize, bufElementCount, bufBElement, makeBuffer
) where

import Graphics.GPipe.Context

import Prelude hiding ((.), id)
import Control.Monad.Trans.State 
import Control.Category
import Control.Arrow
import Control.Monad (void, foldM_)
import Foreign.Storable
import Foreign.Marshal.Alloc (allocaBytes)
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

bufSize :: forall os b. Buffer os b -> Int
bufSize b = bufElementSize b * bufElementCount b

type BufferName = Int
type Offset = Int
type Stride = Int

data BInput = BInput {bInSkipElems :: Int, bInInstanceDiv :: Int}

type ToBufferInput = (BufferName, Stride, BInput)

data ToBuffer a b = ToBuffer
    (Kleisli (StateT Offset (Reader ToBufferInput)) a b)
    (Kleisli (StateT (Ptr ()) IO) a b)

instance Category ToBuffer where
    id = ToBuffer id id
    ToBuffer a b . ToBuffer x y = ToBuffer (a.x) (b.y)
    
instance Arrow ToBuffer where
    arr f = ToBuffer (arr f) (arr f)
    first (ToBuffer a b) = ToBuffer (first a) (first b)
    
data B a = B { bName :: Int, bOffset :: Int, bStride :: Int, bSkipElems :: Int, bInstanceDiv :: Int}

type BFloat = B Float
type BInt32 = B Int32
type BInt16 = B Int16
type BInt8 = B Int8
type BWord32 = B Word32
type BWord16 = B Word16
type BWord8 = B Word8
newtype BInt32Norm = BInt32Norm BInt32
newtype BInt16Norm = BInt16Norm BInt16
newtype BInt8Norm = BInt8Norm BInt8
newtype BWord32Norm = BWord32Norm BWord32
newtype BWord16Norm = BWord16Norm BWord16
newtype BWord8Norm = BWord8Norm BWord8

newtype B2 a = B2 a
newtype B3 a = B3 a
newtype B4 a = B4 a

instance Storable a => BufferFormat (B a) where
    type HostFormat (B a) = a
    toBuffer = ToBuffer 
                    (Kleisli $ const static)
                    (Kleisli writer)
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
                                   
instance BufferFormat BInt32Norm where
    type HostFormat BInt32Norm = Int32
    toBuffer = arr BInt32Norm . toBuffer 
instance BufferFormat BInt16Norm where
    type HostFormat BInt16Norm = Int16
    toBuffer = arr BInt16Norm . toBuffer 
instance BufferFormat BInt8Norm where
    type HostFormat BInt8Norm = Int8
    toBuffer = arr BInt8Norm . toBuffer 
instance BufferFormat BWord32Norm where
    type HostFormat BWord32Norm = Word32
    toBuffer = arr BWord32Norm . toBuffer 
instance BufferFormat BWord16Norm where
    type HostFormat BWord16Norm = Word16
    toBuffer = arr BWord16Norm . toBuffer 
instance BufferFormat BWord8Norm where
    type HostFormat BWord8Norm = Word8
    toBuffer = arr BWord8Norm . toBuffer 

instance BufferFormat (B a) => BufferFormat (B2 (B a)) where
    type HostFormat (B2 (B a)) = (a, a)
    toBuffer = proc (a, b) -> do
            (a', _::B a) <- toBuffer -< (a, b)
            returnA -< B2 a'
instance BufferFormat (B a) => BufferFormat (B3 (B a)) where
    type HostFormat (B3 (B a)) = (a, a, a)
    toBuffer = proc (a, b, c) -> do
            (a', _::B a, _::B a) <- toBuffer -< (a, b, c)
            returnA -< B3 a'
instance BufferFormat (B a) => BufferFormat (B4 (B a)) where
    type HostFormat (B4 (B a)) = (a, a, a, a)
    toBuffer = proc (a, b, c, d) -> do
            (a', _::B a, _::B a, _::B a) <- toBuffer -< (a, b, c, d)
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

 

newBuffer :: (MonadIO m, BufferFormat b) => Int -> ContextT os f m (Buffer os b)
newBuffer elementCount =
    liftContextIO $ do name <- genBufferGl
                       let buffer = makeBuffer name elementCount  
                       setBufferStorageGl name (bufSize buffer)
                       return buffer

makeBuffer :: forall os b. BufferFormat b => BufferName -> Int -> Buffer os b
makeBuffer name elementCount =
    let ToBuffer a b = toBuffer :: ToBuffer (HostFormat b) b
        elementM = runStateT (runKleisli a (undefined :: HostFormat b)) 0
        elementSize = snd $ runReader elementM (name, undefined, undefined)
        elementF bIn = fst $ runReader elementM (name, elementSize, bIn)
        writer ptr x = void $ runStateT (runKleisli b x) ptr
    in Buffer name elementSize elementCount elementF writer


storeBuffer :: MonadIO m => [HostFormat f] -> Int -> Buffer os f -> ContextT os f m ()
storeBuffer xs offset buffer = 
    let len = min (length xs) (max 0 (bufElementCount buffer - offset) ) * bufElementSize buffer
        off = offset * bufElementSize buffer
        write ptr x = do bufWriter buffer ptr x
                         return $! ptr `plusPtr` bufElementSize buffer
    in liftContextIO $ allocaBytes len $ \ ptr-> do
                            foldM_ write ptr xs
                            glStoreBufferGl (bufName buffer) ptr off len
    

                       
glStoreBufferGl :: Int -> Ptr () -> Int -> Int -> IO () 
glStoreBufferGl = undefined
                       
genBufferGl :: IO Int
genBufferGl = undefined     

setBufferStorageGl :: Int -> Int -> IO ()
setBufferStorageGl = undefined                  
    