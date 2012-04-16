{-# LANGUAGE Arrows, TypeFamilies, EmptyDataDecls,
  GeneralizedNewtypeDeriving, ScopedTypeVariables,
  TypeSynonymInstances #-}

module Graphics.GPipe.Buffer where

import Prelude hiding ((.), id)
import Control.Monad.Trans.State.Strict 
import Control.Category
import Control.Arrow
import Foreign.Storable
import Foreign.Ptr
import Control.Monad.IO.Class
import Data.Word
import Data.Int
import Control.Monad (void)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

class BufferFormat f where
    type HostFormat f
    toBuffer :: ToBuffer (HostFormat f) f
    fromBuffer :: FromBuffer f (HostFormat f)

type Offset = Int
type BufferName = Int

data B a = B BufferName Offset

type BFloat = B Float
type BInt32 = B Int32
type BInt16 = B Int16
type BInt8 = B Int8
type BWord32 = B Word32
type BWord16 = B Word16
type BWord8 = B Word8

data BFloat2
data BFloat3
data BFloat4
-- TODO: Add more opaque tuples

instance Storable a => BufferFormat (B a) where
    type HostFormat (B a) = a
    toBuffer = ToBuffer 
                    (Kleisli $ const static)
                    (Kleisli writer)
                where
                    size = sizeOf (undefined :: a)
                    static = do bName <- ask
                                offset <- lift get
                                lift $ put $ offset + size
                                return $ B bName offset
                    writer a = do ptr <- get
                                  put $ ptr `plusPtr` size
                                  liftIO $ poke (castPtr ptr) a
                                  return undefined 

instance BufferFormat BFloat2 where
    type HostFormat BFloat2 = (Float, Float)
instance BufferFormat BFloat3 where
    type HostFormat BFloat3 = (Float, Float, Float)
instance BufferFormat BFloat4 where
    type HostFormat BFloat4 = (Float, Float, Float, Float)

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

data Buffer os b = Buffer {
                    bName :: !Int, 
                    bElementSize :: !Int,
                    bElementCount :: !Int,
                    bElement :: !b,
                    bWriter :: !(Ptr () -> HostFormat b -> IO ())
                    }

bSize :: forall os b. Buffer os b -> Int
bSize b = bElementSize b * bElementCount b

data ToBuffer a b = ToBuffer
    (Kleisli (ReaderT BufferName (State Offset)) a b)
    (Kleisli (StateT (Ptr ()) IO) a b)

instance Category ToBuffer where
    id = ToBuffer id id
    ToBuffer a b . ToBuffer x y = ToBuffer (a.x) (b.y)
    
instance Arrow ToBuffer where
    arr f = ToBuffer (arr f) (arr f)
    first (ToBuffer a b) = ToBuffer (first a) (first b)


data FromBuffer b a = FromBuffer 

    
makeBuffer :: forall os f. BufferFormat f => BufferName -> Int -> Buffer os f
makeBuffer name elementCount =
    let ToBuffer a b = toBuffer :: ToBuffer (HostFormat f) f
        (element, elementSize) = runState (runReaderT (runKleisli a (undefined :: HostFormat f)) name) 0
        writer ptr x = void $ runStateT (runKleisli b x) ptr
    in Buffer name elementSize elementCount element writer
