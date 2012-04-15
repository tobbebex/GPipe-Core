{-# LANGUAGE Arrows, TypeFamilies, EmptyDataDecls, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeSynonymInstances #-}

module Graphics.GPipe.Buffer where

import Control.Monad.Trans.State hiding (runState)
import qualified Control.Monad.Trans.State as State (runState)
import Control.Arrow.Transformer.Static
import Control.Category (Category)
import Control.Arrow
import Control.Arrow.Operations hiding (write)
import Control.Arrow.Transformer.State
import Foreign.Storable
import Foreign.Ptr
import Control.Monad.IO.Class
import Data.Word
import Data.Int
import Foreign.Marshal.Alloc (allocaBytes)
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer (lift)
import Control.Monad (void)

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
    toBuffer = ToBuffer $ wrapM $ do 
            offset <- get
            let size = sizeOf (undefined :: a)
                nextOffset = size + offset 
            put nextOffset
            return $ proc input -> do
                prevWrites <- lift fetch -< ()
                lift store -< \ ptr -> prevWrites ptr >> liftIO (pokeByteOff ptr offset input)
                bName <- readState -< ()
                returnA -< B bName offset

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



data Buffer os a = Buffer a 

type ToBufferWriter = Ptr () -> IO ()

newtype ToBuffer a b = ToBuffer (StaticMonadArrow (State Offset) (ReaderArrow BufferName (StateArrow ToBufferWriter (->))) a b) deriving (Category, Arrow)

data FromBuffer b a = FromBuffer 


bufferSize :: BufferFormat f => [f] -> Int
bufferSize list = length list * (snd . unwrapToBuffer . head) list 


createBufferIO ::forall a.  BufferFormat a => [HostFormat a] -> BufferName -> (Ptr () -> IO ()) -> IO a
createBufferIO list bName f =
    let (arrow, elementSize) = unwrapToBuffer (undefined :: a)
    in allocaBytes (elementSize * length list) $ \ ptr -> do b <- writeBufferIO arrow bName elementSize list ptr
                                                             f ptr
                                                             return b

modifyBufferIO ::forall a.  BufferFormat a => [HostFormat a] -> BufferName -> Int -> Ptr () -> IO ()
modifyBufferIO [] _ _ _ = return ()
modifyBufferIO list bName off ptr =
    let (arrow, elementSize) = unwrapToBuffer (undefined :: a)
    in void $ writeBufferIO arrow bName elementSize list $ ptr `plusPtr` (elementSize * off)
    
unwrapToBuffer :: forall f. BufferFormat f => f -> (ReaderArrow BufferName (StateArrow ToBufferWriter (->)) (HostFormat f) f, Int)
unwrapToBuffer _ =
    let ToBuffer a = toBuffer :: ToBuffer (HostFormat f) f
    in State.runState (unwrapM a) 0
    
writeBufferIO :: ReaderArrow BufferName (StateArrow ToBufferWriter (->)) e a -> BufferName -> Int -> [e] -> Ptr () -> IO a
writeBufferIO arrow bName elementSize list ptr = 
    let runArr x = runState (runReader arrow) ((x, bName), const $ return ())
    in case list of
        [x] -> let (b, m) = runArr x
                 in m ptr >> return b 
        (x:xs) -> do snd (runArr x) ptr
                     writeBufferIO arrow bName elementSize xs (ptr `plusPtr` elementSize)
        [] -> error "Empty list"
            