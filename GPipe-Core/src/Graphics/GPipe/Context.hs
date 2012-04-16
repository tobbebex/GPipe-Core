{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module Graphics.GPipe.Context 
(
    ContextProvider(..),
    ContextT(),
    runContextT,
    runSharedContextT,
    liftContextIO,
    liftAndSwapContextIO,
    frameBufferSize,
    newBuffer,
    storeBuffer
)
where

import Graphics.GPipe.Format
import Graphics.GPipe.Buffer
import Control.Monad.Exception (bracket, MonadAsyncException, MonadException)
import Control.Monad.Trans.Reader 
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative (Applicative)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (plusPtr, Ptr)
import Control.Monad (foldM_)


class ContextProvider p where
    inContext :: p -> IO a -> IO a
    newContext :: (FramebufferFormat f) => f -> IO p
    newSharedContext :: (FramebufferFormat f) => f -> p -> IO p
    deleteContext :: p -> IO ()
    contextSwap :: p -> IO ()
    contextFrameBufferSize :: p -> IO (Int, Int)

newtype ContextT p f os m a = ContextT (ReaderT p m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadException, MonadAsyncException)

liftContextIO :: (ContextProvider p, MonadIO m) => IO a -> ContextT p os f m a
liftContextIO m = ContextT ask >>= liftIO . flip inContext m

liftAndSwapContextIO :: forall p os f m. (ContextProvider p, MonadIO m) => IO () -> ContextT p os f m ()
liftAndSwapContextIO m = liftContextIO $ m >> contextSwap (undefined :: p)

frameBufferSize :: (ContextProvider p, MonadIO m) => ContextT p os f m (Int, Int)
frameBufferSize = ContextT ask >>= liftIO . contextFrameBufferSize

runContextT :: (ContextProvider p, MonadAsyncException m, MonadIO m, FramebufferFormat f) => f -> (forall os. ContextT p f os m a) -> m a
runContextT f (ContextT m) = 
    bracket 
        (liftIO $ newContext f)
        (liftIO . deleteContext)
        (runReaderT m)   

runSharedContextT :: (ContextProvider p, MonadIO m, MonadAsyncException m, FramebufferFormat f2) => f2 -> ContextT p f2 os (ContextT p f os m) a -> ContextT p f os m a
runSharedContextT f (ContextT m) =   
    bracket
        (ContextT ask >>= liftIO . newSharedContext f)
        (liftIO . deleteContext)    
        (runReaderT m)   

     
{-# RULES
"liftContextIO >>= liftContextIO >>= x"    forall m1 m2 x.  liftContextIO m1 >>= (\_ -> liftContextIO m2 >>= x) = liftContextIO (m1 >> m2) >>= x
"liftContextIO >>= liftContextIO"          forall m1 m2.    liftContextIO m1 >>= (\_ -> liftContextIO m2) = liftContextIO (m1 >> m2)
  #-}



newBuffer :: (ContextProvider p, MonadIO m, BufferFormat a) => Int -> ContextT p os f m (Buffer os a)
newBuffer elementCount =
    liftContextIO $ do name <- genBufferGl
                       let buffer = makeBuffer name elementCount
                       setBufferStorageGl name (bSize buffer)
                       return buffer

storeBuffer :: (ContextProvider p, MonadIO m) => [HostFormat a] -> Int -> Buffer os a -> ContextT p os f m ()
storeBuffer xs offset buffer = 
    let len = min (length xs) (max 0 (bElementCount buffer - offset) ) * bElementSize buffer
        off = offset * bElementSize buffer
        write ptr x = do bWriter buffer ptr x
                         return $! ptr `plusPtr` bElementSize buffer
    in liftContextIO $ allocaBytes len $ \ ptr-> do
                            foldM_ write ptr xs
                            glStoreBufferGl (bName buffer) ptr off len
    
                       
glStoreBufferGl :: Int -> Ptr () -> Int -> Int -> IO () 
glStoreBufferGl = undefined
                       
genBufferGl :: IO Int
genBufferGl = undefined     

setBufferStorageGl :: Int -> Int -> IO ()
setBufferStorageGl = undefined                  
