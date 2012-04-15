{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module Graphics.GPipe.Context 
(
    ContextProvider(..),
    ContextT(),
    runContextT,
    runSharedContextT,
    liftContextIO,
    liftAndSwapContextIO,
    frameBufferSize
)
where

import Graphics.GPipe.Format
import Control.Monad.Exception (bracket, MonadAsyncException, MonadException)
import Control.Monad.Trans.Reader 
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative (Applicative)


class ContextProvider p where
    inContext :: p -> IO () -> IO ()
    newContext :: (FramebufferFormat f) => f -> IO p
    newSharedContext :: (FramebufferFormat f) => f -> p -> IO p
    deleteContext :: p -> IO ()
    contextSwap :: p -> IO ()
    contextFrameBufferSize :: p -> IO (Int, Int)

newtype ContextT p f os m a = ContextT (ReaderT p m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadException, MonadAsyncException)

liftContextIO :: (ContextProvider p, MonadIO m) => IO () -> ContextT p os f m ()
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
        