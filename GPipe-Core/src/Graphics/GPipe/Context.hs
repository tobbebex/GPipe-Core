{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, GADTs #-}

module Graphics.GPipe.Context 
(
    ContextFactory,
    ContextFormat(..),
    ContextHandle(..),
    ContextT(),
    runContextT,
    runSharedContextT,
    liftContextIO,
    swap,
    frameBufferSize
)
where

import Graphics.GPipe.Format
import Control.Monad.Exception (bracket, MonadAsyncException, MonadException)
import Control.Monad.Trans.Reader 
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative (Applicative)

type ContextFactory c ds = ContextFormat c ds -> IO ContextHandle

data ContextHandle = ContextHandle {
    newSharedContext :: forall c ds. ContextFormat c ds -> IO ContextHandle,
    contextDo :: forall a. IO a -> IO a,
    contextSwap :: IO (),
    contextFrameBufferSize :: IO (Int, Int),
    contextDelete :: IO ()
} 

newtype ContextT os f m a = 
    ContextT (ReaderT ContextHandle m a) 
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadException, MonadAsyncException)
    

runContextT :: (MonadIO m, MonadAsyncException m) => ContextFactory c ds -> ContextFormat c ds -> (forall os. ContextT os (ContextFormat c ds) m a) -> m a
runContextT cf f (ContextT m) = 
    bracket 
        (liftIO $ cf f)
        (liftIO . contextDelete)
        (runReaderT m)

runSharedContextT :: (MonadIO m, MonadAsyncException m) => ContextFormat c ds -> ContextT os (ContextFormat c ds) (ContextT os f m) a -> ContextT os f m a
runSharedContextT f (ContextT m) =   
    bracket
        (ContextT ask >>= liftIO . ($ f) . newSharedContext)
        (liftIO . contextDelete)
        (runReaderT m)

liftContextIO :: MonadIO m => IO a -> ContextT os f m a
liftContextIO m = ContextT ask >>= liftIO . flip contextDo m

swap :: MonadIO m => ContextT os f m ()
swap = ContextT ask >>= liftIO . contextSwap

frameBufferSize :: (MonadIO m) => ContextT os f m (Int, Int)
frameBufferSize = ContextT ask >>= liftIO . contextFrameBufferSize
     
{-# RULES
"liftContextIO >>= liftContextIO >>= x"    forall m1 m2 x.  liftContextIO m1 >>= (\_ -> liftContextIO m2 >>= x) = liftContextIO (m1 >> m2) >>= x
"liftContextIO >>= liftContextIO"          forall m1 m2.    liftContextIO m1 >>= (\_ -> liftContextIO m2) = liftContextIO (m1 >> m2)
  #-}

