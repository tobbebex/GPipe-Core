{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, GADTs, DeriveDataTypeable #-}

module Graphics.GPipe.Context 
(
    ContextFactory,
    ContextFormat(..),
    ContextHandle(..),
    ContextT(),
    GPipeException(..),
    runContextT,
    runSharedContextT,
    liftContextIO,
    liftContextIOAsync,
    addContextFinalizer,
    swap,
    frameBufferSize
)
where

import Graphics.GPipe.Format
import Control.Monad.Exception (MonadException, Exception, MonadAsyncException,bracket)
import Control.Monad.Trans.Reader 
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative (Applicative)
import Data.Typeable (Typeable)
import System.Mem.Weak (addFinalizer)

type ContextFactory c ds = ContextFormat c ds -> IO ContextHandle

data ContextHandle = ContextHandle {
    newSharedContext :: forall c ds. ContextFormat c ds -> IO ContextHandle,
    contextDoSync :: forall a. IO a -> IO a,
    contextDoAsync :: IO () -> IO (),
    contextSwap :: IO (),   
    contextFrameBufferSize :: IO (Int, Int),
    contextDelete :: IO ()
} 

newtype ContextT os f m a = 
    ContextT (ReaderT ContextHandle m a) 
    deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)
    
instance MonadTrans (ContextT os f) where
    lift = ContextT . lift 


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
liftContextIO m = ContextT ask >>= liftIO . flip contextDoSync m

addContextFinalizer :: MonadIO m => key -> IO () -> ContextT os f m ()
addContextFinalizer k m = ContextT ask >>= liftIO . addFinalizer k . flip contextDoAsync m

liftContextIOAsync :: MonadIO m => IO () -> ContextT os f m ()
liftContextIOAsync m = ContextT ask >>= liftIO . flip contextDoAsync m

swap :: MonadIO m => ContextT os f m ()
swap = ContextT ask >>= (\c -> liftIO $ contextDoAsync c $ contextSwap c)

frameBufferSize :: (MonadIO m) => ContextT os f m (Int, Int)
frameBufferSize = ContextT ask >>= (\c -> liftIO $ contextDoSync c $ contextFrameBufferSize c)

data GPipeException = GPipeException String
     deriving (Show, Typeable)

instance Exception GPipeException


-- TODO Add async rules     
{-# RULES
"liftContextIO >>= liftContextIO >>= x"    forall m1 m2 x.  liftContextIO m1 >>= (\_ -> liftContextIO m2 >>= x) = liftContextIO (m1 >> m2) >>= x
"liftContextIO >>= liftContextIO"          forall m1 m2.    liftContextIO m1 >>= (\_ -> liftContextIO m2) = liftContextIO (m1 >> m2)
  #-}

