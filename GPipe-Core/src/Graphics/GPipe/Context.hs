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
    swap,
    frameBufferSize
)
where

import Graphics.GPipe.Format
import Control.Monad.Exception (finally, MonadException, Exception)
import Control.Monad.Trans.Reader 
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative (Applicative)
import Data.Typeable (Typeable)

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
    deriving (Functor, Applicative, Monad, MonadIO, MonadException)
    
instance MonadTrans (ContextT os f) where
    lift = ContextT . lift 


runContextT :: (MonadIO m, MonadException m) => ContextFactory c ds -> ContextFormat c ds -> (forall os. ContextT os (ContextFormat c ds) m a) -> m a
runContextT cf f (ContextT m) = do
    c <- liftIO $ cf f
    finally
        (runReaderT m c)
        (liftIO $ contextDelete c)

runSharedContextT :: (MonadIO m, MonadException m) => ContextFormat c ds -> ContextT os (ContextFormat c ds) (ContextT os f m) a -> ContextT os f m a
runSharedContextT f (ContextT m) = do
    s' <- ContextT ask
    c <- liftIO $ ($ f) $ newSharedContext s'   
    finally
        (runReaderT m c)
        (liftIO $ contextDelete c)

liftContextIO :: MonadIO m => IO a -> ContextT os f m a
liftContextIO m = ContextT ask >>= liftIO . flip contextDoSync m

liftContextIOAsync :: MonadIO m => IO () -> ContextT os f m ()
liftContextIOAsync m = ContextT ask >>= liftIO . flip contextDoAsync m

swap :: MonadIO m => ContextT os f m ()
swap = ContextT ask >>= liftIO . contextSwap

frameBufferSize :: (MonadIO m) => ContextT os f m (Int, Int)
frameBufferSize = ContextT ask >>= liftIO . contextFrameBufferSize

data GPipeException = GPipeException String
     deriving (Show, Typeable)

instance Exception GPipeException


-- TODO Add async rules     
{-# RULES
"liftContextIO >>= liftContextIO >>= x"    forall m1 m2 x.  liftContextIO m1 >>= (\_ -> liftContextIO m2 >>= x) = liftContextIO (m1 >> m2) >>= x
"liftContextIO >>= liftContextIO"          forall m1 m2.    liftContextIO m1 >>= (\_ -> liftContextIO m2) = liftContextIO (m1 >> m2)
  #-}

