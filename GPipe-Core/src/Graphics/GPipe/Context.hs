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
    liftContextIOAsync,
    swap,
    frameBufferSize,
    getContextState, -- TODO: Make only visible in package
)
where

import Graphics.GPipe.Format
import Graphics.GPipe.ContextState
import Control.Monad.Exception (bracket, MonadAsyncException, MonadException)
import Control.Monad.Trans.Reader 
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative (Applicative)
import Control.Monad.Trans.State.Lazy

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
    ContextT (StateT ContextState (ReaderT ContextHandle m) a) 
    deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)
    
instance MonadTrans (ContextT os f) where
    lift = ContextT . lift . lift


runContextT :: (MonadIO m, MonadAsyncException m) => ContextFactory c ds -> ContextFormat c ds -> (forall os. ContextT os (ContextFormat c ds) m a) -> m a
runContextT cf f (ContextT m) = 
    bracket 
        (liftIO $ cf f)
        (liftIO . contextDelete)
        (\c -> do s <- liftIO newContextState 
                  runReaderT (evalStateT m s) c)

runSharedContextT :: (MonadIO m, MonadAsyncException m) => ContextFormat c ds -> ContextT os (ContextFormat c ds) (ContextT os f m) a -> ContextT os f m a
runSharedContextT f (ContextT m) =   
    bracket
        (ContextT (lift ask) >>= liftIO . ($ f) . newSharedContext)
        (liftIO . contextDelete)
        (\c -> do s <- liftIO newContextState 
                  runReaderT (evalStateT m s) c)

liftContextIO :: MonadIO m => IO a -> ContextT os f m a
liftContextIO m = ContextT (lift ask) >>= liftIO . flip contextDoSync m

liftContextIOAsync :: MonadIO m => IO () -> ContextT os f m ()
liftContextIOAsync m = ContextT (lift ask) >>= liftIO . flip contextDoAsync m

swap :: MonadIO m => ContextT os f m ()
swap = ContextT (lift ask) >>= liftIO . contextSwap

frameBufferSize :: (MonadIO m) => ContextT os f m (Int, Int)
frameBufferSize = ContextT (lift ask) >>= liftIO . contextFrameBufferSize

getContextState :: Monad m => ContextT os f m ContextState
getContextState = ContextT get 


-- TODO Add async rules     
{-# RULES
"liftContextIO >>= liftContextIO >>= x"    forall m1 m2 x.  liftContextIO m1 >>= (\_ -> liftContextIO m2 >>= x) = liftContextIO (m1 >> m2) >>= x
"liftContextIO >>= liftContextIO"          forall m1 m2.    liftContextIO m1 >>= (\_ -> liftContextIO m2) = liftContextIO (m1 >> m2)
  #-}

