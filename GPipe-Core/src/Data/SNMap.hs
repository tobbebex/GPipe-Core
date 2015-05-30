{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, RankNTypes #-}

module Data.SNMap (
    SNMap,
    SNMapReaderT,
    runSNMapReaderT,
    newSNMap,
    memoize,    
    memoizeM    
)where

import System.Mem.StableName 
import qualified Data.HashTable.IO as HT 
import Data.Functor
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class
import System.Mem.Weak (addFinalizer)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Applicative (Applicative)
import Control.Monad.Exception (MonadException, MonadAsyncException)

newtype SNMap m a = SNMap (HT.BasicHashTable (StableName (m a)) a)

newSNMap :: IO (SNMap m a)
newSNMap = SNMap <$> HT.new

memoize :: MonadIO m => SNMap m a -> m a -> m a
memoize (SNMap h) m = do s <- liftIO $ makeStableName $! m
                         x <- liftIO $ HT.lookup h s
                         case x of
                                Just a -> return a
                                Nothing -> do a <- m
                                              liftIO $ do
                                                  HT.insert h s a
                                                  addFinalizer m (HT.delete h s)
                                              return a

newtype SNMapReaderT a m b = SNMapReaderT (ReaderT (SNMap (SNMapReaderT a m) a) m b) deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)

runSNMapReaderT :: MonadIO m => SNMapReaderT a m b -> m b
runSNMapReaderT (SNMapReaderT m) = do h <- liftIO newSNMap
                                      runReaderT m h 

instance MonadTrans (SNMapReaderT a) where
    lift = SNMapReaderT . lift

memoizeM :: MonadIO m => SNMapReaderT a m a -> SNMapReaderT a m a
memoizeM m = do h <- SNMapReaderT ask
                memoize h m
