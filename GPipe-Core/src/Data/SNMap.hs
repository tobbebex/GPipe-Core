{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, RankNTypes #-}

module Data.SNMap (
    SNMap,
    SNMapReaderT,
    runSNMapReaderT,
    newSNMap,
    memoize,    
    memoizeM,
    scopedM
)where

import System.Mem.StableName 
import qualified Data.HashTable.IO as HT 
import Data.Functor
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class
import System.Mem.Weak (addFinalizer)
import Control.Applicative (Applicative)
import Control.Monad.Exception (MonadException, MonadAsyncException)
import Control.Monad.Trans.State.Strict

newtype SNMap m a = SNMap (HT.BasicHashTable (StableName (m a)) a)

newSNMap :: IO (SNMap m a)
newSNMap = SNMap <$> HT.new

memoize :: MonadIO m => m (SNMap m a) -> m a -> m a
memoize getter m = do s <- liftIO $ makeStableName $! m
                      (SNMap h) <- getter 
                      x <- liftIO $ HT.lookup h s
                      case x of
                                Just a -> return a
                                Nothing -> do a <- m
                                              (SNMap h') <- getter --Need to redo because of scope 
                                              liftIO $ HT.insert h' s a
                                              return a

newtype SNMapReaderT a m b = SNMapReaderT (StateT (SNMap (SNMapReaderT a m) a) m b) deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)

runSNMapReaderT :: MonadIO m => SNMapReaderT a m b -> m b
runSNMapReaderT (SNMapReaderT m) = do h <- liftIO newSNMap
                                      evalStateT m h 

instance MonadTrans (SNMapReaderT a) where
    lift = SNMapReaderT . lift

memoizeM :: MonadIO m => SNMapReaderT a m a -> SNMapReaderT a m a
memoizeM = memoize (SNMapReaderT get)

-- | Run a subcomputation in a scope, where nothing memoized inside will be remembered after
scopedM :: MonadIO m => SNMapReaderT a m x -> SNMapReaderT a m x
scopedM m= do SNMap h <- SNMapReaderT get
              save <- liftIO $ HT.toList h
              x <- m
              h' <- liftIO $ HT.fromList save
              SNMapReaderT $ put (SNMap h')
              return x
