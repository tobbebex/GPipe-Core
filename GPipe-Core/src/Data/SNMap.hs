{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.SNMap (
    SNMap,
    SNMapReaderT,
    runSNMapReaderT,
    newSNMap,
    memoize,    
    memoizeM    
)where

import System.Mem.StableName 
import qualified Data.HashTable as HT 
import Data.Functor
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class
import System.Mem.Weak (addFinalizer)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

newtype SNMap m a = SNMap (HT.HashTable (StableName (m a)) a)

newSNMap :: IO (SNMap m a)
newSNMap = SNMap <$> HT.new (==) (fromIntegral . hashStableName)

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

newtype SNMapReaderT a m b = SNMapReaderT (ReaderT (SNMap (SNMapReaderT a m) a) m b) deriving (Monad, MonadIO, Functor)

runSNMapReaderT :: MonadIO m => SNMapReaderT a m b -> m b
runSNMapReaderT (SNMapReaderT m) = do h <- liftIO newSNMap
                                      runReaderT m h 

instance MonadTrans (SNMapReaderT a) where
    lift = SNMapReaderT . lift

memoizeM :: MonadIO m => SNMapReaderT a m a -> SNMapReaderT a m a
memoizeM m = do h <- SNMapReaderT ask
                memoize h m
