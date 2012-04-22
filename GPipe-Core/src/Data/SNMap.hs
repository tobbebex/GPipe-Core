
module Data.SNMap (
    SNMap,
    SNMapReaderT,
    newSNMap,
    memoize,    
    memoizeM    
)where

import System.Mem.StableName 
import qualified Data.HashTable as HT 
import Data.Functor
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Mem.Weak (addFinalizer)
import Control.Monad.Trans.Reader (ReaderT, ask)


newtype SNMap a b = SNMap (HT.HashTable (StableName a) b)

newSNMap :: IO (SNMap a b)
newSNMap = SNMap <$> HT.new (==) (fromIntegral . hashStableName)

memoize :: MonadIO m => SNMap key val -> (key -> m val) -> key -> m val
memoize (SNMap h) f a = do s <- liftIO $ makeStableName $! a
                           x <- liftIO $ HT.lookup h s
                           case x of
                                Just b -> return b
                                Nothing -> do y <- f a
                                              liftIO $ do
                                                  HT.insert h s y
                                                  addFinalizer a (HT.delete h s)
                                              return y

type SNMapReaderT a b = ReaderT (SNMap a b)

memoizeM :: MonadIO m => (a -> SNMapReaderT a b m b) -> a -> SNMapReaderT a b m b
memoizeM f a = do h <- ask
                  memoize h f a