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
    getContextFinalizerAdder,
    swap,
    frameBufferSize,
    addVAOBufferFinalizer,
    addFBOTextureFinalizer,
    getContextData,
    getVAO, setVAO,
    getFBO, setFBO,
    VAOKey(..), FBOKey(..)
)
where

import Graphics.GPipe.Format
import Control.Monad.Exception (MonadException, Exception, MonadAsyncException,bracket)
import Control.Monad.Trans.Reader 
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative (Applicative)
import Data.Typeable (Typeable)
import qualified Data.Map.Strict as Map 
import System.Mem.Weak (addFinalizer)
import Graphics.Rendering.OpenGL.Raw.Core33
import Control.Concurrent.MVar
import Data.IORef
import Control.Monad
import Data.List (delete)
import Foreign.C.Types

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
    ContextT (ReaderT (ContextHandle, (ContextData, SharedContextDatas)) m a) 
    deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)
    
instance MonadTrans (ContextT os f) where
    lift = ContextT . lift 


runContextT :: (MonadIO m, MonadAsyncException m) => ContextFactory c ds -> ContextFormat c ds -> (forall os. ContextT os (ContextFormat c ds) m a) -> m a
runContextT cf f (ContextT m) = 
    bracket 
        (liftIO $ cf f)
        (liftIO . contextDelete)
        $ \ h -> do cds <- liftIO newContextDatas
                    cd <- liftIO $ addContextData cds
                    let ContextT i = initGlState
                        rs = (h, (cd, cds))
                    runReaderT (i >> m) rs

runSharedContextT :: (MonadIO m, MonadAsyncException m) => ContextFormat c ds -> ContextT os (ContextFormat c ds) (ContextT os f m) a -> ContextT os f m a
runSharedContextT f (ContextT m) =
    bracket
        (do (h',(_,cds)) <- ContextT ask
            h <- liftIO $ newSharedContext h' f
            cd <- liftIO $ addContextData cds
            return (h,cd)
        )
        (\(h,cd) -> do cds <- ContextT $ asks (snd . snd)
                       liftIO $ removeContextData cds cd
                       liftIO $ contextDelete h)
        $ \(h,cd) -> do cds <- ContextT $ asks (snd . snd)
                        let ContextT i = initGlState
                            rs = (h, (cd, cds))
                        runReaderT (i >> m) rs

initGlState :: MonadIO m => ContextT os f m ()
initGlState = liftContextIOAsync $ glEnable gl_FRAMEBUFFER_SRGB

liftContextIO :: MonadIO m => IO a -> ContextT os f m a
liftContextIO m = ContextT (asks fst) >>= liftIO . flip contextDoSync m

addContextFinalizer :: MonadIO m => IORef a -> IO () -> ContextT os f m ()
addContextFinalizer k m = ContextT (asks fst) >>= liftIO . void . mkWeakIORef k . flip contextDoAsync m

getContextFinalizerAdder  :: MonadIO m =>  ContextT os f m (IORef a -> IO () -> IO ())
getContextFinalizerAdder = do h <- ContextT (asks fst)
                              return $ \k m -> void $ mkWeakIORef k (contextDoAsync h m)  

liftContextIOAsync :: MonadIO m => IO () -> ContextT os f m ()
liftContextIOAsync m = ContextT (asks fst) >>= liftIO . flip contextDoAsync m

swap :: MonadIO m => ContextT os f m ()
swap = ContextT (asks fst) >>= (\c -> liftIO $ contextDoAsync c $ contextSwap c)

frameBufferSize :: (MonadIO m) => ContextT os f m (Int, Int)
frameBufferSize = ContextT (asks fst) >>= (\c -> liftIO $ contextDoSync c $ contextFrameBufferSize c)

data GPipeException = GPipeException String
     deriving (Show, Typeable)

instance Exception GPipeException


-- TODO Add async rules     
{-# RULES
"liftContextIO >>= liftContextIO >>= x"    forall m1 m2 x.  liftContextIO m1 >>= (\_ -> liftContextIO m2 >>= x) = liftContextIO (m1 >> m2) >>= x
"liftContextIO >>= liftContextIO"          forall m1 m2.    liftContextIO m1 >>= (\_ -> liftContextIO m2) = liftContextIO (m1 >> m2)
  #-}

--------------------------

type SharedContextDatas = MVar [ContextData]
type ContextData = MVar (VAOCache, FBOCache)
data VAOKey = VAOKey { vaoBname :: !CUInt, vaoCombBufferOffset :: !Int, vaoComponents :: !CInt, vaoNorm :: !Bool, vaoDiv :: !Int } deriving (Eq, Ord)
data FBOKey = FBOKey { fboTname :: !CUInt, fboTlayerOrNegIfRendBuff :: !Int, fboTlevel :: !Int } deriving (Eq, Ord)
type VAOCache = Map.Map [VAOKey] (IORef CUInt)
type FBOCache = Map.Map [FBOKey] (IORef CUInt)

newContextDatas = newMVar []
addContextData r = do cd <- newMVar (Map.empty, Map.empty)  
                      modifyMVar_ r $ return . (cd:)
                      return cd

removeContextData :: SharedContextDatas -> ContextData -> IO ()
removeContextData r cd = modifyMVar_ r $ return . delete cd

addCacheFinalizer :: MonadIO m => (CUInt -> (VAOCache, FBOCache) -> (VAOCache, FBOCache)) -> IORef CUInt -> ContextT os f m ()
addCacheFinalizer f r =  ContextT $ do cds <- asks (snd . snd)
                                       liftIO $ do n <- readIORef r
                                                   void $ mkWeakIORef r $ do cs' <- readMVar cds 
                                                                             mapM_ (flip modifyMVar_ (return . f n)) cs'

addVAOBufferFinalizer :: MonadIO m => IORef CUInt -> ContextT os f m ()
addVAOBufferFinalizer = addCacheFinalizer deleteVAOBuf  
    where deleteVAOBuf n (vao, fbo) = (Map.filterWithKey (\k _ -> all ((/=n) . vaoBname) k) vao, fbo)

    
addFBOTextureFinalizer :: MonadIO m => Bool -> IORef CUInt -> ContextT os f m ()
addFBOTextureFinalizer isRB = addCacheFinalizer deleteVBOBuf    
    where deleteVBOBuf n (vao, fbo) = (vao, Map.filterWithKey (\k _ -> all (\fk -> fboTname fk /= n || isRB /= (fboTlayerOrNegIfRendBuff fk < 0) ) k) fbo)


getContextData :: MonadIO m => ContextT os f m (ContextData)
getContextData = ContextT $ asks (fst . snd)

getVAO :: ContextData -> [VAOKey] -> IO (Maybe (IORef CUInt))
getVAO cd k = do (vaos, _) <- readMVar cd
                 return (Map.lookup k vaos)    

setVAO :: ContextData -> [VAOKey] -> IORef CUInt -> IO ()
setVAO cd k v = do modifyMVar_ cd $ \(vaos, fbos) -> return (Map.insert k v vaos, fbos)  

getFBO :: ContextData -> [FBOKey] -> IO (Maybe (IORef CUInt))
getFBO cd k = do (_, fbos) <- readMVar cd
                 return (Map.lookup k fbos)

setFBO :: ContextData -> [FBOKey] -> IORef CUInt -> IO ()
setFBO cd k v = modifyMVar_ cd $ \(vaos, fbos) -> return (vaos, Map.insert k v fbos)  
