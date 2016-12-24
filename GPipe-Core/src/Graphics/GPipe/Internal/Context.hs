{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, GADTs, DeriveDataTypeable #-}

module Graphics.GPipe.Internal.Context
(
    ContextManager,
    ContextHandle(..),
    ContextT(),
    GPipeException(..),
    runContextT,
    runSharedContextT,
    liftContextIO,
    liftContextIOAsync,
    addContextFinalizer,
    getContextFinalizerAdder,
    getRenderContextFinalizerAdder ,
    swapContextBuffers,
    withContextWindow,
    addVAOBufferFinalizer,
    addFBOTextureFinalizer,
    getContextData,
    getRenderContextData,
    getVAO, setVAO,
    getFBO, setFBO,
    ContextData,
    VAOKey(..), FBOKey(..), FBOKeys(..),
    Render(..), render, getContextBuffersSize,
    registerRenderWriteTexture
)
where

import Graphics.GPipe.Internal.Format
import Control.Monad.Exception (MonadException, Exception, MonadAsyncException,bracket)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative (Applicative, (<$>))
import Data.Typeable (Typeable)
import qualified Data.IntSet as Set
import qualified Data.Map.Strict as Map
import Graphics.GL.Core33
import Graphics.GL.Types
import Control.Concurrent.MVar
import Data.IORef
import Control.Monad
import Data.List (delete)
import Foreign.C.Types
import Data.Maybe (maybeToList)
import Linear.V2 (V2(V2))
import Control.Monad.Trans.Error
import Control.Exception (throwIO)
import Control.Monad.Trans.State.Strict

type ContextManager c ds w m a = ContextFormat c ds -> (ContextHandle w -> m a) -> m a

data ContextHandle w = ContextHandle {
    -- | Like a 'ContextManager' but creates a context that shares the object space of this handle's context. Called from same thread as created the initial context.
    withSharedContext :: forall c ds m a. ContextFormat c ds -> (ContextHandle w -> m a) -> m a,
    -- | Run an OpenGL IO action in this context, returning a value to the caller.
    --   The boolean argument will be @True@ if this call references this context's window, and @False@ if it only references shared objects
    --   The thread calling this may not be the same creating the context.
    contextDoSync :: forall a. Bool ->IO a -> IO a,
    -- | Run an OpenGL IO action in this context, that doesn't return any value to the caller.
    --   The boolean argument will be @True@ if this call references this context's window, and @False@ if it only references shared objects
    --   The thread calling this may not be the same creating the context (for finalizers it is most definetly not).
    contextDoAsync :: Bool -> IO () -> IO (),
    -- | Swap the front and back buffers in the context's default frame buffer. Called from same thread as created context.
    contextSwap :: IO (),
    -- | Get the current size of the context's default framebuffer (which may change if the window is resized). Called from same thread as created context.
    contextFrameBufferSize :: IO (Int, Int),
    -- | A value representing the context's window. It is recommended that this is an opaque type that doesn't have any exported functions. Instead, provide 'ContextT' actions
    --   that are implemented in terms of 'withContextWindow' to expose any functionality to the user that need a reference the context's window.
    contextWindow :: w
}

-- | The monad transformer that encapsulates a GPipe context (which wraps an OpenGl context).
--
--   A value of type @ContextT w os f m a@ is an action on a context with these parameters:
--
--   [@w@] The type of the window that is bound to this context. It is defined by the window manager package and is probably an opaque type.
--
--   [@os@] An abstract type that is used to denote the object space. This is an forall type defined by the 'runContextT' call which will restrict any objects created inside this context
--          to be returned from it or used by another context (the same trick as the 'ST' monad uses).
--
--   [@f@] The format of the context's default frame buffer, always an instance of 'ContextFormat'.
--
--   [@m@] The monad this monad transformer wraps. Need to have 'IO' in the bottom for this 'ContextT' to be runnable.
--
--   [@a@] The value returned from this monad action.
--
newtype ContextT w os f m a =
    ContextT (ReaderT (ContextHandle w, (ContextData, SharedContextDatas)) m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)

instance MonadTrans (ContextT w os f) where
    lift = ContextT . lift

-- | Run a 'ContextT' monad transformer, creating a window (unless the 'ContextFormat' is 'ContextFormatNone') that is later destroyed when the action returns. This function will
--   also create a new object space.
--   You need a 'ContextManager', which is provided by an auxillary package, such as @GPipe-GLFW@.
runContextT :: (MonadIO m, MonadAsyncException m) => ContextManager c ds w m a -> ContextFormat c ds -> (forall os. ContextT w os (ContextFormat c ds) m a) -> m a
runContextT withContext f (ContextT m) =
    withContext f
        $ \ h -> do cds <- liftIO newContextDatas
                    cd <- liftIO $ addContextData cds
                    let ContextT i = initGlState
                        rs = (h, (cd, cds))
                    runReaderT (i >> m) rs

-- | Run a 'ContextT' monad transformer inside another one, creating a window (unless the 'ContextFormat' is 'ContextFormatNone') that is later destroyed when the action returns. The inner 'ContextT' monad
-- transformer will share object space with the outer one. The 'ContextManager' of the outer context will be used in the creation of the inner context.
runSharedContextT :: (MonadIO m, MonadAsyncException m) => ContextFormat c ds -> ContextT w os (ContextFormat c ds) (ContextT w os f m) a -> ContextT w os f m a
runSharedContextT f (ContextT m) = do
    h' <- ContextT $ asks fst
    withSharedContext h' f $ \h -> do
      bracket
        (do cds <- ContextT $ asks (snd . snd)
            cd <- liftIO $ addContextData cds
            return (h,cd)
        )
        (\(h,cd) -> do cds <- ContextT $ asks (snd . snd)
                       liftIO $ do removeContextData cds cd
                       )
        $ \(h,cd) -> do cds <- ContextT $ asks (snd . snd)
                        let ContextT i = initGlState
                            rs = (h, (cd, cds))
                        runReaderT (i >> m) rs

initGlState :: MonadIO m => ContextT w os f m ()
initGlState = liftContextIOAsyncInWin $ do glEnable GL_FRAMEBUFFER_SRGB
                                           glEnable GL_SCISSOR_TEST
                                           glPixelStorei GL_PACK_ALIGNMENT 1
                                           glPixelStorei GL_UNPACK_ALIGNMENT 1

liftContextIO :: MonadIO m => IO a -> ContextT w os f m a
liftContextIO m = do h <- ContextT (asks fst)
                     liftIO $ contextDoSync h False m

addContextFinalizer :: MonadIO m => IORef a -> IO () -> ContextT w os f m ()
addContextFinalizer k m = do h <- ContextT (asks fst)
                             liftIO $ void $ mkWeakIORef k $ contextDoAsync h False m

-- | This is only used to finalize nonShared objects such as VBOs and FBOs
getContextFinalizerAdder  :: MonadIO m =>  ContextT w os f m (IORef a -> IO () -> IO ())
getContextFinalizerAdder = do h <- ContextT (asks fst)
                              return $ \k m -> void $ mkWeakIORef k $ contextDoAsync h True m

liftContextIOAsync :: MonadIO m => IO () -> ContextT w os f m ()
liftContextIOAsync m = do h <- ContextT (asks fst)
                          liftIO $ contextDoAsync h False m

liftContextIOAsyncInWin :: MonadIO m => IO () -> ContextT w os f m ()
liftContextIOAsyncInWin m = do h <- ContextT (asks fst)
                               liftIO $ contextDoAsync h True m

-- | Run this action after a 'render' call to swap out the context windows back buffer with the front buffer, effectively showing the result.
--   This call may block if vsync is enabled in the system and/or too many frames are outstanding.
--   After this call, the context window content is undefined and should be cleared at earliest convenience using 'clearContextColor' and friends.
swapContextBuffers :: MonadIO m => ContextT w os f m ()
swapContextBuffers = ContextT (asks fst) >>= (liftIO . contextSwap)

type ContextDoAsync = Bool -> IO () -> IO ()

-- | A monad in which shaders are run.
newtype Render os f a = Render (ErrorT String (StateT Set.IntSet (ReaderT (ContextDoAsync, (ContextData, SharedContextDatas)) IO)) a) deriving (Monad, Applicative, Functor)

-- | Run a 'Render' monad, that may have the effect of the context window or textures being drawn to.
--
--   May throw a 'GPipeException' if a combination of draw images (FBO) used by this render call is unsupported by the graphics driver
render :: (MonadIO m, MonadException m) => Render os f () -> ContextT w os f m ()
render (Render m) = do c <- ContextT ask
                       eError <- liftIO $ contextDoSync (fst c) True $ runReaderT (evalStateT (runErrorT m) Set.empty) (contextDoAsync (fst c), snd c)
                       case eError of
                        Left s -> liftIO $ throwIO $ GPipeException s
                        _ -> return ()

registerRenderWriteTexture :: Int -> Render os f ()
registerRenderWriteTexture x = Render $ lift $ modify $ Set.insert x

-- | Return the current size of the context frame buffer. This is needed to set viewport size and to get the aspect ratio to calculate projection matrices.
getContextBuffersSize :: MonadIO m => ContextT w os f m (V2 Int)
getContextBuffersSize = ContextT $ do c <- asks fst
                                      (x,y) <- liftIO $ contextFrameBufferSize c
                                      return $ V2 x y

-- | Use the context window handle, which type is specific to the window system used. This handle shouldn't be returned from this function
withContextWindow :: MonadIO m => (w -> IO a) -> ContextT w os f m a
withContextWindow f= ContextT $ do c <- asks fst
                                   liftIO $ f (contextWindow c)

-- | This is only used to finalize nonShared objects such as VBOs and FBOs
getRenderContextFinalizerAdder  :: Render os f (IORef a -> IO () -> IO ())
getRenderContextFinalizerAdder = do f <- Render (lift $ lift $ asks fst)
                                    return $ \k m -> void $ mkWeakIORef k (f True m)

-- | This kind of exception may be thrown from GPipe when a GPU hardware limit is reached (for instance, too many textures are drawn to from the same 'FragmentStream')
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
data VAOKey = VAOKey { vaoBname :: !GLuint, vaoCombBufferOffset :: !Int, vaoComponents :: !GLint, vaoNorm :: !Bool, vaoDiv :: !Int } deriving (Eq, Ord)
data FBOKey = FBOKey { fboTname :: !GLuint, fboTlayerOrNegIfRendBuff :: !Int, fboTlevel :: !Int } deriving (Eq, Ord)
data FBOKeys = FBOKeys { fboColors :: [FBOKey], fboDepth :: Maybe FBOKey, fboStencil :: Maybe FBOKey } deriving (Eq, Ord)
type VAOCache = Map.Map [VAOKey] (IORef GLuint)
type FBOCache = Map.Map FBOKeys (IORef GLuint)

getFBOKeys :: FBOKeys -> [FBOKey]
getFBOKeys (FBOKeys xs d s) = xs ++ maybeToList d ++ maybeToList s

newContextDatas :: IO (MVar [ContextData])
newContextDatas = newMVar []

addContextData :: SharedContextDatas -> IO ContextData
addContextData r = do cd <- newMVar (Map.empty, Map.empty)
                      modifyMVar_ r $ return . (cd:)
                      return cd

removeContextData :: SharedContextDatas -> ContextData -> IO ()
removeContextData r cd = modifyMVar_ r $ return . delete cd

addCacheFinalizer :: MonadIO m => (GLuint -> (VAOCache, FBOCache) -> (VAOCache, FBOCache)) -> IORef GLuint -> ContextT w os f m ()
addCacheFinalizer f r =  ContextT $ do cds <- asks (snd . snd)
                                       liftIO $ do n <- readIORef r
                                                   void $ mkWeakIORef r $ do cs' <- readMVar cds
                                                                             mapM_ (`modifyMVar_` (return . f n)) cs'

addVAOBufferFinalizer :: MonadIO m => IORef GLuint -> ContextT w os f m ()
addVAOBufferFinalizer = addCacheFinalizer deleteVAOBuf
    where deleteVAOBuf n (vao, fbo) = (Map.filterWithKey (\k _ -> all ((/=n) . vaoBname) k) vao, fbo)


addFBOTextureFinalizer :: MonadIO m => Bool -> IORef GLuint -> ContextT w os f m ()
addFBOTextureFinalizer isRB = addCacheFinalizer deleteVBOBuf
    where deleteVBOBuf n (vao, fbo) = (vao, Map.filterWithKey
                                          (\ k _ ->
                                             all
                                               (\ fk ->
                                                  fboTname fk /= n || isRB /= (fboTlayerOrNegIfRendBuff fk < 0))
                                               $ getFBOKeys k)
                                          fbo)


getContextData :: MonadIO m => ContextT w os f m ContextData
getContextData = ContextT $ asks (fst . snd)

getRenderContextData :: Render os f ContextData
getRenderContextData = Render $ lift $ lift $ asks (fst . snd)

getVAO :: ContextData -> [VAOKey] -> IO (Maybe (IORef GLuint))
getVAO cd k = do (vaos, _) <- readMVar cd
                 return (Map.lookup k vaos)

setVAO :: ContextData -> [VAOKey] -> IORef GLuint -> IO ()
setVAO cd k v = modifyMVar_ cd $ \ (vaos, fbos) -> return (Map.insert k v vaos, fbos)

getFBO :: ContextData -> FBOKeys -> IO (Maybe (IORef GLuint))
getFBO cd k = do (_, fbos) <- readMVar cd
                 return (Map.lookup k fbos)

setFBO :: ContextData -> FBOKeys -> IORef GLuint -> IO ()
setFBO cd k v = modifyMVar_ cd $ \(vaos, fbos) -> return (vaos, Map.insert k v fbos)
