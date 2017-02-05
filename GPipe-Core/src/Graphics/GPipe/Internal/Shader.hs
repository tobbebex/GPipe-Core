{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving,
  FlexibleInstances, GADTs #-}

module Graphics.GPipe.Internal.Shader (
    Shader(..),
    ShaderM(..),
    ShaderState(..),
    CompiledShader,
    Render(..),
    getName,
    tellDrawcall,
    askUniformAlignment,
    modifyRenderIO,
    compileShader,
    mapShader,
    guard',
    maybeShader,
    chooseShader,
    silenceShader
) where


import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.Context
import Graphics.GPipe.Internal.Buffer
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.State.Strict as StrictState
import Control.Monad.IO.Class
import qualified Data.IntSet as Set
import Control.Monad.Trans.Writer.Lazy (tell, WriterT(..), execWriterT)
import Control.Monad.Exception (MonadException)
import Control.Applicative (Applicative, Alternative, (<|>))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Monad (MonadPlus, when)
import Control.Monad.Trans.List (ListT(..))
import Data.Monoid (All(..), mempty)
import Data.Either
import Control.Monad.Trans.Reader
import Data.List (find)

data ShaderState s = ShaderState Int (RenderIOState s)

newShaderState :: ShaderState s
newShaderState = ShaderState 1 newRenderIOState

getName :: ShaderM s Int
getName = do ShaderState n r <- ShaderM $ lift $ lift $ lift get
             ShaderM $ lift $ lift $ lift $ put $ ShaderState (n+1) r
             return n

askUniformAlignment = ShaderM ask

modifyRenderIO :: (RenderIOState s -> RenderIOState s) -> ShaderM s ()
modifyRenderIO f = ShaderM $ lift $ lift $ lift $ modify (\(ShaderState a s) -> ShaderState a (f s))

tellDrawcall :: IO (Drawcall s) -> ShaderM s ()
tellDrawcall dc = ShaderM $ lift $ tell ([dc], mempty)

mapDrawcall :: (s -> s') -> Drawcall s' -> Drawcall s
mapDrawcall f (Drawcall a b c d e g h i j k m) = Drawcall (a . f) b c d e g h i j k m

newtype ShaderM s a = ShaderM (ReaderT UniformAlignment (WriterT ([IO (Drawcall s)], s -> All) (ListT (State (ShaderState s)))) a) deriving (MonadPlus, Monad, Alternative, Applicative, Functor)

-- | The monad in which all GPU computations are done. 'Shader os s a' lives in an object space 'os' and a context with format 'f', closing over an environent of type 's'.
newtype Shader os s a = Shader (ShaderM s a)  deriving (MonadPlus, Monad, Alternative, Applicative, Functor)

-- | Map the environment to a different environment and run a Shader in that sub environment, returning it's result.
mapShader :: (s -> s') -> Shader os s' a -> Shader os s a
mapShader f (Shader (ShaderM m)) = Shader $ ShaderM $ do
        uniAl <- ask
        lift $ WriterT $ ListT $ do
            ShaderState x s <- get
            let (adcs, ShaderState x' s') = runState (runListT (runWriterT (runReaderT m uniAl))) (ShaderState x newRenderIOState)
            put $ ShaderState x' (mapRenderIOState f s' s)
            return $ map (\(a,(dcs, disc)) -> (a, (map (>>= (return . mapDrawcall f)) dcs, disc . f))) adcs

-- | Conditionally run the effects of a shader when a 'Maybe' value is 'Just' something.
maybeShader :: (s -> Maybe s') -> Shader os s' () -> Shader os s ()
maybeShader f m = (guard' (isJust . f) >> mapShader (fromJust . f) m) <|> guard' (isNothing . f)

-- | Like 'guard', but dependent on the 'Shaders' environment value. Since this will be evaluated at shader run time, as opposed to shader compile time for 'guard',
--   using this to do recursion will make 'compileShader' diverge. You can break that divergence by combining it with a normal 'guard' and a maximum loop count.
guard' :: (s -> Bool) -> Shader os s ()
guard' f = Shader $ ShaderM $ lift $ tell (mempty, All . f)

-- | Select one of two 'Shader' actions based on whether an 'Either' value is 'Left' or 'Right'.
chooseShader :: (s -> Either s' s'') -> Shader os s' a -> Shader os s'' a -> Shader os s a
chooseShader f a b = (guard' (isLeft . f) >> mapShader (fromLeft . f) a) <|> (guard' (isRight . f) >> mapShader (fromRight . f) b)
    where fromLeft (Left x) = x
          fromRight (Right x) = x

-- | Discard all effects of a 'Shader' action (i.e., dont draw anything) and just return the resulting value.
silenceShader :: Shader os s a -> Shader os s a
silenceShader (Shader (ShaderM m)) = Shader $ ShaderM $ do
        uniAl <- ask
        lift $ WriterT $ ListT $ do
            s <- get
            let (adcs, s') = runState (runListT (runWriterT (runReaderT m uniAl))) s
            put s'
            return $ map (\ (a, (_, disc)) -> (a, ([], disc))) adcs

-- | A compiled shader is just a function that takes an environment and returns a 'Render' action
type CompiledShader os s = s -> Render os ()

-- | Compiles a shader into a 'CompiledShader'. This action will usually take a second or more, so put it during a loading sequence or something.
--
--   May throw a 'GPipeException' if the graphics driver doesn't support something in this shader (e.g. too many interpolated floats sent between a vertex and a fragment shader)
compileShader :: (ContextHandler ctx, MonadIO m, MonadException m) => Shader os x () -> ContextT ctx os m (CompiledShader os x)
compileShader (Shader (ShaderM m)) = do
        uniAl <- liftNonWinContextIO getUniformAlignment
        let (adcs, ShaderState _ st) = runState (runListT (execWriterT (runReaderT m uniAl))) newShaderState
        xs <- mapM (\(dcs, disc) -> do rf <- compile dcs st
                                       return (rf, disc)) adcs
        return $ \ s -> case find (\(_,disc) -> getAll (disc s)) xs of
                          Nothing -> error "render: Shader evaluated to mzero"
                          Just (rf,_) -> rf s
