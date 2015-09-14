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
    withoutContext,
    mapShader,
    guard',
    maybeShader,
    chooseShader,
    silenceShader,
    throwFromMaybe
) where


import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.Context
import Graphics.GPipe.Internal.Buffer
import Control.Monad.Trans.State 
import qualified Control.Monad.Trans.State.Strict as StrictState 
import Control.Monad.IO.Class
import qualified Data.IntSet as Set 
import Control.Monad.Trans.Writer.Lazy (tell, WriterT(..), runWriterT)
import Control.Monad.Exception (MonadException)
import Control.Applicative (Applicative, Alternative, (<|>))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Monad (MonadPlus, when)
import Control.Monad.Trans.List (ListT(..))
import Data.Monoid (All(..), mempty)
import Data.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error (throwError)

data ShaderState s = ShaderState Int (RenderIOState s)

newShaderState :: ShaderState s
newShaderState = ShaderState 0 newRenderIOState

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
mapDrawcall f (Drawcall a b c d e g h i j k) = Drawcall (a . f) b c d e g h i j k 

newtype ShaderM s a = ShaderM (ReaderT UniformAlignment (WriterT ([IO (Drawcall s)], s -> All) (ListT (State (ShaderState s)))) a) deriving (MonadPlus, Monad, Alternative, Applicative, Functor)

-- | The monad in which all GPU computations are done. 'Shader os f s a' lives in an object space 'os' and a context with format 'f', closing over an environent of type 's'.  
newtype Shader os f s a = Shader (ShaderM s a)  deriving (MonadPlus, Monad, Alternative, Applicative, Functor)

-- | Map the environment to a different environment and run a Shader in that sub environment, returning it's result. 
mapShader :: (s -> s') -> Shader os f s' a -> Shader os f s a
mapShader f (Shader (ShaderM m)) = Shader $ ShaderM $ do
        uniAl <- ask
        lift $ WriterT $ ListT $ do
            ShaderState x s <- get
            let (adcs, ShaderState x' s') = runState (runListT (runWriterT (runReaderT m uniAl))) (ShaderState x newRenderIOState)
            put $ ShaderState x' (mapRenderIOState f s' s) 
            return $ map (\(a,(dcs, disc)) -> (a, (map (>>= (return . mapDrawcall f)) dcs, disc . f))) adcs

-- | Conditionally run the effects of a shader when a 'Maybe' value is 'Just' something.   
maybeShader :: (s -> Maybe s') -> Shader os f s' () -> Shader os f s ()
maybeShader f m = (guard' (isJust . f) >> mapShader (fromJust . f) m) <|> guard' (isNothing . f) 

-- | Like 'guard', but dependent on the 'Shaders' environment value. Since this will be evaluated at shader run time, as opposed to shader compile time for 'guard',
--   using this to do recursion will make 'compileShader' diverge. You can break that divergence by combining it with a normal 'guard' and a maximum loop count.
guard' :: (s -> Bool) -> Shader os f s ()
guard' f = Shader $ ShaderM $ lift $ tell (mempty, All . f) 

-- | Select one of two 'Shader' actions based on whether an 'Either' value is 'Left' or 'Right'.
chooseShader :: (s -> Either s' s'') -> Shader os f s' a -> Shader os f s'' a -> Shader os f s a
chooseShader f a b = (guard' (isLeft . f) >> mapShader (fromLeft . f) a) <|> (guard' (isRight . f) >> mapShader (fromRight . f) b) 
    where fromLeft (Left x) = x
          fromRight (Right x) = x        

-- | Discard all effects of a 'Shader' action (i.e., dont draw anything) and just return the resulting value. This makes it possible to use a 'Shader' written for a different context format. 
silenceShader :: Shader os f' s a -> Shader os f s a
silenceShader (Shader (ShaderM m)) = Shader $ ShaderM $ do
        uniAl <- ask
        lift $ WriterT $ ListT $ do
            s <- get
            let (adcs, s') = runState (runListT (runWriterT (runReaderT m uniAl))) s
            put s'
            return $ map (\ (a, (_, disc)) -> (a, ([], disc))) adcs

-- | A compiled shader is just a function that takes an environment and returns a 'Render' action 
type CompiledShader os f s = s -> Render os f ()

-- | Compiles a shader into a 'CompiledShader'. This action will usually take a second or more, so put it during a loading sequence or something.   
--
--   May throw a 'GPipeException' if the graphics driver doesn't support something in this shader (e.g. too many interpolated floats sent between a vertex and a fragment shader),
--   or if shader evaluates to 'mzero'.
compileShader :: (MonadIO m, MonadException m) => Shader os f x () -> ContextT w os f' m (CompiledShader os f x)
compileShader (Shader (ShaderM m)) = do
        uniAl <- liftContextIO getUniformAlignment
        let (adcs, ShaderState _ s) = runState (runListT (runWriterT (runReaderT m uniAl))) newShaderState
            f ((disc, runF):ys) e@(cd, env, asserter) = if getAll (disc env) then runF cd env asserter else f ys e
            f  [] _               = error "render: Shader evaluated to mzero\n"
        xs <- mapM (\(_,(dcs, disc)) -> do 
                                runF <- compile dcs s
                                return (disc, runF)) adcs
        return $ \ s -> Render $ do cd <- lift $ lift $ asks $ fst . snd
                                    texmap <- lift StrictState.get
                                    let asserter x = when (Set.member x texmap) $ error "render: Running shader that samples from texture that currently has an image borrowed from it. Try run this shader from a separate render call where no images from the same texture are drawn to or cleared." 
                                    throwFromMaybe $ lift $ lift $ lift $ f xs (cd, s, asserter)

throwFromMaybe m = do mErr <- m
                      case mErr of
                          Just err -> throwError err
                          Nothing -> return ()
                         

-- | Use this to run a shader that doesn't reference the context frame buffer, allowing the same shader to be run in another context with a different context format (but still with same object space).
withoutContext :: Render os () () -> Render os f ()
withoutContext (Render m) = Render m