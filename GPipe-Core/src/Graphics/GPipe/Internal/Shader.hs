{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving,
  FlexibleInstances, ImpredicativeTypes, GADTs #-}

module Graphics.GPipe.Internal.Shader (
    Shader(..),
    ShaderM(..),
    ShaderState(..),
    CompiledShader,
    Render(..),
    getName,
    tellDrawcall,
    modifyRenderIO,
    render,
    compileShader,
    mapShader,
    guard',
    maybeShader,
    chooseShader,
    silenceShader
) where


import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.Context
import Control.Monad.Trans.State 
import Control.Monad.IO.Class

import Control.Monad.Trans.Writer.Lazy (tell, WriterT(..), runWriterT)
import Control.Monad.Exception (MonadException)
import Control.Applicative (Applicative, Alternative, (<|>))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Monad (MonadPlus)
import Control.Monad.Trans.List (ListT(..))
import Data.Monoid (All(..), mempty)
import Data.Either

data ShaderState s = ShaderState Int (RenderIOState s)

newShaderState :: ShaderState s
newShaderState = ShaderState 0 newRenderIOState

getName :: ShaderM s Int
getName = do ShaderState n r <- ShaderM $ lift $ lift get
             ShaderM $ lift $ lift $ put $ ShaderState (n+1) r
             return n

modifyRenderIO :: (RenderIOState s -> RenderIOState s) -> ShaderM s ()
modifyRenderIO f = ShaderM $ lift $ lift $ modify (\(ShaderState a s) -> ShaderState a (f s))

tellDrawcall :: IO (Drawcall s) -> ShaderM s ()
tellDrawcall dc = ShaderM $ tell ([dc], mempty) 

mapDrawcall :: (s -> s') -> Drawcall s' -> Drawcall s
mapDrawcall f (Drawcall a b c d e g h i) = Drawcall (a . f) b c d e g h i 
           
newtype ShaderM s a = ShaderM (WriterT ([IO (Drawcall s)], s -> All) (ListT (State (ShaderState s))) a) deriving (MonadPlus, Monad, Alternative, Applicative, Functor)

newtype Shader os f s a = Shader (ShaderM s a)  deriving (MonadPlus, Monad, Alternative, Applicative, Functor)

mapShader :: (s -> s') -> Shader os f s' a -> Shader os f s a
mapShader f (Shader (ShaderM m)) = Shader $ ShaderM $ WriterT $ ListT $ do
        ShaderState x s <- get      
        let (adcs, ShaderState x' s') = runState (runListT (runWriterT m)) (ShaderState x newRenderIOState)
        put $ ShaderState x' (mapRenderIOState f s' s) 
        return $ map (\(a,(dcs, disc)) -> (a, (map (>>= (return . mapDrawcall f)) dcs, disc . f))) adcs

maybeShader :: (s -> Maybe s') -> Shader os f s' () -> Shader os f s ()
maybeShader f m = (guard' (isJust . f) >> mapShader (fromJust . f) m) <|> guard' (isNothing . f) 

guard' :: (s -> Bool) -> Shader os f s ()
guard' f = Shader $ ShaderM $ tell (mempty, All . f) 

chooseShader :: (s -> Either s' s'') -> Shader os f s' a -> Shader os f s'' a -> Shader os f s a
chooseShader f a b = (guard' (isLeft . f) >> mapShader (fromLeft . f) a) <|> (guard' (isRight . f) >> mapShader (fromRight . f) b) 
    where fromLeft (Left x) = x
          fromRight (Right x) = x        

silenceShader :: Shader os f' s a -> Shader os f s a
silenceShader (Shader (ShaderM m)) = Shader $ ShaderM $ WriterT $ ListT $ do
        s <- get
        let (adcs, s') = runState (runListT (runWriterT m)) s
        put s'
        return $ map (\ (a, (_, disc)) -> (a, ([], disc))) adcs

type CompiledShader os f s = s -> Render os f ()

compileShader :: (MonadIO m, MonadException m) => Shader os f x () -> ContextT os f m (CompiledShader os f x)
compileShader (Shader (ShaderM m)) =
    let (adcs, ShaderState _ s) = runState (runListT (runWriterT m)) newShaderState
        f ((disc, runF):ys) e = if getAll (disc e) then runF e else f ys e
        f  [] _               = return ()
    in do xs <- mapM (\(_,(dcs, disc)) -> do 
                                runF <- compile dcs s
                                return (disc, runF)) adcs
          return $ Render . f xs     

newtype Render os f a = Render (IO a) deriving (Monad, Applicative, Functor)

render :: (MonadIO m, MonadException m) => Render os f () -> ContextT os f m ()
render (Render m) = liftContextIOAsync m
