{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving,
  FlexibleInstances, ImpredicativeTypes, GADTs #-}

module Graphics.GPipe.Frame (
    Frame(..),
    FrameM(..),
    FrameState(..),
    Render(..),
    getName,
    getDrawcall,
    tellDrawcall,
    modifyRenderIO,
    render,
    runFrame,
    compileFrame,
    mapFrame,
    silenceFrame
) where


import Graphics.GPipe.FrameCompiler
import Graphics.GPipe.Context
import Control.Monad.Trans.State 
import Control.Monad.IO.Class

import Control.Monad.Trans.Writer.Lazy (Writer, runWriter, tell)
import Control.Monad.Exception (MonadException)
import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (lift)
import Control.Monad (liftM)


data FrameState s = FrameState Int Int (RenderIOState s)

newFrameState :: FrameState s
newFrameState = FrameState 0 1 newRenderIOState

getName :: FrameM s Int
getName = do FrameState n d r <- FrameM get
             FrameM $ put $ FrameState (n+1) d r
             return n

getDrawcall :: FrameM s Int
getDrawcall = do FrameState n d r <- FrameM get
                 FrameM $ put $ FrameState n (d+1) r
                 return d

modifyRenderIO :: (RenderIOState s -> RenderIOState s) -> FrameM s ()
modifyRenderIO f = FrameM $ modify (\(FrameState a b s) -> FrameState a b (f s))

tellDrawcall :: forall s. IO Drawcall -> FrameM s ()
tellDrawcall dc = FrameM $ lift $ tell [dc] 
             
newtype FrameM s a = FrameM (StateT (FrameState s) (Writer [IO Drawcall]) a) deriving (Monad, Applicative, Functor)

newtype Frame os f s a = Frame (FrameM s a)  deriving (Monad, Applicative, Functor)

mapFrame :: (s -> s') -> Frame os f s' a -> Frame os f s a
mapFrame f (Frame (FrameM m)) = Frame $ FrameM $   
    do FrameState x y s <- get
       let ((a,FrameState x' y' s'), dcs) = runWriter $ runStateT m (FrameState x y newRenderIOState)
       put $ FrameState x' y' (mapRenderIOState f s' s)
       lift $ tell dcs
       return a

silenceFrame :: Frame os f' s a -> Frame os f s a
silenceFrame (Frame (FrameM m)) = Frame $ FrameM $   
    do s <- get
       let ((a,s'), _) = runWriter $ runStateT m s
       put s'
       return a

compileFrame :: (MonadIO m, MonadException m) => Frame os f x () -> ContextT os f m (CompiledFrame os f x)
compileFrame (Frame (FrameM m)) =
    let (((),FrameState _ _ s), dcs) = runWriter $ runStateT m newFrameState 
    in compile dcs s

newtype Render os f a = Render (IO a) deriving (Monad, Applicative, Functor)

render :: (MonadIO m, MonadException m) => Render os f () -> ContextT os f m ()
render (Render m) = liftContextIOAsync m

runFrame :: CompiledFrame os f x -> x -> Render os f ()
runFrame (CompiledFrame f) x = Render $ do
                                   putStrLn "-------------------------------------------------------------------------------------------"
                                   putStrLn "-------------------------------------------------------------------------------------------"
                                   putStrLn "Running frame"
                                   f x
                                   putStrLn "-------------------------------------------------------------------------------------------"
                                   putStrLn "-------------------------------------------------------------------------------------------"



     