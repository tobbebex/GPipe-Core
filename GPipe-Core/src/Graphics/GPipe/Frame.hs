{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving,
  FlexibleInstances, ImpredicativeTypes, GADTs #-}

module Graphics.GPipe.Frame (
    Frame(..),
    IntFrame(..),
    StaticFrame,
    dynInStatOut,
    statIn,    
    getName,
    getDrawcall,
    setupForName,
    doForName,
    runFrame,
    compileFrame
) where

import Control.Monad.Trans.Class (lift)
import Control.Category
import Control.Arrow 

import Graphics.GPipe.FrameCompiler
import Graphics.GPipe.Context
import Control.Monad.Trans.State 
import Control.Monad.IO.Class
import Data.IntMap as Map

import Prelude hiding ((.), id)
import Control.Monad.Trans.Writer.Lazy (execWriter, Writer)
import Control.Monad.Exception (throw, MonadException)


data FrameState = FrameState Int Int

type StaticFrameT m = StateT FrameState m
runStaticFrameT :: Monad m => StateT FrameState m a -> m a
runStaticFrameT m = evalStateT m (FrameState 0 1)



type StaticFrame = Writer [DrawCall] 


--type AttribNameToIndexMap = Map.IntMap Int

type DynamicFrame = State NameToIOforProgAndIndex
runDynamicFrame m = execState m Map.empty 

doForName :: Int -> (ProgramName -> Index -> Binding -> IO ()) -> DynamicFrame () 
doForName n io = modify $ alter (Just . f) n 
    where f Nothing = io
          f (Just x) = \p i b -> x p i b >> io p i b

-- Warning, setupForName is order dependent, must be run before any doForName  
setupForName :: Int -> (ProgramName -> Index -> Binding -> IO ()) -> DynamicFrame ()  
setupForName n io = modify $ alter (Just . f) n 
    where f Nothing = io
          f (Just x) = x


-- index refers to what is used in the final shader. Index space is limited, usually 16
-- attribname is what was declared, but all might not be used. Attribname share namespace with uniforms and textures and is unlimited(TM)


getName :: Monad m => StaticFrameT m Int
getName = do FrameState n d <- get
             put $ FrameState (n+1) d
             return n

getDrawcall :: Monad m => StaticFrameT m Int
getDrawcall = do FrameState n d <- get
                 put $ FrameState n (d+1)
                 return d
             
--runDynF :: Frame os f () () -> IO ()
--runDynF (Frame (Kleisli m) _) = m ()

data IntFrame a b = Frame (Kleisli (StaticFrameT DynamicFrame) a b) (Kleisli (StaticFrameT StaticFrame) a b) 

newtype Frame os f a b = IntFrame (IntFrame a b) 


instance Category (Frame os f) where
     IntFrame x . IntFrame y = IntFrame (x . y)    
     id = IntFrame id

instance Arrow (Frame os f) where
     arr f = IntFrame (arr f) 
     first (IntFrame x) = IntFrame (first x) 

-- dyn in : Buffers
-- stat out: arrays, streams 

instance Category IntFrame where
     Frame x b . Frame y d = Frame (x . y)  (b . d)    
     id = Frame id id

instance Arrow IntFrame where
     arr f = Frame (arr f) (arr f) 
     first (Frame x b) = Frame (first x) (first b) 


dynInStatOut :: (forall m. Monad m => StaticFrameT m (r, d -> DynamicFrame ())) -> IntFrame d r
dynInStatOut m =  Frame (Kleisli $ \a -> do   (r, md) <- m
                                              lift $ md a
                                              return r)
                        (Kleisli $ const $ do (r, _md) <- m
                                              return r)

                                                                                         
statIn :: (a -> StaticFrame ()) -> IntFrame a ()
statIn ms = Frame (arr $ const ()) (Kleisli (lift . ms))

data CompiledFrame os f x = CompiledFrame (x -> Either String (IO ()))

compileFrame :: (MonadIO m, MonadException m) => Frame os f x () -> ContextT os f m (CompiledFrame os f x)
compileFrame (IntFrame (Frame (Kleisli dyn) (Kleisli stat))) =
    do f <- compile . execWriter . runStaticFrameT . stat $ error "compileFrame: Frame is not fully defined! Make sure you use lazy patterns in the arrow notation ('proc ~(a,b,c,..) -> do')"
       return (CompiledFrame (f . runDynamicFrame . runStaticFrameT . dyn))


runFrame :: (MonadIO m, MonadException m) => CompiledFrame os f x -> x -> ContextT os f m ()
runFrame (CompiledFrame f) x = case f x of
                                        Right io -> liftContextIO io
                                        Left e -> throw $ GPipeException e


     