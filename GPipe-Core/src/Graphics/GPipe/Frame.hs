{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving, FlexibleInstances, ImpredicativeTypes, GADTs, EmptyDataDecls #-}

module Graphics.GPipe.Frame (
    Frame(..),
    IntFrame(),
    --runFrame,
    dynInStatOut,
    statIn,    
    getName,
    setupForName,
    doForName,
    runFrame,
    compileFrame
) where

import System.Mem.StableName
import Control.Monad.Trans.Class (lift)
import Control.Category
import Control.Arrow 
import Graphics.GPipe.Format

import Graphics.GPipe.Stream
import Graphics.GPipe.Shader
import Graphics.GPipe.Context
import Graphics.GPipe.ContextState
import Control.Applicative (Applicative)
import Control.Monad.Trans.State 
import Control.Monad.IO.Class
import Data.IntMap as Map

import Prelude hiding ((.), id)
import Control.Monad.Trans.Reader
import Data.Maybe (fromJust)
import Data.Functor.Identity (Identity)
import qualified Data.HashTable.IO as HT


type AttribNameToIndexMap = Map.IntMap Int

-- All objects doesnt use all of these. Uniform use all, vertex array use only index and texture use the last two(?)
type ProgramName = Int
type Index = Int
type Binding = Int

type NameToIOforProgAndIndex = Map.IntMap (ProgramName -> Index -> Binding -> IO ())


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
getName = do FrameState n <- get
             put $ FrameState (n+1)
             return n
             
--runDynF :: Frame os f () () -> IO ()
--runDynF (Frame (Kleisli m) _) = m ()

data IntFrame a b = Frame (Kleisli (StaticFrameT DynamicFrame) a b) (Kleisli (StaticFrameT Identity) a b) -- TODO: Add some kind of shader writer output instead of Identity

newtype Frame os f a b = IntFrame (IntFrame a b) deriving (Category, Arrow)


-- dyn in : Buffers
-- stat out: arrays, streams 

instance Category IntFrame where
     {-# INLINE (.) #-}
     Frame x b . Frame y d = Frame (x . y)  (b . d)    
     {-# INLINE id #-}
     id = Frame id id

instance Arrow IntFrame where
     {-# INLINE arr #-}
     arr f = Frame (arr f) (arr f) 
     first (Frame x b) = Frame (first x) (first b) 

data VertArray a = VertArray Int
data VertStr a = VertStr [a]
data Vert a = Vert String
data Frag a = Frag String

bindShaders (IntFrame (Frame _ s)) = do n <- makeStableName $! s
                                        putStrLn ("Load shaders with id " ++ show (hashStableName n)) --debug 

{-# INLINE dynInStatOut #-}
dynInStatOut :: (forall m. Monad m => StaticFrameT m (r, d -> DynamicFrame ())) -> IntFrame d r
dynInStatOut m =  Frame (Kleisli $ \a -> do   (r, md) <- m
                                              lift $ md a
                                              return r)
                        (Kleisli $ const $ do (r, _md) <- m
                                              return r)

                                                                                         
{-# INLINE statIn #-}
statIn :: (a -> Identity ()) -> IntFrame a ()
statIn ms = Frame (arr $ const ()) (Kleisli (lift . ms))


runFrame :: MonadIO m => Frame os f () () -> ContextT os f m ()
runFrame (IntFrame (Frame (Kleisli dyn) (Kleisli stat))) =    
    let dynMap = runDynamicFrame (runStaticFrameT (dyn ()))
    in do state <- getContextState
          compiledFrame <- getCompiledFrame (stat ()) (shaderCache state)
          runDynFrame compiledFrame dynMap

compileFrame :: MonadIO m => Frame os f () () -> ContextT os f m Bool
compileFrame (IntFrame (Frame (Kleisli dyn) (Kleisli stat))) =    
    do state <- getContextState
       (_,b) <- getCompiledFrame (stat ()) (shaderCache state)
       return b

getCompiledFrame m h = do (s, x) <- liftContextIO $ do 
                                    s <- makeStableName $! m
                                    x <- HT.lookup h s
                                    return (s, x)
                          case x of
                                Just a -> return (a, True)
                                Nothing -> do a <- compile m
                                              liftContextIO $ HT.insert h s a
                                              return (a, False)
compile m = do
    return $ CompiledFrame                                                     

runDynFrame c dynMap = return ()                                 



{-

 -- TODO: Put input in arrow
drawContextColor :: ColorRenderable c => ColorOption c -> Stream Fragments fr (FragColor c) -> Frame fr os (ContextFormat c ds) ()
drawContextColor = undefined 
drawContextColorDepth :: (ColorRenderable c, DepthRenderable ds) => ColorOption c -> DepthOption -> Stream Fragments fr (FragColor c, FragDepth) -> Frame fr os (ContextFormat c ds) ()
drawContextColorDepth = undefined 
drawContextColorStencil :: (ColorRenderable c, StencilRenderable ds) => ColorOption c -> StencilOption -> Stream Fragments fr (FragColor c) -> Frame fr os (ContextFormat c ds) ()
drawContextColorStencil = undefined
drawContextColorDepthStencil :: (ColorRenderable c, DepthRenderable ds, StencilRenderable ds) => ColorOption c -> DepthStencilOption -> Stream Fragments fr (FragColor c, FragDepth) -> Frame fr os (ContextFormat c ds) ()
drawContextColorDepthStencil = undefined
drawContextDepth :: DepthRenderable ds => DepthOption -> Stream Fragments fr FragDepth -> Frame fr os (ContextFormat c ds) ()
drawContextDepth = undefined 
drawContextStencil :: StencilRenderable ds => StencilOption -> Stream Fragments fr () -> Frame fr os (ContextFormat c ds) ()
drawContextStencil = undefined
drawContextDepthStencil :: (DepthRenderable ds, StencilRenderable ds) => DepthStencilOption -> Stream Fragments fr FragDepth -> Frame fr os (ContextFormat c ds) ()
drawContextDepthStencil = undefined
drawColors :: [forall c. ColorRenderable c => (a -> (Color c FFloat), RenderTarget os c, ColorOption c)] -> Stream Fragments fr a -> Frame fr os f ()
drawColors xs = undefined
drawColorsDepth  :: DepthRenderable ds => [forall c. ColorRenderable c => (a -> (Color c FFloat), RenderTarget os c, ColorOption c)] -> DepthStencil os ds -> DepthOption -> Stream Fragments fr (a, FragDepth) -> Frame fr os f ()
drawColorsDepth xs = undefined
drawColorsStencil  :: StencilRenderable ds => [forall c. ColorRenderable c => (a -> (Color c FFloat), RenderTarget os c, ColorOption c)] -> DepthStencil os ds -> StencilOption -> Stream Fragments fr a -> Frame fr os f ()
drawColorsStencil xs = undefined
drawColorsDepthStencil  :: (DepthRenderable ds, StencilRenderable ds) => [forall c. ColorRenderable c => (a -> (Color c FFloat), RenderTarget os c, ColorOption c)] -> DepthStencil os ds -> DepthOption -> StencilOption -> Stream Fragments fr (a, FragDepth) -> Frame fr os f ()
drawColorsDepthStencil xs = undefined
drawDepth  :: DepthRenderable ds => DepthStencil os ds -> DepthOption -> Stream Fragments fr FragDepth -> Frame fr os f ()
drawDepth = undefined
drawStencil  :: StencilRenderable ds => DepthStencil os ds -> StencilOption -> Stream Fragments fr () -> Frame fr os f ()
drawStencil = undefined
drawDepthStencil  :: (DepthRenderable ds, StencilRenderable ds) => DepthStencil os ds -> DepthOption -> StencilOption -> Stream Fragments fr FragDepth -> Frame fr os f ()
drawDepthStencil = undefined
-}
-- --.......................--


-- Private 

setupFboGl f = undefined
teardownFboGl f = undefined
clearFrameBufferGl c d s = undefined
 
    
     