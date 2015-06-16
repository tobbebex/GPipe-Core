{-# LANGUAGE ScopedTypeVariables, Arrows, BangPatterns, GeneralizedNewtypeDeriving, RankNTypes #-}
module Main where

import Data.Boolean
import Graphics.GPipe.Context
import Graphics.GPipe.Buffer
import Graphics.GPipe.Format
import Graphics.GPipe.VertexArray as VertexArray
import Graphics.GPipe.Frame
import Graphics.GPipe.FrameCompiler
import Graphics.GPipe.VertexStream
import Graphics.GPipe.FragmentStream
import Graphics.GPipe.FrameBuffer
import Graphics.GPipe.Shader
import Graphics.GPipe.Shader.Operations
import Graphics.GPipe.Uniform
import Control.Applicative ((<$>), Applicative())
import Control.Arrow (Arrow(..), Kleisli(..), returnA)
import Control.Category (Category(..))
import Prelude hiding ((.), id, length)
import System.Mem.StableName (makeStableName, hashStableName)
import Data.Monoid (Monoid(..), (<>), mconcat)
import Control.Monad.Trans.State.Lazy (State, runState, evalState, put, get, execState, StateT, runStateT, evalStateT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)


debugContext :: ContextFactory c ds 
debugContext f = return $ ContextHandle 
                (error "newSharedContext not supported")
                id
                (\_ -> return ())
                (return ())
                (return (1,1))
                (return ())
                
main :: IO ()
main = runContextT debugContext (ContextFormatColorDepth RGBA8 Depth16) (myProg)

--myProg :: ContextT os (ContextFormat RGBAFloatFormat DepthFormat) IO ()
myProg = do (myVertices1 :: Buffer os BFloat) <- newBuffer 12
            (myVertices2) <- newBuffer 45
            myUniform1 <- newBuffer 45
      
            
            liftIO $ printStableName myFrame -- TODO: Use this
            f <- compileFrame myFrame
            runFrame f (myVertices1, myVertices2, myUniform1)
          
 
            
 {--            let f1@(IntFrame (Frame _ x)) = myFrame myVertices1 myVertices2 myUniform1
            liftIO $ printStableName x           
            runFrame f1                                                
            let f2@(IntFrame (Frame _ y))  = myFrame myVertices2 myVertices1 myUniform1
            liftIO $ printStableName y
            runFrame f2                                                  
            return ()
            --}
            
myFrame = proc ~(myVertices1, myVertices2, myUniform1) -> do
                                                              myVertArray1 <- newVertexArray -< myVertices1
                                                              myVertArray11 <- newVertexArray -< myVertices1
                                                              myVertArray12 <- newVertexArray -< myVertices2
                                                              myVertArray2 <- newVertexArray -< myVertices2
                                                              p1 <- toPrimitiveStream -< toPrimitiveArray TriangleList myVertArray1 <> toPrimitiveArray TriangleList myVertArray2
                                                              p2 <- toPrimitiveStream -< toPrimitiveArrayInstanced TriangleList (VertexArray.zipWith (,) myVertArray2 myVertArray12) myVertArray11 (,)
                                                              u1 <- toUniformBlock -< (myUniform1, 10) 
                                                              u2 <- toUniformBlock -< (myUniform1, 9) 
                                                              u3 <- toUniformBlock -< (myUniform1, 8) 
                                                              u4 <- toUniformBlock -< (myUniform1, 7) 
                                                              let p1' = fmap (func u1) p1
                                                              let fragStream = rasterize Front (fmap (\x -> let y = x+u1 in ((x,cont (x+y),u1+x,u2), y+u1)) (p1 <> fmap (\((a,b),c) -> a + b + c) p2))
                                                              let fragStream2 = rasterize Front (fmap (\x -> let y = x+u1 in ((x,(x+y),u1-x,u3), y+u1)) p1')
                                                              let fragStream3 = fmap (\f -> RGBA f u4 f 1) (fragStream2 <> fragStream)
                                                              let fragStream4 = fmap (\f -> RGBA (f*2) (f+u4) f 1) (fragStream <> fragStream2)
                                                              drawContextColor -< (ColorOption NoBlending (RGBA True True True True), fragStream3)
                                                              drawContextColor -< (ColorOption NoBlending (RGBA True True True True), fragStream4)
                                                                                      
printStableName x = (makeStableName $! x) >>= print . hashStableName

func :: VFloat -> VFloat -> VFloat
func x y = (\(x,_,_) -> x) $ while (\(x,y,z) -> x <* y -z) (\(x,y,z) -> (x*2+z,y-2,z)) (x,y,x*y)

{-# NOINLINE cont #-}
cont x = contr (contr (contr x)) 
{-# NOINLINE contr #-}
contr x = x+x+x+x 

--myShader = cos
--{-# NOINLINE myShader #-}

--g v = myShader <$> toPrimitiveStream TriangleList v
 
--f v = cos <$> toPrimitiveStream TriangleList v
--{-# NOINLINE f #-}

{--
getName :: Monad m => StateT FrameState m Int
getName = do FrameState n <- get
             put $ FrameState (n+1)
             return n

addState s= do x <- get
               put $ x <> s

data FrameState = FrameState Int

getStaticFrame :: Kleisli (StateT FrameState StaticFrame) () a -> StaticFrameState
getStaticFrame (Kleisli x) = execState (evalStateT (x ()) (FrameState 0)) mempty

newtype StaticFrameState = StaticFrameState String deriving (Monoid, Show)

type DynamicFrame = IO 
type StaticFrame = State StaticFrameState

runDynF :: (Kleisli (StateT FrameState DynamicFrame) () ()) -> IO ()
runDynF (Kleisli m) = evalStateT (m ()) (FrameState 0)

data Frame a b = Frame (Kleisli (StateT FrameState DynamicFrame) a b) (Kleisli (StateT FrameState StaticFrame) a b)


instance Category Frame where
     {-# INLINE (.) #-}
     Frame x b . Frame y d = Frame (x . y)  (b . d)    
     {-# INLINE id #-}
     id = Frame id id

instance Arrow Frame where
     {-# INLINE arr #-}
     arr f = Frame (arr f) (arr f) 
     first (Frame x b) = Frame (first x) (first b) 

data VertArray a = VertArray Int
data VertStr a = VertStr [a]
data Vert a = Vert String
data Frag a = Frag String



{-# INLINE dynInStatOut #-}
dynInStatOut :: (forall m. Monad m => StateT FrameState m (x,r)) -> (x -> d -> DynamicFrame ()) -> Frame d r
dynInStatOut m md = Frame (Kleisli $ \a -> do (x,r) <- m
                                              lift $ md x a
                                              return r)
                        (Kleisli $ const $ do (x, r) <- m
                                              return r)
                                              
{-# INLINE statIn #-}
statIn :: (a -> StateT FrameState StaticFrame ()) -> Frame a ()
statIn ms = Frame (arr $ const ()) (Kleisli ms)

{-# INLINE newVertexStr #-}
newVertexStr = dynInStatOut 
                (getName >>= \n -> return (n, VertStr [Vert ("V" ++ show n)]))
                (\n (VertArray a) -> print ("dloadVert" ++ show n) >> print a)

{-# INLINE newUni #-}
newUni = dynInStatOut 
                (getName >>= \n -> return (n, Vert ("U" ++ show n)))
                (\n (VertArray a) -> print ("dloadUniform" ++ show n) >> print a)
                
mapShader :: (x->y) -> VertStr x -> VertStr y
mapShader f (VertStr a) = VertStr (map f a)

{-# INLINE draw #-}
draw :: Frame (VertStr (Vert a)) ()
draw = statIn $ \(VertStr xs) -> do lift $ addState (mconcat (map (\(Vert x) -> StaticFrameState (" draw " ++ x)) xs))                                               
                                    return ()
                                             
plus :: Vert t -> Vert t -> Vert a
plus (Vert s) (Vert t) = Vert $ "plus(" ++ s ++ "," ++ t ++")"

instance Monoid (VertStr a) where
    (VertStr a) `mappend` (VertStr b) = VertStr (a ++ b)
    mempty = VertStr []

constVert :: Int -> Vert Int
constVert x = Vert $ show x

main2 :: IO ()
main2 = do let a1 = VertArray 1
               a2 = VertArray 20
               a3 = VertArray 3
               myFrame x y = proc () -> do v1 <- newVertexStr -< y
                                           v2 <- newUni -< x
                                           v21 <- newUni -< a3
                                           _v22 <- newUni -< a3
                                           _v11 <- newVertexStr -< y
                                           v12 <- newVertexStr -< y
                                           vx <- arr (\(a,b,c) -> mapShader (plus b . plus c) a) -< (v1, v2, v21)
                                           draw -< vx <> v12 <> mapShader (plus v2 . plus v21) v1
               myFrame2    = proc () -> do v1 <- newVertexStr -< a1
                                           v2 <- newUni -< a2
                                           v21 <- newUni -< a3
                                           v12 <- newVertexStr -< a1
                                           draw -< mapShader (plus v2 . plus v21) v1
                                           draw -< v12
               anotherFrame y = proc () -> do v1 <- newVertexStr -< y
                                              v2 <- newUni -< a2
                                              draw -< mapShader (plus v2 . plus (constVert 13)) v1
               Frame  d1 e1 = myFrame a3 a3 
               Frame  d2 e2 = myFrame a2 a1
               Frame  d3 e3 = myFrame2
               Frame  d4 e4 = anotherFrame a1
               Frame  d5 e5 = anotherFrame a2
               Frame  d6 e6 = myFrame a1 a3
               Frame  d7 e7 = anotherFrame a3
               
{-           printStableName q1 
           print $ evalState (q1 ((),mempty)) (FrameState 0 mempty)
           runDynF d1
           printStableName q2 
           runDynF d2
           printStableName q3 
           print $ evalState (q3 ((),mempty)) (FrameState 0 mempty)
           --runDynF d3
           printStableName q4
           print $ evalState (q4 ((),mempty)) (FrameState 0 mempty)
           --runDynF d4
           printStableName q5
           --runDynF d5
           printStableName q6
           runDynF d6
           printStableName q7
           --runDynF d7 
           -}
           
           print "--------------------------"
           printStableName e1
           print $ getStaticFrame e1
           printStableName e2
           printStableName e3
           print $ getStaticFrame e3
           printStableName e4
           print $ getStaticFrame e4
           printStableName e5
           printStableName e6
           printStableName e7
           print "--------------------------"
           printStableName d1
           runDynF d1
           printStableName d2
           runDynF d2
           printStableName d3
           printStableName d4
           printStableName d5
           printStableName d6
           runDynF d6
           printStableName d7
           
           
 
           
printStableName x = (makeStableName $! x) >>= print . hashStableName

--}

{--
--}