{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving #-}

module Graphics.GPipe.VertexStream where

import Control.Monad.Trans.Class
import Graphics.GPipe.Frame (DynamicFrame)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Prelude hiding (length, id, (.))
import Graphics.GPipe.Buffer
import Graphics.GPipe.Shader
import Graphics.GPipe.Frame
import Graphics.GPipe.FrameCompiler
import Graphics.GPipe.VertexArray hiding (length)
import qualified Graphics.GPipe.VertexArray as VertexArray  (length)
import Graphics.GPipe.IndexArray hiding (length)
import qualified Graphics.GPipe.IndexArray as IndexArray (length)
import Control.Category
import Control.Arrow
import Data.Foldable (forM_)
import Data.Monoid (Monoid(..))

--------------------

class PrimitiveTopology p where
    data Geometry p :: * -> *
    toGLtopology :: p -> Int
    makeGeometry :: [a] -> Geometry p a  
   
data Triangles = TriangleStrip | TriangleList
data TrianglesWithAdjacency = TriangleStripWithAdjacency
data Lines = LineStrip | LineList
data LinesWithAdjacency = LinesWithAdjacencyList | LinesWithAdjacencyStrip   
data Points = PointList

instance PrimitiveTopology Triangles where
    data Geometry Triangles a = Triangle a a a
    toGLtopology TriangleStrip = 0
    toGLtopology TriangleList = 1
   
instance PrimitiveTopology TrianglesWithAdjacency where
    data Geometry TrianglesWithAdjacency a = TriangleWithAdjacency a a a a a a
    toGLtopology TriangleStripWithAdjacency = 0

instance PrimitiveTopology Lines where
    data Geometry Lines a = Line a a
    toGLtopology LineStrip = 0
    toGLtopology LineList = 1

instance PrimitiveTopology LinesWithAdjacency where
    data Geometry LinesWithAdjacency a = LineWithAdjacency a a a a
    toGLtopology LinesWithAdjacencyList = 0
    toGLtopology LinesWithAdjacencyStrip = 1

instance PrimitiveTopology Points where
    data Geometry Points a = Point a
    toGLtopology PointList = 0

type InstanceCount = Int

data PrimitiveArrayInt p a = PrimitiveArraySimple p Int a 
                           | PrimitiveArrayIndexed p IndexArray a 
                           | PrimitiveArrayInstanced p InstanceCount Int a 
                           | PrimitiveArrayIndexedInstanced p IndexArray InstanceCount a 

newtype PrimitiveArray p a = PrimitiveArray [PrimitiveArrayInt p a]

instance Monoid (PrimitiveArray p a) where
    mempty = PrimitiveArray []
    mappend (PrimitiveArray a) (PrimitiveArray b) = PrimitiveArray (a ++ b)

instance Functor (PrimitiveArray p) where
    fmap f (PrimitiveArray xs) = PrimitiveArray  $ fmap g xs
        where g (PrimitiveArraySimple p l a) = PrimitiveArraySimple p l (f a)
              g (PrimitiveArrayIndexed p i a) = PrimitiveArrayIndexed p i (f a)
              g (PrimitiveArrayInstanced p il l a) = PrimitiveArrayInstanced p il l (f a)
              g (PrimitiveArrayIndexedInstanced p i il a) = PrimitiveArrayIndexedInstanced p i il (f a)
              
toPrimitiveArray :: PrimitiveTopology p => p -> VertexArray () a -> PrimitiveArray p a
toPrimitiveArray p va = PrimitiveArray [PrimitiveArraySimple p (VertexArray.length va) (bArrBFunc va (BInput 0 0))]
toPrimitiveArrayIndexed :: PrimitiveTopology p => p -> IndexArray -> VertexArray () a -> PrimitiveArray p a
toPrimitiveArrayIndexed p ia va = PrimitiveArray [PrimitiveArrayIndexed p ia (bArrBFunc va (BInput 0 0))]
toPrimitiveArrayInstanced :: PrimitiveTopology p => p -> VertexArray () a -> VertexArray Instances b -> (a -> b -> c) -> PrimitiveArray p c
toPrimitiveArrayInstanced p va ina f = PrimitiveArray [PrimitiveArrayInstanced p (VertexArray.length ina) (VertexArray.length va) (f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput 0 1))]
toPrimitiveArrayIndexedInstanced :: PrimitiveTopology p => p -> IndexArray -> VertexArray () a -> VertexArray Instances b -> (a -> b -> c) -> PrimitiveArray p c
toPrimitiveArrayIndexedInstanced p ia va ina f = PrimitiveArray [PrimitiveArrayIndexedInstanced p ia (VertexArray.length ina) (f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput 0 1))]

--------------------
type DrawCallName = Int
data VertexStreamData = VertexStreamData DrawCallName

newtype VertexStream t a = VertexStream [(a, VertexStreamData)] deriving Monoid

instance Functor (VertexStream t) where
        fmap f (VertexStream xs) = VertexStream $ map (first f) xs

class BufferFormat a => VertexInput a where
    type VertexFormat a
    toVertex :: ToVertex a (VertexFormat a)  

instance Monoid (DynamicFrame ()) where
    mempty = return ()
    mappend a b = a >> b

newtype ToVertex a b = ToVertex (Kleisli (StateT Int (Writer (DynamicFrame ()))) a b) deriving (Category, Arrow)

   
instance VertexInput BFloat where
    type VertexFormat BFloat = VFloat
    toVertex = ToVertex $ Kleisli $ \b -> do n <- get
                                             put $ (n+1)
                                             lift $ tell (doForName n $ \ix -> glBindBuffer (bName b) glVERTEX_ARRAY  >> glAttribArray ix 1 glFLOAT False (bName b) (bStride b) (bStride b * bSkipElems b + bOffset b))
                                             return (S $ useVInput STypeFloat n)
instance VertexInput BInt32Norm where
    type VertexFormat BInt32Norm = VFloat
    toVertex = ToVertex $ Kleisli $ \(BNormalized b) -> do 
                                             n <- get
                                             put $ (n+1)
                                             lift $ tell (doForName n $ \ix -> glBindBuffer (bName b) glVERTEX_ARRAY  >> glAttribArray ix 1 glINT32 False (bName b) (bStride b) (bStride b * bSkipElems b + bOffset b))
                                             return (S $ useVInput STypeFloat n)

                                                               
toPrimitiveStream :: forall os f a p. (VertexInput a, PrimitiveTopology p) => Frame os f (PrimitiveArray p a) (VertexStream p (VertexFormat a))   
toPrimitiveStream = IntFrame $ dynInStatOut $ do 
                                      FrameState n d <- get -- n is streamname, aka shadername when paired with drawcall
                                      let sampleBuffer = makeBuffer undefined undefined :: Buffer os a
                                          b = bufBElement sampleBuffer $ BInput 0 0
                                          ((x,n'), _) = runWriter (runStateT (f b) (n+1)) 
                                      put $ FrameState (n') d
                                      return (VertexStream [(x, VertexStreamData n)], \ (PrimitiveArray xs) -> mapM_ (drawcall n) xs) 
    where 
        ToVertex (Kleisli f) = toVertex :: ToVertex a (VertexFormat a)
        drawcall n (PrimitiveArraySimple p l a) = do snd $ runWriter (runStateT (f a) (n+1))
                                                     doForName n $ \_ -> glDrawArrays (toGLtopology p) 0 l
        drawcall n (PrimitiveArrayIndexed p i a) = do
                                              snd $ runWriter (runStateT (f a) (n+1))
                                              doForName n $ \_ -> do 
                                                                      forM_ (restart i) glRestartIndex  
                                                                      glBindBuffer (iArrName i) glELEMENT_ARRAY
                                                                      glDrawElements (toGLtopology p) (IndexArray.length i) (indexType i) (offset i)
        drawcall n (PrimitiveArrayInstanced p il l a) = do
                                              snd $ runWriter (runStateT (f a) (n+1))
                                              doForName n $ \_ -> glDrawArraysInstanced (toGLtopology p) 0 l il
        drawcall n (PrimitiveArrayIndexedInstanced p i il a) = do
                                              snd $ runWriter (runStateT (f a) (n+1))
                                              doForName n $ \_ -> do
                                                      forM_ (restart i) glRestartIndex 
                                                      glBindBuffer (iArrName i) glELEMENT_ARRAY
                                                      glDrawElementsInstanced (toGLtopology p) (IndexArray.length i) (indexType i) (offset i) il
    

glDrawArrays :: Int -> Int -> Int -> IO ()
glDrawArrays a b c = putStrLn $ "glDrawArrays " ++ show (a,b,c)

glDrawArraysInstanced :: Int -> Int -> Int -> Int -> IO ()
glDrawArraysInstanced a b c d = putStrLn $ "glDrawArraysInstanced " ++ show (a,b,c,d)

glDrawElements :: Int -> Int -> Int -> Int -> IO ()
glDrawElements a b c d = putStrLn $ "glDrawElements " ++ show (a,b,c,d)

glDrawElementsInstanced :: Int -> Int -> Int -> Int -> Int -> IO ()
glDrawElementsInstanced a b c d e = putStrLn $ "glDrawElementsInstanced " ++ show (a,b,c,d,e)
 
glRestartIndex :: Int -> IO ()
glRestartIndex a = putStrLn $ "glRestartIndex " ++ show a

glBindBuffer :: Int -> Int -> IO ()
glBindBuffer a b = putStrLn $ "glBindBuffer " ++ show (a,b)                                

glAttribArray :: Int -> Int -> Int -> Bool -> Int -> Int -> Int -> IO () 
glAttribArray a b c d e f g = putStrLn $ "glAttribArray " ++ show (a,b,c,d,e,f,g)                              

glINT32 :: Int                                      
glINT32 = 0

glFLOAT :: Int
glFLOAT = 1

glVERTEX_ARRAY :: Int
glVERTEX_ARRAY = 1

glELEMENT_ARRAY :: Int
glELEMENT_ARRAY = 1
