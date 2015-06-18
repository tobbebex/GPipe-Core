{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving #-}

module Graphics.GPipe.VertexStream where

import Control.Monad.Trans.Class
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
import Data.IntMap.Lazy (insert)

--------------------

class PrimitiveTopology p where
    toGLtopology :: p -> Int
    --data Geometry p :: * -> *
    --makeGeometry :: [a] -> Geometry p a  
   
data Triangles = TriangleStrip | TriangleList
data TrianglesWithAdjacency = TriangleStripWithAdjacency
data Lines = LineStrip | LineList
data LinesWithAdjacency = LinesWithAdjacencyList | LinesWithAdjacencyStrip   
data Points = PointList

instance PrimitiveTopology Triangles where
    toGLtopology TriangleStrip = 0
    toGLtopology TriangleList = 1
    --data Geometry Triangles a = Triangle a a a
   
instance PrimitiveTopology TrianglesWithAdjacency where
    toGLtopology TriangleStripWithAdjacency = 0
    --data Geometry TrianglesWithAdjacency a = TriangleWithAdjacency a a a a a a

instance PrimitiveTopology Lines where
    toGLtopology LineStrip = 0
    toGLtopology LineList = 1
    --data Geometry Lines a = Line a a

instance PrimitiveTopology LinesWithAdjacency where
    toGLtopology LinesWithAdjacencyList = 0
    toGLtopology LinesWithAdjacencyStrip = 1
    --data Geometry LinesWithAdjacency a = LineWithAdjacency a a a a

instance PrimitiveTopology Points where
    toGLtopology PointList = 0
    --data Geometry Points a = Point a

type InstanceCount = Int

data PrimitiveArrayInt p a = PrimitiveArraySimple p Int a 
                           | PrimitiveArrayIndexed p IndexArray a 
                           | PrimitiveArrayInstanced p InstanceCount Int a 
                           | PrimitiveArrayIndexedInstanced p IndexArray InstanceCount a 

newtype PrimitiveArray p a = PrimitiveArray {getPrimitiveArray :: [PrimitiveArrayInt p a]}

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
data PrimitiveStreamData = PrimitiveStreamData DrawCallName

newtype PrimitiveStream t a = PrimitiveStream [(a, PrimitiveStreamData)] deriving Monoid

instance Functor (PrimitiveStream t) where
        fmap f (PrimitiveStream xs) = PrimitiveStream $ map (first f) xs

class BufferFormat a => VertexInput a where
    type VertexFormat a
    toVertex :: ToVertex a (VertexFormat a)  

newtype ToVertex a b = ToVertex (Kleisli (StateT Int (Writer [Binding -> IO ()])) a b) deriving (Category, Arrow)

   
instance VertexInput BFloat where
    type VertexFormat BFloat = VFloat
    toVertex = ToVertex $ Kleisli $ \b -> do n <- get
                                             put $ n + 1
                                             lift $ tell [\ix -> glBindBuffer (bName b) glVERTEX_ARRAY  >> glAttribArray ix 1 glFLOAT False (bName b) (bStride b) (bStride b * bSkipElems b + bOffset b)]
                                             return (S $ useVInput STypeFloat n)
instance VertexInput BInt32Norm where
    type VertexFormat BInt32Norm = VFloat
    toVertex = ToVertex $ Kleisli $ \(BNormalized b) -> do 
                                             n <- get
                                             put $ n + 1
                                             lift $ tell [\ix -> glBindBuffer (bName b) glVERTEX_ARRAY  >> glAttribArray ix 1 glINT32 False (bName b) (bStride b) (bStride b * bSkipElems b + bOffset b)]
                                             return (S $ useVInput STypeFloat n)
instance (VertexInput a, VertexInput b) => VertexInput (a,b) where
    type VertexFormat (a,b) = (VertexFormat a, VertexFormat b)
    toVertex = proc (a,b) -> do a' <- toVertex -< a
                                b' <- toVertex -< b
                                returnA -< (a', b')
                                                               
toPrimitiveStream :: forall os f s a p. (VertexInput a, PrimitiveTopology p) => (s -> PrimitiveArray p a) -> Frame os f s (PrimitiveStream p (VertexFormat a))   
toPrimitiveStream sf = Frame $ do n <- getName
                                  let sampleBuffer = makeBuffer undefined undefined :: Buffer os a
                                      x = fst $ runWriter (evalStateT (mf $ bufBElement sampleBuffer $ BInput 0 0) 0) 
                                  doForInputArray n (map drawcall . getPrimitiveArray . sf)
                                  return $ PrimitiveStream [(x, PrimitiveStreamData n)] 
    where 
        ToVertex (Kleisli mf) = toVertex :: ToVertex a (VertexFormat a)
        drawcall (PrimitiveArraySimple p l a) binds = do runAttribs  a binds
                                                         glDrawArrays (toGLtopology p) 0 l
        drawcall (PrimitiveArrayIndexed p i a) binds = do 
                                                    runAttribs a binds
                                                    bindIndexBuffer i
                                                    glDrawElements (toGLtopology p) (IndexArray.length i) (indexType i) (offset i)
        drawcall (PrimitiveArrayInstanced p il l a) binds = do
                                              runAttribs a binds
                                              glDrawArraysInstanced (toGLtopology p) 0 l il
        drawcall (PrimitiveArrayIndexedInstanced p i il a) binds = do
                                                      runAttribs a binds
                                                      bindIndexBuffer i
                                                      glDrawElementsInstanced (toGLtopology p) (IndexArray.length i) (indexType i) (offset i) il
        bindIndexBuffer i = do forM_ (restart i) glRestartIndex 
                               glBindBuffer (iArrName i) glELEMENT_ARRAY                                                      

        assignIxs :: Int -> Int -> [Int] -> [Int -> IO ()] -> [IO ()]
        assignIxs n ix xxs@(x:xs) (f:fs) | x == n    = f ix : assignIxs (n+1) (ix+1) xs fs
                                         | otherwise = assignIxs (n+1) (ix+1) xxs fs
        assignIxs _ _ _ [] = []                                          
        assignIxs _ _ _ _ = error "Too few attributes generated in toPrimitiveStream"
        
        runAttribs a binds = sequence_ $ assignIxs 0 0 binds $ snd $ runWriter (runStateT (mf a) 0)

        doForInputArray :: Int -> (s -> [[Binding] -> IO()]) -> FrameM s ()
        doForInputArray n io = modifyRenderIO (\s -> s { inputArrayToRenderIOs = insert n io (inputArrayToRenderIOs s) } )


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
