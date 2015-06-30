module Graphics.GPipe.PrimitiveArray where

import Graphics.GPipe.Buffer
import Graphics.GPipe.IndexArray
import Graphics.GPipe.VertexArray as VertexArray
import Data.Monoid

class PrimitiveTopology p where
    toGLtopology :: p -> Int
    --data Geometry p :: * -> *
    --makeGeometry :: [a] -> Geometry p a  
   
data Triangles = TriangleStrip | TriangleList
data Lines = LineStrip | LineList
data Points = PointList
--data TrianglesWithAdjacency = TriangleStripWithAdjacency
--data LinesWithAdjacency = LinesWithAdjacencyList | LinesWithAdjacencyStrip   

instance PrimitiveTopology Triangles where
    toGLtopology TriangleStrip = 0
    toGLtopology TriangleList = 1
    --data Geometry Triangles a = Triangle a a a
   
instance PrimitiveTopology Lines where
    toGLtopology LineStrip = 0
    toGLtopology LineList = 1
    --data Geometry Lines a = Line a a

instance PrimitiveTopology Points where
    toGLtopology PointList = 0
    --data Geometry Points a = Point a

{-
Some day:

instance PrimitiveTopology TrianglesWithAdjacency where
    toGLtopology TriangleStripWithAdjacency = 0
    data Geometry TrianglesWithAdjacency a = TriangleWithAdjacency a a a a a a

instance PrimitiveTopology LinesWithAdjacency where
    toGLtopology LinesWithAdjacencyList = 0
    toGLtopology LinesWithAdjacencyStrip = 1
    data Geometry LinesWithAdjacency a = LineWithAdjacency a a a a
-}

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
toPrimitiveArrayInstanced :: PrimitiveTopology p => p -> VertexArray () a -> VertexArray t b -> (a -> b -> c) -> PrimitiveArray p c
toPrimitiveArrayInstanced p va ina f = PrimitiveArray [PrimitiveArrayInstanced p (VertexArray.length ina) (VertexArray.length va) (f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput 0 1))]
toPrimitiveArrayIndexedInstanced :: PrimitiveTopology p => p -> IndexArray -> VertexArray () a -> VertexArray t b -> (a -> b -> c) -> PrimitiveArray p c
toPrimitiveArrayIndexedInstanced p ia va ina f = PrimitiveArray [PrimitiveArrayIndexedInstanced p ia (VertexArray.length ina) (f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput 0 1))]
