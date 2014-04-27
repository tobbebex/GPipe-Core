{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}
module Graphics.GPipe.Stream where

import Graphics.GPipe.Shader
import Control.Monad.Trans.State
import qualified Data.IntSet as IntSet

-- | Filter mode used in sampler state
data Filter = Nearest | Linear
            deriving (Eq,Ord,Bounded,Enum,Show)
-- | EData.IntSettsed in sampler state
data EdgeMode = Wrap | Mirror | Clamp
              deriving (Eq,Ord,Bounded,Enum,Show)
              
              
type SamplerSpec = (Int, Filter, EdgeMode)
type UniformStreamBufferBindings = [(ShaderGlobDeclM (), CompiledShader -> StateT Int IO ())] -- Going backwards, ie index 0 is last
type SamplerStreamBindings = UniformStreamBufferBindings -- Going backwards, ie index 0 is last

selectReversedIndexed :: IntSet.IntSet -> [a] -> [a]
selectReversedIndexed s xs = select (IntSet.toAscList s) (reverse xs) 0 
        where
            select (xxs@(x:xs)) (y:ys) p | x == p    = y : select xs ys (p+1)
                                         | otherwise = select xxs ys (p+1)
            select [] _ p                            = []
            select _ [] p                            = error "selectReversedIndexed: Tried selecting elements outside list"                                    


type NextUniformBlock = Int 
type NextSampler = Int 
type UsedVaryingIndices = IntSet.IntSet  
data PrimitiveStreamData = PrimitiveStreamData (InputNameToIndex -> (ShaderGlobDeclM (), IO())) UniformStreamBufferBindings SamplerStreamBindings |
                           FragmentStreamData (UsedVaryingIndices -> (ShaderGlobDeclM (), IO (ShaderSource, CompiledShader -> IO ()))) UniformStreamBufferBindings SamplerStreamBindings 
data Stream fr t a = Stream [(a, NextUniformBlock, NextSampler, PrimitiveStreamData)] 
                      -- This ^ has to be invariant to input to frame, or shader will recompile each frame! See uniforms or texture samplers

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
instance PrimitiveTopology TrianglesWithAdjacency where
    data Geometry TrianglesWithAdjacency a = TriangleWithAdjacency a a a a a a
instance PrimitiveTopology Lines where
    data Geometry Lines a = Line a a
instance PrimitiveTopology LinesWithAdjacency where
    data Geometry LinesWithAdjacency a = LineWithAdjacency a a a a
instance PrimitiveTopology Points where
    data Geometry Points a = Point a

data Geometries
data Fragments
