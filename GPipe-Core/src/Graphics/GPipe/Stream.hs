{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}
module Graphics.GPipe.Stream where

import Graphics.GPipe.Shader
import Control.Monad.Trans.State

type UniformStreamBufferBindings = [(ShaderGlobDeclM (), CompiledShader -> StateT Int IO ())] -- Going backwards, ie index 0 is last 
type NextUniformBlock = Int 
data PrimitiveStreamData = PrimitiveStreamData (InputNameToIndex -> (ShaderGlobDeclM (), IO())) UniformStreamBufferBindings
data Stream fr t a = Stream [(a, NextUniformBlock, PrimitiveStreamData)] 


class PrimitiveTopology p where
    data Primitive p
    toGLtopology :: p -> Int

data Triangle = TriangleStrip | TriangleList
data TrianglesWithAdjacency = TriangleStripWithAdjacency
data Lines = LineStrip | LineList
data LinesWithAdjacency = LinesWithAdjacency  
data Points = Points


data Fragment
