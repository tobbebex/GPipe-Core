{-# LANGUAGE GADTs #-}

module Graphics.GPipe.Pipeline where

import Graphics.GPipe.Format
import Graphics.GPipe.VertexArray
import Graphics.GPipe.Shader
import Graphics.GPipe.VertexInput

data Pipeline os f where
     Pipeline :: FramebufferFormat f 
        => VertexArray os a
        -> Shader os (ShaderInput a) b
        -> Pipeline os f





     
runPipeLineGl :: t -> IO ()
runPipeLineGl p = undefined        