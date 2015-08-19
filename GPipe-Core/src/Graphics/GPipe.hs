-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- GPipe models the entire OpenGl 3.3 core profile graphics pipeline in a functional typesafe way. It supports typesafe mutable buffers and textures, 
-- customized VBO specification including instancing and multiple attribute buffers, DSL to write typesafe shader code in haskell, FBOs with multiple render targets, 
-- integral texture types, and more!
--
-- This is a convenience module, combining GPipes all other modules.
-----------------------------------------------------------------------------

module Graphics.GPipe (
    module Graphics.GPipe.Context,
    module Graphics.GPipe.Format,
    module Graphics.GPipe.Buffer,
    module Graphics.GPipe.PrimitiveArray,
    module Graphics.GPipe.PrimitiveStream,
    module Graphics.GPipe.FragmentStream,
    module Graphics.GPipe.FrameBuffer,
    module Graphics.GPipe.Shader,
    module Graphics.GPipe.Expr,
    module Graphics.GPipe.Uniform,
    module Graphics.GPipe.Texture,
    module Graphics.GPipe.Sampler
)
where

import Graphics.GPipe.Context
import Graphics.GPipe.Format
import Graphics.GPipe.Buffer
import Graphics.GPipe.PrimitiveArray
import Graphics.GPipe.PrimitiveStream
import Graphics.GPipe.FragmentStream
import Graphics.GPipe.FrameBuffer
import Graphics.GPipe.Shader
import Graphics.GPipe.Expr
import Graphics.GPipe.Uniform
import Graphics.GPipe.Texture
import Graphics.GPipe.Sampler
