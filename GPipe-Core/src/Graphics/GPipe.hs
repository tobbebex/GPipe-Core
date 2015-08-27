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
-- A typesafe API based on the conceptual model of OpenGl, but without the imperative state machine. 
-- Aims to be as close to the raw OpenGl performance as possible, without compromising type safety or functional style. 
-- Includes DSL for shaders to provide typesafety even when crossing into that domain.
-- Uses OpenGl 3.3 under the hood.
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
