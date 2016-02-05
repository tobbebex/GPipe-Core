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
-- GPipe aims to be as close to raw OpenGl performance as possible, without compromising type safety or functional style.
-- Includes an embedded domain specific language for GLSL shaders which provides type safety even when crossing into that domain.
-- Uses the OpenGl 3.3 core profile under the hood.
--
-- To learn GPipe, start with the <https://github.com/tobbebex/GPipe-Core#readme readme>
-- at the <https://github.com/tobbebex/GPipe-Core source repository>.
-- You could also go directly to <https://github.com/plredmond/GPipe-Test a working example>
-- or the tutorials:
-- <http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe.html    Part 1>,
-- <http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe_11.html Part 2>,
-- <http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe.html    Part 3>,
-- <http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe_21.html Part 4>,
-- <http://tobbebex.blogspot.se/2015/11/gpu-programming-in-haskell-using-gpipe.html    Part 5>.
--
-- This module reexports relevant GPipe modules and the external "Linear" and "Data.Boolean" modules.
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
    module Graphics.GPipe.Sampler,
    module Graphics.GPipe.Orphans,    
    module Linear,
    module Data.Boolean
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
import Graphics.GPipe.Orphans
import Linear
import Data.Boolean
