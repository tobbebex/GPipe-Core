-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Shader
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- A 'Shader' is a monad in which 'PrimitiveStream's and 'FragmentStream's live, together with samplers and uniform values. Any computations made on the streams and values in 
-- the 'Shader' monad will be performed on the GPU. A 'Shader' needs to be compiled before it can be used. In order to make it work over different environments after it
-- has been compiled, it closes over an environment value just like a 'Reader' monad, with the distinction that there is no 'ask' action, since we cannot make the
-- actual monad operation depend on the environment.
--
-- A 'Shader' is an instance of 'Alternative' and 'MonadPlus' which makes it possible to express choice with functions like 'guard'. The left most alternative will always be the
-- resulting monad.
-----------------------------------------------------------------------------

module Graphics.GPipe.Shader (
    -- * The Shader monad
    Shader(),
    compileShader,
    withoutContext,
    CompiledShader,

    -- * The Render monad
    Render(), 
    render,
        
    -- * Shader monad combinators
    guard',
    mapShader,
    maybeShader,
    chooseShader,
    silenceShader
)
where

import Graphics.GPipe.Internal.Context
import Graphics.GPipe.Internal.Shader 
