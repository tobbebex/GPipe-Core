-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Context
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- A Context in GPipe (just as in OpenGl) consist of two things, a window and an object space. The object space consists of Buffers, Textures and Shaders.
-- You may create a context without a window (for example for rendering to textures that are saved as pngs instead of showed), and you can create a
-- context that shares the object space with another context.
--
-- Context creation is abstracted away from GPipe, and you need a package that provides a 'ContextFactory', such as @GPipe-GLFW@.
-----------------------------------------------------------------------------

module Graphics.GPipe.Context (
    -- * Contexts
    ContextT(),
    runContextT,

    -- * Windows
    Window(),
    newWindow,
    deleteWindow,
    getFrameBufferSize,
    swapWindowBuffers,
    withContextWindow,

    -- * Extending interface
    ContextHandler(..),

    -- * Hardware exceptions
    GPipeException(..),
)
where

import Graphics.GPipe.Internal.Context
