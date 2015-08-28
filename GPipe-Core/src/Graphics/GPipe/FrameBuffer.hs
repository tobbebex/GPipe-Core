-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.FrameBuffer
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | This module defines all functions and types for drawing into a context window 
--   or texture from a 'Graphics.GPipe.Shader.Shader'.
-----------------------------------------------------------------------------
module Graphics.GPipe.FrameBuffer (
    -- * Draw into the context window
    drawContextColor,
    drawContextDepth,
    drawContextColorDepth,
    drawContextStencil,
    drawContextColorStencil,
    drawContextDepthStencil,
    drawContextColorDepthStencil,

    -- * Draw into one or more texture images
    draw,
    drawDepth,
    drawStencil,
    drawDepthStencil,
    drawColor,
    DrawColors(),

    -- * Texture images
    Image(),
    imageEquals,
    imageSize,
    getTexture1DImage,
    getTexture1DArrayImage, 
    getTexture2DImage, 
    getTexture2DArrayImage, 
    getTexture3DImage, 
    getTextureCubeImage,    
    
    -- * Clearing the context window
    -- | Use these functions to clear the color, depth or stencil values in the context's window 
    clearContextColor,
    clearContextDepth,
    clearContextStencil, 
    clearContextDepthStencil, 

    -- * Clearing texture images
    -- | Use these functions to clear the color, depth or stencil values in texture images. 
    clearColorImage,
    clearDepthImage,
    clearStencilImage,
    clearDepthStencilImage,
        
    -- * Color drawing types
    FragColor, 
    ContextColorOption(..),
    ColorMask,
    UseBlending,
    Blending(..),
    ConstantColor,
    BlendingFactors(..),
    BlendEquation(..),
    BlendingFactor(..),
    LogicOp(..),

    -- * Depth drawing types        
    FragDepth,      
    DepthOption(..),
    DepthMask,
    DepthFunction,
    
    -- * Stencil drawing types
    StencilOptions,
    StencilOption(..),
    DepthStencilOption(..),   
    FrontBack(..),    
    StencilOp(..),
)
where

import Graphics.GPipe.Internal.Texture
import Graphics.GPipe.Internal.FrameBuffer 