{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Format
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- This module provides data types for all formats of textures and frame buffers. None of the type classes in this module are intended to be instanced by anyone else. In fact, 
-- GPipe only uses these type classes through the GADT 'Format', which is closed, so any new instances wouldnt be considered anyway. 

-----------------------------------------------------------------------------

module Graphics.GPipe.Format (
    -- * Texture formats
    Format(..), 
    TextureFormat(),
    RFloat, RInt, RWord, RGFloat, RGInt, RGWord, RGBFloat, RGBInt, RGBWord, RGBAFloat, RGBAInt, RGBAWord, Depth, Stencil, DepthStencil, 
    -- * Format constraints    
    ColorSampleable(type Color, type ColorElement),
    ColorRenderable(),
    DepthRenderable(),
    StencilRenderable(),
    -- * Context formats
    ContextFormat(..),
    ContextColorFormat(),
    contextBits,
) 
where

import Graphics.GPipe.Internal.Format 