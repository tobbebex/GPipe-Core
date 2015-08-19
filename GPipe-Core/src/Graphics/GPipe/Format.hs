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
-- GPipe only uses these type classes through the GADT 'Format', which is closed, so any new instances wouldnt be considered anyway. It is however possible to for instance
-- create an orphan instance for 'ColorRenderable Stencil'. Dont try this though as it will lead to an error.

-----------------------------------------------------------------------------

module Graphics.GPipe.Format (
    Format(..), 
    TextureFormat(),
    ColorSampleable(type Color, type ColorElement),
    ColorRenderable(),
    DepthRenderable(),
    StencilRenderable(),
    ContextFormat(..),
    ContextColorFormat(),
    contextBits,
    RFloat, RInt, RWord, RGFloat, RGInt, RGWord, RGBFloat, RGBInt, RGBWord, RGBAFloat, RGBAInt, RGBAWord, Depth, Stencil, DepthStencil, 
) 
where

import Graphics.GPipe.Internal.Format 