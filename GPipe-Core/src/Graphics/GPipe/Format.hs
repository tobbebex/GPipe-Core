{-# LANGUAGE TypeFamilies          #-}
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