{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, GADTs #-}

module Graphics.GPipe.Format where

import Data.Int
import Data.Word

data R a = R a
data RG a = RG a a
data RGB a = RGB a a a
data RGBA a = RGBA a a a a
 
class ColorFormat a where
    type Color a :: * -> *
    type ColorElement a :: *

data RFloatFormat = R8 | R8S | R16 | R16S | R16F | R32F
data RIntFormat = R8I | R16I | R32I
data RUIntFormat = R8UI | R16UI | R32UI

data RGFloatFormat = RG8 | RG8S | RG16 | RG16S | RG16F | RG32F
data RGIntFormat = RG8I | RG16I | RG32I
data RGUIntFormat = RG8UI | RG16UI | RG32UI

data RGBFloatFormat = R3G3S2 | RGB4 | RGB5 | RGB8 | RGB8S | RGB10 | RGB12 | RGB16 | RGB16S | RGB16F | RGB32F | R11FG11FB10F | RGB9E5 | SRGB8  
data RGBIntFormat = RGB8I | RGB16I | RGB32I
data RGBUIntFormat = RGB8UI | RGB16UI | RGB32UI

data RGBAFloatFormat = RGBA2 | RGBA4 | RGB5A1 | RGBA8 | RGBA8S | RGB10A2 | RGBA12 | RGBA16 | RGBA16S | RGBA16F | RGBA32F | SRGB8A8  
data RGBAIntFormat = RGBA8I | RGBA16I | RGBA32I
data RGBAUIntFormat = RGBA8UI | RGBA16UI | RGBA32UI

instance ColorFormat RFloatFormat where
    type Color RFloatFormat = R
    type ColorElement RFloatFormat = Float
instance ColorFormat RIntFormat where
    type Color RIntFormat = R
    type ColorElement RIntFormat = Int32 
instance ColorFormat RUIntFormat where
    type Color RUIntFormat = R
    type ColorElement RUIntFormat = Word32
instance ColorFormat RGFloatFormat where
    type Color RGFloatFormat = RG
    type ColorElement RGFloatFormat = Float 
instance ColorFormat RGIntFormat where
    type Color RGIntFormat = RG
    type ColorElement RGIntFormat = Int32 
instance ColorFormat RGUIntFormat where
    type Color RGUIntFormat = RG
    type ColorElement RGUIntFormat = Word32
instance ColorFormat RGBFloatFormat where
    type Color RGBFloatFormat = RGB    
    type ColorElement RGBFloatFormat = Float 
instance ColorFormat RGBIntFormat where
    type Color RGBIntFormat = RGB
    type ColorElement RGBIntFormat = Int32 
instance ColorFormat RGBUIntFormat where
    type Color RGBUIntFormat = RGB
    type ColorElement RGBUIntFormat = Word32
instance ColorFormat RGBAFloatFormat where
    type Color RGBAFloatFormat = RGB    
    type ColorElement RGBAFloatFormat = Float 
instance ColorFormat RGBAIntFormat where
    type Color RGBAIntFormat = RGBA
    type ColorElement RGBAIntFormat = Int32 
instance ColorFormat RGBAUIntFormat where
    type Color RGBAUIntFormat = RGBA
    type ColorElement RGBAUIntFormat = Word32

instance ColorFormat DepthFormat where
    type Color DepthFormat = R    
    type ColorElement DepthFormat = Float 
instance ColorFormat DepthStencilFormat where
    type Color DepthStencilFormat = R    
    type ColorElement DepthStencilFormat = Float 

class (RenderBufferFormat c, ColorFormat c) => ColorRenderable c
instance ColorRenderable RFloatFormat
instance ColorRenderable RIntFormat 
instance ColorRenderable RUIntFormat 
instance ColorRenderable RGFloatFormat
instance ColorRenderable RGIntFormat 
instance ColorRenderable RGUIntFormat 
instance ColorRenderable RGBFloatFormat
instance ColorRenderable RGBIntFormat 
instance ColorRenderable RGBUIntFormat 
instance ColorRenderable RGBAFloatFormat
instance ColorRenderable RGBAIntFormat 
instance ColorRenderable RGBAUIntFormat 

class RenderBufferFormat f where
instance RenderBufferFormat RFloatFormat
instance RenderBufferFormat RIntFormat 
instance RenderBufferFormat RUIntFormat 
instance RenderBufferFormat RGFloatFormat
instance RenderBufferFormat RGIntFormat 
instance RenderBufferFormat RGUIntFormat 
instance RenderBufferFormat RGBFloatFormat
instance RenderBufferFormat RGBIntFormat 
instance RenderBufferFormat RGBUIntFormat 
instance RenderBufferFormat RGBAFloatFormat
instance RenderBufferFormat RGBAIntFormat 
instance RenderBufferFormat RGBAUIntFormat
instance RenderBufferFormat DepthFormat
instance RenderBufferFormat StencilFormat
instance RenderBufferFormat DepthStencilFormat
 

--------------------------------------------------------------------------

data DepthFormat = Depth16 | Depth24 | Depth32 | Depth32F 
data StencilFormat = Stencil1 | Stencil4 | Stencil8 | Stencil16
data DepthStencilFormat = DepthStencilFormat DepthFormat StencilFormat | Depth32FStencil8 | Depth24Stencil8 

class RenderBufferFormat f => DepthRenderable f 
class RenderBufferFormat f => StencilRenderable f

instance DepthRenderable DepthFormat
instance DepthRenderable DepthStencilFormat

instance StencilRenderable StencilFormat
instance StencilRenderable DepthStencilFormat

data ContextFormat c ds where
    ContextFormatColor :: ColorRenderable c => c -> ContextFormat c ()  
    ContextFormatColorDepth :: ColorRenderable c => c -> DepthFormat -> ContextFormat c DepthFormat  
    ContextFormatColorStencil :: ColorRenderable c => c -> StencilFormat  -> ContextFormat c StencilFormat  
    ContextFormatColorDepthStencil :: ColorRenderable c => c -> DepthStencilFormat -> ContextFormat c DepthStencilFormat  
    ContextFormatDepth :: DepthFormat -> ContextFormat () DepthFormat  
    ContextFormatStencil :: StencilFormat  -> ContextFormat () StencilFormat  
    ContextFormatDepthStencil :: DepthStencilFormat -> ContextFormat () DepthStencilFormat  
    