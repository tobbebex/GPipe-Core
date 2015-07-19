{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Graphics.GPipe.Format where

import           Data.Int
import           Data.Word
import           Data.Vec

import Graphics.Rendering.OpenGL.Raw.Core33

data RFloatFormat = R8 | R8S | R16 | R16S | R16F | R32F
data RIntFormat = R8I | R16I | R32I
data RUIntFormat = R8UI | R16UI | R32UI

data RGFloatFormat = RG8 | RG8S | RG16 | RG16S | RG16F | RG32F
data RGIntFormat = RG8I | RG16I | RG32I
data RGUIntFormat = RG8UI | RG16UI | RG32UI

data RGBFloatFormat = R3G3B2 | RGB4 | RGB5 | RGB8 | RGB8S | RGB10 | RGB12 | RGB16 | RGB16S | RGB16F | RGB32F | R11FG11FB10F | RGB9E5 | SRGB8
data RGBIntFormat = RGB8I | RGB16I | RGB32I
data RGBUIntFormat = RGB8UI | RGB16UI | RGB32UI

data RGBAFloatFormat = RGBA2 | RGBA4 | RGB5A1 | RGBA8 | RGBA8S | RGB10A2 | RGBA12 | RGBA16 | RGBA16S | RGBA16F | RGBA32F | SRGB8A8
data RGBAIntFormat = RGBA8I | RGBA16I | RGBA32I
data RGBAUIntFormat = RGBA8UI | RGBA16UI | RGBA32UI

data DepthFormat = Depth16 | Depth24 | Depth32 | Depth32F
data StencilFormat = Stencil1 | Stencil4 | Stencil8 | Stencil16
data DepthStencilFormat = DepthStencilFormat DepthFormat StencilFormat | Depth24Stencil8 | Depth32FStencil8

class TextureFormat f where
    getGlInternalFormat :: f -> GLenum
    getGlFormat  :: f -> GLenum

instance TextureFormat RFloatFormat where
    getGlInternalFormat R8 = gl_R8 
    getGlInternalFormat R8S = gl_R8_SNORM
    getGlInternalFormat R16 = gl_R16
    getGlInternalFormat R16S = gl_R16_SNORM
    getGlInternalFormat R16F = gl_R16F
    getGlInternalFormat R32F = gl_R32F
    getGlFormat _ = gl_RED
instance TextureFormat RIntFormat where
    getGlInternalFormat R8I = gl_R8I 
    getGlInternalFormat R16I = gl_R16I
    getGlInternalFormat R32I = gl_R32I
    getGlFormat _ = gl_RED_INTEGER
instance TextureFormat RUIntFormat where
    getGlInternalFormat R8UI = gl_R8UI 
    getGlInternalFormat R16UI = gl_R16UI
    getGlInternalFormat R32UI = gl_R32UI
    getGlFormat _ = gl_RED_INTEGER

instance TextureFormat RGFloatFormat where
    getGlInternalFormat RG8 = gl_RG8 
    getGlInternalFormat RG8S = gl_RG8_SNORM
    getGlInternalFormat RG16 = gl_RG16
    getGlInternalFormat RG16S = gl_RG16_SNORM
    getGlInternalFormat RG16F = gl_RG16F
    getGlInternalFormat RG32F = gl_RG32F
    getGlFormat _ = gl_RG
instance TextureFormat RGIntFormat where
    getGlInternalFormat RG8I = gl_RG8I 
    getGlInternalFormat RG16I = gl_RG16I
    getGlInternalFormat RG32I = gl_RG32I
    getGlFormat _ = gl_RG_INTEGER
instance TextureFormat RGUIntFormat where
    getGlInternalFormat RG8UI = gl_RG8UI
    getGlInternalFormat RG16UI = gl_RG16UI    
    getGlInternalFormat RG32UI = gl_RG32UI
    getGlFormat _ = gl_RG_INTEGER

instance TextureFormat RGBFloatFormat where
    getGlInternalFormat R3G3B2 = gl_R3_G3_B2
    getGlInternalFormat RGB4 = gl_RGB4
    getGlInternalFormat RGB5 = gl_RGB5
    getGlInternalFormat RGB8 = gl_RGB8
    getGlInternalFormat RGB8S = gl_RGB8_SNORM
    getGlInternalFormat RGB10 = gl_RGB10
    getGlInternalFormat RGB12 = gl_RGB12
    getGlInternalFormat RGB16 = gl_RGB16
    getGlInternalFormat RGB16S = gl_RGB16_SNORM
    getGlInternalFormat RGB16F = gl_RGB16F
    getGlInternalFormat RGB32F = gl_RGB32F
    getGlInternalFormat R11FG11FB10F = gl_R11F_G11F_B10F
    getGlInternalFormat RGB9E5 = gl_RGB9_E5
    getGlInternalFormat SRGB8 = gl_SRGB8
    getGlFormat _ = gl_RGB
instance TextureFormat RGBIntFormat where
    getGlInternalFormat RGB8I = gl_RGB8I
    getGlInternalFormat RGB16I = gl_RGB16I
    getGlInternalFormat RGB32I = gl_RGB32I
    getGlFormat _ = gl_RGB_INTEGER
instance TextureFormat RGBUIntFormat where
    getGlInternalFormat RGB8UI = gl_RGB8UI
    getGlInternalFormat RGB16UI = gl_RGB16UI
    getGlInternalFormat RGB32UI = gl_RGB32UI
    getGlFormat _ = gl_RGB_INTEGER

instance TextureFormat RGBAFloatFormat where
    getGlInternalFormat RGBA2 = gl_RGBA2 
    getGlInternalFormat RGBA4 = gl_RGBA4 
    getGlInternalFormat RGB5A1 = gl_RGB5_A1 
    getGlInternalFormat RGBA8 = gl_RGBA8 
    getGlInternalFormat RGBA8S = gl_RGBA8_SNORM 
    getGlInternalFormat RGB10A2 = gl_RGB10_A2 
    getGlInternalFormat RGBA12 = gl_RGBA12 
    getGlInternalFormat RGBA16 = gl_RGBA16 
    getGlInternalFormat RGBA16S = gl_RGBA16_SNORM 
    getGlInternalFormat RGBA16F = gl_RGBA16F 
    getGlInternalFormat RGBA32F = gl_RGBA32F 
    getGlInternalFormat SRGB8A8 = gl_SRGB8_ALPHA8 
    getGlFormat _ = gl_RGBA
instance TextureFormat RGBAIntFormat where
    getGlInternalFormat RGBA8I = gl_RGBA8I 
    getGlInternalFormat RGBA16I = gl_RGBA16I 
    getGlInternalFormat RGBA32I = gl_RGBA32I
    getGlFormat _ = gl_RGBA_INTEGER
instance TextureFormat RGBAUIntFormat where
    getGlInternalFormat RGBA8UI = gl_RGBA8UI 
    getGlInternalFormat RGBA16UI = gl_RGBA16UI 
    getGlInternalFormat RGBA32UI = gl_RGBA32UI
    getGlFormat _ = gl_RGBA_INTEGER

instance TextureFormat DepthFormat where
    getGlInternalFormat Depth16 = gl_DEPTH_COMPONENT16
    getGlInternalFormat Depth24 = gl_DEPTH_COMPONENT24
    getGlInternalFormat Depth32 = gl_DEPTH_COMPONENT32
    getGlInternalFormat Depth32F = gl_DEPTH_COMPONENT32F
    getGlFormat _ = gl_DEPTH_COMPONENT
instance TextureFormat StencilFormat where
    getGlInternalFormat Stencil1 = gl_STENCIL_INDEX1
    getGlInternalFormat Stencil4 = gl_STENCIL_INDEX4
    getGlInternalFormat Stencil8 = gl_STENCIL_INDEX8
    getGlInternalFormat Stencil16 = gl_STENCIL_INDEX16
    getGlFormat _ = gl_STENCIL_INDEX
instance TextureFormat DepthStencilFormat where
    getGlInternalFormat Depth24Stencil8 = gl_DEPTH24_STENCIL8
    getGlInternalFormat Depth32FStencil8 = gl_DEPTH32F_STENCIL8
    getGlInternalFormat _ = gl_DEPTH_STENCIL -- Not to be used
    getGlFormat _ = gl_DEPTH_STENCIL

class TextureFormat f => ColorSampleable f where
    type Color f :: * -> *
    type ColorElement f :: *
    typeStr :: f -> String
    toColor :: f -> (x,x,x,x) -> Color f x
    fromColor :: f -> Color f x -> [x]

instance ColorSampleable RFloatFormat where
    type Color RFloatFormat = V1
    type ColorElement RFloatFormat = Float
    typeStr _ = "float"
    toColor _ (r,_,_,_) = V1 r
    fromColor _ (V1 r) = [r]
    
instance ColorSampleable RIntFormat where
    type Color RIntFormat = V1
    type ColorElement RIntFormat = Int
    typeStr _ = "int"
    toColor _ (r,_,_,_) = V1 r
    fromColor _ (V1 r) = [r]

instance ColorSampleable RUIntFormat where
    type Color RUIntFormat = V1
    type ColorElement RUIntFormat = Word
    typeStr _ = "uint"
    toColor _ (r,_,_,_) = V1 r
    fromColor _ (V1 r) = [r]

instance ColorSampleable RGFloatFormat where
    type Color RGFloatFormat = V2
    type ColorElement RGFloatFormat = Float
    typeStr _ = "vec2"
    toColor _ (r,g,_,_) = V2 r g
    fromColor _ (V2 r g) = [r,g]
instance ColorSampleable RGIntFormat where
    type Color RGIntFormat = V2
    type ColorElement RGIntFormat = Int
    typeStr _ = "ivec2"
    toColor _ (r,g,_,_) = V2 r g
    fromColor _ (V2 r g) = [r,g]
instance ColorSampleable RGUIntFormat where
    type Color RGUIntFormat = V2
    type ColorElement RGUIntFormat = Word
    typeStr _ = "uvec2"
    toColor _ (r,g,_,_) = V2 r g
    fromColor _ (V2 r g) = [r,g]
    
instance ColorSampleable RGBFloatFormat where
    type Color RGBFloatFormat = V3
    type ColorElement RGBFloatFormat = Float
    typeStr _ = "vec3"
    toColor _ (r,g,b,_) = V3 r g b
    fromColor _ (V3 r g b) = [r,g,b]
instance ColorSampleable RGBIntFormat where
    type Color RGBIntFormat = V3
    type ColorElement RGBIntFormat = Int
    typeStr _ = "ivec3"
    toColor _ (r,g,b,_) = V3 r g b
    fromColor _ (V3 r g b) = [r,g,b]
instance ColorSampleable RGBUIntFormat where
    type Color RGBUIntFormat = V3
    type ColorElement RGBUIntFormat = Word
    typeStr _ = "uvec3"
    toColor _ (r,g,b,_) = V3 r g b
    fromColor _ (V3 r g b) = [r,g,b]
instance ColorSampleable RGBAFloatFormat where
    type Color RGBAFloatFormat = V4
    type ColorElement RGBAFloatFormat = Float
    typeStr _ = "vec4"
    toColor _ (r,g,b,a) = V4 r g b a
    fromColor _ (V4 r g b a) = [r,g,b,a]
instance ColorSampleable RGBAIntFormat where
    type Color RGBAIntFormat = V4
    type ColorElement RGBAIntFormat = Int
    typeStr _ = "ivec4"
    toColor _ (r,g,b,a) = V4 r g b a
    fromColor _ (V4 r g b a) = [r,g,b,a]
instance ColorSampleable RGBAUIntFormat where
    type Color RGBAUIntFormat = V4
    type ColorElement RGBAUIntFormat = Word
    typeStr _ = "uvec4"
    toColor _ (r,g,b,a) = V4 r g b a
    fromColor _ (V4 r g b a) = [r,g,b,a]

instance ColorSampleable DepthFormat where
    type Color DepthFormat = V1
    type ColorElement DepthFormat = Float
    typeStr _ = "float"
    toColor _ (r,_,_,_) = V1 r
    fromColor _ (V1 r) = [r]
instance ColorSampleable DepthStencilFormat where
    type Color DepthStencilFormat = V1
    type ColorElement DepthStencilFormat = Float
    typeStr _ = "float"
    toColor _ (r,_,_,_) = V1 r
    fromColor _ (V1 r) = [r]

class ColorSampleable c => ColorRenderable c
class TextureFormat f => DepthRenderable f
class TextureFormat f => StencilRenderable f

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

instance DepthRenderable DepthFormat
instance DepthRenderable DepthStencilFormat

instance StencilRenderable StencilFormat
instance StencilRenderable DepthStencilFormat


class ColorRenderable c => ContextColorFormat c where
    redBits :: c -> Int
    greenBits :: c -> Int
    blueBits :: c -> Int
    isSrgb :: c -> Bool

instance ContextColorFormat RFloatFormat where
    redBits R8 = 8
    redBits R8S = 8
    redBits R16 = 16
    redBits R16S = 16
    redBits R16F = 16
    redBits R32F = 32
    greenBits _ = 0
    blueBits _ = 0
    isSrgb _ = False

instance ContextColorFormat RGFloatFormat where
    redBits RG8 = 8
    redBits RG8S = 8
    redBits RG16 = 16
    redBits RG16S = 16
    redBits RG16F = 16
    redBits RG32F = 32
    greenBits = redBits
    blueBits _ = 0
    isSrgb _ = False

instance ContextColorFormat RGBFloatFormat where
    redBits R3G3B2 = 3
    redBits RGB4 = 4
    redBits RGB5 = 5
    redBits RGB8 = 8
    redBits RGB8S = 8
    redBits RGB10 = 10
    redBits RGB12 = 12
    redBits RGB16 = 16
    redBits RGB16S = 16
    redBits RGB16F = 16
    redBits RGB32F = 32
    redBits R11FG11FB10F = 11
    redBits RGB9E5 = 14 -- hmm...
    redBits SRGB8 = 8
    greenBits = redBits
    blueBits R3G3B2 = 2
    blueBits R11FG11FB10F = 10
    blueBits x = redBits x
    isSrgb SRGB8 = True
    isSrgb _ = False

--------------------------------------------------------------------------

colorBits :: ContextColorFormat c => c -> (Int, Int, Int, Bool)
colorBits f = (redBits f, greenBits f, blueBits f, isSrgb f)

depthBits :: DepthFormat  -> Int
depthBits Depth16 = 16
depthBits Depth24 = 24
depthBits Depth32 = 32
depthBits Depth32F = 32

stencilBits :: StencilFormat -> Int
stencilBits Stencil1 = 1
stencilBits Stencil4 = 4
stencilBits Stencil8 = 8
stencilBits Stencil16 = 16

depthStencilBits :: DepthStencilFormat -> (Int, Int)
depthStencilBits (DepthStencilFormat d s) = (depthBits d, stencilBits s)
depthStencilBits Depth32FStencil8 = (32, 8)
depthStencilBits Depth24Stencil8 = (24, 8)

data ContextFormat c ds where
    ContextFormatNone :: ContextFormat () ()
    ContextFormatColor :: ContextColorFormat c => c -> ContextFormat c ()
    ContextFormatColorDepth :: ContextColorFormat c => c -> DepthFormat -> ContextFormat c DepthFormat
    ContextFormatColorStencil :: ContextColorFormat c => c -> StencilFormat  -> ContextFormat c StencilFormat
    ContextFormatColorDepthStencil :: ContextColorFormat c => c -> DepthStencilFormat -> ContextFormat c DepthStencilFormat
    ContextFormatDepth :: DepthFormat -> ContextFormat () DepthFormat
    ContextFormatStencil :: StencilFormat  -> ContextFormat () StencilFormat
    ContextFormatDepthStencil :: DepthStencilFormat -> ContextFormat () DepthStencilFormat

contextBits :: ContextFormat c ds -> ((Int,Int,Int,Bool),Int,Int)
contextBits ContextFormatNone = ((0,0,0, False),0,0)
contextBits (ContextFormatColor c) = (colorBits c, 0, 0)
contextBits (ContextFormatColorDepth c d) = (colorBits c, depthBits d, 0)
contextBits (ContextFormatColorStencil c s) = (colorBits c, 0, stencilBits s)
contextBits (ContextFormatColorDepthStencil c ds) = let (d,s) = depthStencilBits ds in (colorBits c, d, s)
contextBits (ContextFormatDepth d) = ((0,0,0, False), depthBits d, 0)
contextBits (ContextFormatStencil s) = ((0,0,0, False), 0, stencilBits s)
contextBits (ContextFormatDepthStencil ds) = let (d,s) = depthStencilBits ds in ((0,0,0, False), d, s)
