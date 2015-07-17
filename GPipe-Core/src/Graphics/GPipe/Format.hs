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
data DepthStencilFormat = DepthStencilFormat DepthFormat StencilFormat | Depth32FStencil8 | Depth24Stencil8

class TextureFormat f where
    getGlInternalFormat :: f -> Int
    getGlFormat  :: f -> Int

instance TextureFormat RFloatFormat where
    getGlInternalFormat R8 = 90 
    getGlInternalFormat R8S = 91
    getGlInternalFormat R16 = 92
    getGlInternalFormat R16S = 93
    getGlInternalFormat R16F = 94
    getGlInternalFormat R32F = 95
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat RIntFormat where
    getGlInternalFormat R8I = 0 
    getGlInternalFormat R16I = 0
    getGlInternalFormat R32I = 0
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat RUIntFormat where
    getGlInternalFormat R8UI = 0 
    getGlInternalFormat R16UI = 0
    getGlInternalFormat R32UI = 0
    getGlFormat _ = 900 -- GL_RED

instance TextureFormat RGFloatFormat where
    getGlInternalFormat RG8 = 90 
    getGlInternalFormat RG8S = 91
    getGlInternalFormat RG16 = 92
    getGlInternalFormat RG16S = 93
    getGlInternalFormat RG16F = 94
    getGlInternalFormat RG32F = 95
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat RGIntFormat where
    getGlInternalFormat RG8I = 0 
    getGlInternalFormat RG16I = 0
    getGlInternalFormat RG32I = 0
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat RGUIntFormat where
    getGlInternalFormat RG8UI = 0 
    getGlInternalFormat RG16UI = 0
    getGlInternalFormat RG32UI = 0
    getGlFormat _ = 900 -- GL_RED

instance TextureFormat RGBFloatFormat where
    getGlInternalFormat R3G3B2 = 0
    getGlInternalFormat RGB4 = 0
    getGlInternalFormat RGB5 = 0
    getGlInternalFormat RGB8 = 0
    getGlInternalFormat RGB8S = 0
    getGlInternalFormat RGB10 = 0
    getGlInternalFormat RGB12 = 0
    getGlInternalFormat RGB16 = 0
    getGlInternalFormat RGB16S = 0
    getGlInternalFormat RGB16F = 0
    getGlInternalFormat RGB32F = 0
    getGlInternalFormat R11FG11FB10F = 0 
    getGlInternalFormat RGB9E5 = 0
    getGlInternalFormat SRGB8 = 0
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat RGBIntFormat where
    getGlInternalFormat RGB8I = 0 
    getGlInternalFormat RGB16I = 0
    getGlInternalFormat RGB32I = 0
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat RGBUIntFormat where
    getGlInternalFormat RGB8UI = 0 
    getGlInternalFormat RGB16UI = 0
    getGlInternalFormat RGB32UI = 0
    getGlFormat _ = 900 -- GL_RED

instance TextureFormat RGBAFloatFormat where
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat RGBAIntFormat where
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat RGBAUIntFormat where
    getGlFormat _ = 900 -- GL_RED

instance TextureFormat DepthFormat where
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat StencilFormat where
    getGlFormat _ = 900 -- GL_RED
instance TextureFormat DepthStencilFormat where
    getGlFormat _ = 900 -- GL_RED



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
