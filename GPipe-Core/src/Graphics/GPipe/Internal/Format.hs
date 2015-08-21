{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances, EmptyDataDecls #-}

module Graphics.GPipe.Internal.Format where

import Data.Word
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign.Marshal.Array (withArray)
import Linear.V4
import Linear.V3
import Linear.V2

data RFloat 
data RInt
data RWord 
data RGFloat 
data RGInt 
data RGWord 
data RGBFloat 
data RGBInt 
data RGBWord 
data RGBAFloat 
data RGBAInt 
data RGBAWord 
data Depth 
data Stencil 
data DepthStencil 
 
data Format a where
    R8 :: Format RFloat 
    R8S :: Format RFloat
    R16 :: Format RFloat
    R16S :: Format RFloat
    R16F :: Format RFloat
    R32F :: Format RFloat
    R8I :: Format RInt
    R16I :: Format RInt
    R32I :: Format RInt
    R8UI :: Format RWord
    R16UI :: Format RWord
    R32UI :: Format RWord
    RG8 :: Format RGFloat
    RG8S :: Format RGFloat 
    RG16 :: Format RGFloat 
    RG16S :: Format RGFloat 
    RG16F :: Format RGFloat 
    RG32F :: Format RGFloat 
    RG8I :: Format RGInt
    RG16I :: Format RGInt
    RG32I :: Format RGInt
    RG8UI :: Format RGWord
    RG16UI :: Format RGWord
    RG32UI :: Format RGWord
    R3G3B2 :: Format RGBFloat
    RGB4 :: Format RGBFloat
    RGB5 :: Format RGBFloat
    RGB8 :: Format RGBFloat
    RGB8S :: Format RGBFloat
    RGB10 :: Format RGBFloat
    RGB12 :: Format RGBFloat
    RGB16 :: Format RGBFloat
    RGB16S :: Format RGBFloat
    RGB16F :: Format RGBFloat
    RGB32F :: Format RGBFloat
    R11FG11FB10F :: Format RGBFloat
    RGB9E5 :: Format RGBFloat
    SRGB8 :: Format RGBFloat
    RGB8I :: Format RGBInt
    RGB16I :: Format RGBInt
    RGB32I :: Format RGBInt
    RGBWord :: Format RGBWord
    RGB8UI :: Format RGBWord
    RGB16UI :: Format RGBWord
    RGB32UI :: Format RGBWord
    RGBA2 :: Format RGBAFloat
    RGBA4 :: Format RGBAFloat
    RGB5A1 :: Format RGBAFloat
    RGBA8 :: Format RGBAFloat
    RGBA8S :: Format RGBAFloat
    RGB10A2 :: Format RGBAFloat
    RGBA12 :: Format RGBAFloat
    RGBA16 :: Format RGBAFloat
    RGBA16S :: Format RGBAFloat
    RGBA16F :: Format RGBAFloat
    RGBA32F :: Format RGBAFloat
    SRGB8A8 :: Format RGBAFloat
    RGBA8I :: Format RGBAInt
    RGBA16I :: Format RGBAInt
    RGBA32I :: Format RGBAInt
    RGBA8UI :: Format RGBAWord
    RGBA16UI :: Format RGBAWord
    RGBA32UI :: Format RGBAWord

    Depth16 :: Format Depth
    Depth24 :: Format Depth
    Depth32 :: Format Depth
    Depth32F :: Format Depth
    Stencil1 :: Format Stencil
    Stencil4 :: Format Stencil
    Stencil8 :: Format Stencil
    Stencil16 :: Format Stencil
    Depth24Stencil8  :: Format DepthStencil
    Depth32FStencil8 :: Format DepthStencil

getGlInternalFormat :: Format f -> GLenum
getGlInternalFormat R8 = GL_R8 
getGlInternalFormat R8S = GL_R8_SNORM
getGlInternalFormat R16 = GL_R16
getGlInternalFormat R16S = GL_R16_SNORM
getGlInternalFormat R16F = GL_R16F
getGlInternalFormat R32F = GL_R32F
getGlInternalFormat R8I = GL_R8I 
getGlInternalFormat R16I = GL_R16I
getGlInternalFormat R32I = GL_R32I
getGlInternalFormat R8UI = GL_R8UI 
getGlInternalFormat R16UI = GL_R16UI
getGlInternalFormat R32UI = GL_R32UI
getGlInternalFormat RG8 = GL_RG8 
getGlInternalFormat RG8S = GL_RG8_SNORM
getGlInternalFormat RG16 = GL_RG16
getGlInternalFormat RG16S = GL_RG16_SNORM
getGlInternalFormat RG16F = GL_RG16F
getGlInternalFormat RG32F = GL_RG32F
getGlInternalFormat RG8I = GL_RG8I 
getGlInternalFormat RG16I = GL_RG16I
getGlInternalFormat RG32I = GL_RG32I
getGlInternalFormat RG8UI = GL_RG8UI
getGlInternalFormat RG16UI = GL_RG16UI    
getGlInternalFormat RG32UI = GL_RG32UI
getGlInternalFormat R3G3B2 = GL_R3_G3_B2
getGlInternalFormat RGB4 = GL_RGB4
getGlInternalFormat RGB5 = GL_RGB5
getGlInternalFormat RGB8 = GL_RGB8
getGlInternalFormat RGB8S = GL_RGB8_SNORM
getGlInternalFormat RGB10 = GL_RGB10
getGlInternalFormat RGB12 = GL_RGB12
getGlInternalFormat RGB16 = GL_RGB16
getGlInternalFormat RGB16S = GL_RGB16_SNORM
getGlInternalFormat RGB16F = GL_RGB16F
getGlInternalFormat RGB32F = GL_RGB32F
getGlInternalFormat R11FG11FB10F = GL_R11F_G11F_B10F
getGlInternalFormat RGB9E5 = GL_RGB9_E5
getGlInternalFormat SRGB8 = GL_SRGB8
getGlInternalFormat RGB8I = GL_RGB8I
getGlInternalFormat RGB16I = GL_RGB16I
getGlInternalFormat RGB32I = GL_RGB32I
getGlInternalFormat RGB8UI = GL_RGB8UI
getGlInternalFormat RGB16UI = GL_RGB16UI
getGlInternalFormat RGB32UI = GL_RGB32UI
getGlInternalFormat RGBA2 = GL_RGBA2 
getGlInternalFormat RGBA4 = GL_RGBA4 
getGlInternalFormat RGB5A1 = GL_RGB5_A1 
getGlInternalFormat RGBA8 = GL_RGBA8 
getGlInternalFormat RGBA8S = GL_RGBA8_SNORM 
getGlInternalFormat RGB10A2 = GL_RGB10_A2 
getGlInternalFormat RGBA12 = GL_RGBA12 
getGlInternalFormat RGBA16 = GL_RGBA16 
getGlInternalFormat RGBA16S = GL_RGBA16_SNORM 
getGlInternalFormat RGBA16F = GL_RGBA16F 
getGlInternalFormat RGBA32F = GL_RGBA32F 
getGlInternalFormat SRGB8A8 = GL_SRGB8_ALPHA8 
getGlInternalFormat RGBA8I = GL_RGBA8I 
getGlInternalFormat RGBA16I = GL_RGBA16I 
getGlInternalFormat RGBA32I = GL_RGBA32I
getGlInternalFormat RGBA8UI = GL_RGBA8UI 
getGlInternalFormat RGBA16UI = GL_RGBA16UI 
getGlInternalFormat RGBA32UI = GL_RGBA32UI
getGlInternalFormat Depth16 = GL_DEPTH_COMPONENT16
getGlInternalFormat Depth24 = GL_DEPTH_COMPONENT24
getGlInternalFormat Depth32 = GL_DEPTH_COMPONENT32
getGlInternalFormat Depth32F = GL_DEPTH_COMPONENT32F
getGlInternalFormat Stencil1 = GL_STENCIL_INDEX1
getGlInternalFormat Stencil4 = GL_STENCIL_INDEX4
getGlInternalFormat Stencil8 = GL_STENCIL_INDEX8
getGlInternalFormat Stencil16 = GL_STENCIL_INDEX16
getGlInternalFormat Depth24Stencil8 = GL_DEPTH24_STENCIL8
getGlInternalFormat Depth32FStencil8 = GL_DEPTH32F_STENCIL8

class TextureFormat f where
    getGlFormat  :: f -> GLenum
    getGlFormat = error "You cannot create your own instances of TextureFormat"

instance TextureFormat RFloat where
    getGlFormat _ = GL_RED
instance TextureFormat RInt where
    getGlFormat _ = GL_RED_INTEGER
instance TextureFormat RWord where
    getGlFormat _ = GL_RED_INTEGER

instance TextureFormat RGFloat where
    getGlFormat _ = GL_RG
instance TextureFormat RGInt where
    getGlFormat _ = GL_RG_INTEGER
instance TextureFormat RGWord where
    getGlFormat _ = GL_RG_INTEGER

instance TextureFormat RGBFloat where
    getGlFormat _ = GL_RGB
instance TextureFormat RGBInt where
    getGlFormat _ = GL_RGB_INTEGER
instance TextureFormat RGBWord where
    getGlFormat _ = GL_RGB_INTEGER

instance TextureFormat RGBAFloat where
    getGlFormat _ = GL_RGBA
instance TextureFormat RGBAInt where
    getGlFormat _ = GL_RGBA_INTEGER
instance TextureFormat RGBAWord where
    getGlFormat _ = GL_RGBA_INTEGER

instance TextureFormat Depth where
    getGlFormat _ = GL_DEPTH_COMPONENT
instance TextureFormat Stencil where
    getGlFormat _ = GL_STENCIL_INDEX
instance TextureFormat DepthStencil where
    getGlFormat _ = GL_DEPTH_STENCIL

class TextureFormat f => ColorSampleable f where
    type Color f a
    type ColorElement f :: *
    typeStr :: f -> String
    typeStr4 :: f -> String
    toColor :: f -> V4 x -> Color f x
    fromColor :: f -> Color f x -> [x]
    setBorderColor :: f -> GLenum -> Color f (ColorElement f) -> IO ()
    samplerPrefix :: f -> String
    typeStr = error "You cannot create your own instances of ColorSampleable"
    typeStr4 = error "You cannot create your own instances of ColorSampleable"
    toColor = error "You cannot create your own instances of ColorSampleable"
    fromColor = error "You cannot create your own instances of ColorSampleable"
    setBorderColor = error "You cannot create your own instances of ColorSampleable"
    samplerPrefix _ = ""

instance ColorSampleable RFloat where
    type Color RFloat a = a
    type ColorElement RFloat = Float
    typeStr _ = "float"
    typeStr4 _ = "vec4"
    toColor _ (V4 r _ _ _) = r
    fromColor _ r = [r]
    setBorderColor _ t r = withArray [realToFrac r, 0,0,0] (glTexParameterfv t GL_TEXTURE_BORDER_COLOR)      
instance ColorSampleable RInt where
    type Color RInt a = a
    type ColorElement RInt = Int
    typeStr _ = "int"
    typeStr4 _ = "ivec4"
    toColor _ (V4 r _ _ _) = r
    fromColor _ r = [r]
    setBorderColor _ t r = withArray [fromIntegral r, 0,0,0] (glTexParameterIiv t GL_TEXTURE_BORDER_COLOR)
    samplerPrefix _ = "i"
instance ColorSampleable RWord where
    type Color RWord a = a
    type ColorElement RWord = Word
    typeStr _ = "uint"
    typeStr4 _ = "uvec4"
    toColor _ (V4 r _ _ _) = r
    fromColor _ r = [r]
    setBorderColor _ t r = withArray [fromIntegral r, 0,0,0] (glTexParameterIuiv t GL_TEXTURE_BORDER_COLOR)
    samplerPrefix _ = "u"

instance ColorSampleable RGFloat where
    type Color RGFloat a = V2 a
    type ColorElement RGFloat = Float
    typeStr _ = "vec2"
    typeStr4 _ = "vec4"
    toColor _ (V4 r g _ _) = V2 r g
    fromColor _ (V2 r g) = [r,g]
    setBorderColor _ t (V2 r g) = withArray [realToFrac r, realToFrac g,0,0] (glTexParameterfv t GL_TEXTURE_BORDER_COLOR)
instance ColorSampleable RGInt where
    type Color RGInt a = V2 a
    type ColorElement RGInt = Int
    typeStr _ = "ivec2"
    typeStr4 _ = "ivec4"
    toColor _ (V4 r g _ _) = V2 r g
    fromColor _ (V2 r g) = [r,g]
    setBorderColor _ t (V2 r g) = withArray [fromIntegral r, fromIntegral g,0,0] (glTexParameterIiv t GL_TEXTURE_BORDER_COLOR)
    samplerPrefix _ = "i"
instance ColorSampleable RGWord where
    type Color RGWord a = V2 a
    type ColorElement RGWord = Word
    typeStr _ = "uvec2"
    typeStr4 _ = "uvec4"
    toColor _ (V4 r g _ _) = V2 r g
    fromColor _ (V2 r g) = [r,g]
    setBorderColor _ t (V2 r g) = withArray [fromIntegral r, fromIntegral g,0,0] (glTexParameterIuiv t GL_TEXTURE_BORDER_COLOR)
    samplerPrefix _ = "u"
    
instance ColorSampleable RGBFloat where
    type Color RGBFloat a = V3 a
    type ColorElement RGBFloat = Float
    typeStr _ = "vec3"
    typeStr4 _ = "vec4"
    toColor _ (V4 r g b _) = V3 r g b
    fromColor _ (V3 r g b) = [r,g,b]
    setBorderColor _ t (V3 r g b) = withArray [realToFrac r, realToFrac g, realToFrac b,0] (glTexParameterfv t GL_TEXTURE_BORDER_COLOR)
instance ColorSampleable RGBInt where
    type Color RGBInt a = V3 a
    type ColorElement RGBInt = Int
    typeStr _ = "ivec3"
    typeStr4 _ = "ivec4"
    toColor _ (V4 r g b _) = V3 r g b
    fromColor _ (V3 r g b) = [r,g,b]
    setBorderColor _ t (V3 r g b) = withArray [fromIntegral r, fromIntegral g, fromIntegral b,0] (glTexParameterIiv t GL_TEXTURE_BORDER_COLOR)
    samplerPrefix _ = "i"
instance ColorSampleable RGBWord where
    type Color RGBWord a = V3 a
    type ColorElement RGBWord = Word
    typeStr _ = "uvec3"
    typeStr4 _ = "uvec4"
    toColor _ (V4 r g b _) = V3 r g b
    fromColor _ (V3 r g b) = [r,g,b]
    setBorderColor _ t (V3 r g b) = withArray [fromIntegral r, fromIntegral g, fromIntegral b,0] (glTexParameterIuiv t GL_TEXTURE_BORDER_COLOR)
    samplerPrefix _ = "u"

instance ColorSampleable RGBAFloat where
    type Color RGBAFloat a = V4 a
    type ColorElement RGBAFloat = Float
    typeStr _ = "vec4"
    typeStr4 _ = "vec4"
    toColor _ = id
    fromColor _ (V4 r g b a) = [r,g,b,a]
    setBorderColor _ t (V4 r g b a) = withArray [realToFrac r, realToFrac g, realToFrac b, realToFrac a] (glTexParameterfv t GL_TEXTURE_BORDER_COLOR)
instance ColorSampleable RGBAInt where
    type Color RGBAInt a = V4 a
    type ColorElement RGBAInt = Int
    typeStr _ = "ivec4"
    typeStr4 _ = "ivec4"
    toColor _ = id
    fromColor _ (V4 r g b a) = [r,g,b,a]
    setBorderColor _ t (V4 r g b a) = withArray [fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a] (glTexParameterIiv t GL_TEXTURE_BORDER_COLOR)
    samplerPrefix _ = "i"
instance ColorSampleable RGBAWord where
    type Color RGBAWord a = V4 a
    type ColorElement RGBAWord = Word
    typeStr _ = "uvec4"
    typeStr4 _ = "uvec4"
    toColor _ = id
    fromColor _ (V4 r g b a) = [r,g,b,a]
    setBorderColor _ t (V4 r g b a) = withArray [fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a] (glTexParameterIuiv t GL_TEXTURE_BORDER_COLOR)
    samplerPrefix _ = "u"

instance ColorSampleable Depth where
    type Color Depth a = a
    type ColorElement Depth = Float
    typeStr _ = "float"
    typeStr4 _ = "vec4"
    toColor _ (V4 r _ _ _) = r
    fromColor _ r = [r]
    setBorderColor _ t r = withArray [realToFrac r, 0,0,0] (glTexParameterfv t GL_TEXTURE_BORDER_COLOR)      
instance ColorSampleable DepthStencil where
    type Color DepthStencil a = a
    type ColorElement DepthStencil = Float
    typeStr _ = "float"
    typeStr4 _ = "vec4"
    toColor _ (V4 r _ _ _) = r
    fromColor _ r = [r]
    setBorderColor _ t r = withArray [realToFrac r, 0,0,0] (glTexParameterfv t GL_TEXTURE_BORDER_COLOR)      

class ColorSampleable c => ColorRenderable c where
    isSrgb :: Format c -> Bool
    isSrgb _ = False
    clearColor :: c -> Color c (ColorElement c) -> IO ()
    clearColor = error "You cannot create your own instances of ColorRenderable"
class ColorSampleable f => DepthRenderable f
class TextureFormat f => StencilRenderable f

instance ColorRenderable RFloat where
    clearColor _ r = withArray [realToFrac r, 0,0,0] (glClearBufferfv GL_COLOR 0)
instance ColorRenderable RInt where
    clearColor _ r = withArray [fromIntegral r, 0,0,0] (glClearBufferiv GL_COLOR 0)
instance ColorRenderable RWord where
    clearColor _ r = withArray [fromIntegral r, 0,0,0] (glClearBufferuiv GL_COLOR 0)
instance ColorRenderable RGFloat where
    clearColor _ (V2 r g) = withArray [realToFrac r, realToFrac g,0,0] (glClearBufferfv GL_COLOR 0)
instance ColorRenderable RGInt where
    clearColor _ (V2 r g) = withArray [fromIntegral r, fromIntegral g,0,0] (glClearBufferiv GL_COLOR 0)
instance ColorRenderable RGWord where
    clearColor _ (V2 r g) = withArray [fromIntegral r, fromIntegral g,0,0] (glClearBufferuiv GL_COLOR 0)
instance ColorRenderable RGBFloat where
    isSrgb SRGB8 = True
    isSrgb _ = False
    clearColor _ (V3 r g b) = withArray [realToFrac r, realToFrac g, realToFrac b,0] (glClearBufferfv GL_COLOR 0)
instance ColorRenderable RGBInt where
    clearColor _ (V3 r g b) = withArray [fromIntegral r, fromIntegral g, fromIntegral b,0] (glClearBufferiv GL_COLOR 0)
instance ColorRenderable RGBWord where
    clearColor _ (V3 r g b) = withArray [fromIntegral r, fromIntegral g, fromIntegral b,0] (glClearBufferuiv GL_COLOR 0)
instance ColorRenderable RGBAFloat where
    isSrgb SRGB8A8 = True
    isSrgb _ = False
    clearColor _ (V4 r g b a) = withArray [realToFrac r, realToFrac g, realToFrac b, realToFrac a] (glClearBufferfv GL_COLOR 0)
instance ColorRenderable RGBAInt where
    clearColor _ (V4 r g b a) = withArray [fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a] (glClearBufferiv GL_COLOR 0)
instance ColorRenderable RGBAWord where
    clearColor _ (V4 r g b a) = withArray [fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a] (glClearBufferuiv GL_COLOR 0)

instance DepthRenderable Depth
instance DepthRenderable DepthStencil

instance StencilRenderable Stencil
instance StencilRenderable DepthStencil


class ColorRenderable c => ContextColorFormat c where
    redBits :: Format c -> Int
    greenBits :: Format c -> Int
    blueBits :: Format c -> Int
    alphaBits :: Format c -> Int
    redBits = error "You cannot create your own instances of ContextColorFormat"
    greenBits = error "You cannot create your own instances of ContextColorFormat"
    blueBits = error "You cannot create your own instances of ContextColorFormat"
    alphaBits = error "You cannot create your own instances of ContextColorFormat"

instance ContextColorFormat RFloat where
    redBits R8 = 8
    redBits R8S = 8
    redBits R16 = 16
    redBits R16S = 16
    redBits R16F = 16
    redBits R32F = 32
    greenBits _ = 0
    blueBits _ = 0
    alphaBits _ = 0

instance ContextColorFormat RGFloat where
    redBits RG8 = 8
    redBits RG8S = 8
    redBits RG16 = 16
    redBits RG16S = 16
    redBits RG16F = 16
    redBits RG32F = 32
    greenBits = redBits
    blueBits _ = 0
    alphaBits _ = 0

instance ContextColorFormat RGBFloat where
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
    alphaBits _ = 0

instance ContextColorFormat RGBAFloat where
    redBits RGBA2 = 2
    redBits RGBA4 = 4
    redBits RGB5A1 = 5
    redBits RGBA8 = 8
    redBits RGBA8S = 8
    redBits RGB10A2 = 10
    redBits RGBA12 = 12
    redBits RGBA16 = 16
    redBits RGBA16S = 16
    redBits RGBA16F = 16
    redBits RGBA32F = 32
    redBits SRGB8A8 = 8   
    greenBits = redBits
    blueBits = redBits
    alphaBits RGB5A1 = 1
    alphaBits RGB10A2 = 2
    alphaBits x = redBits x
    
--------------------------------------------------------------------------

colorBits :: ContextColorFormat c => Format c -> (Int, Int, Int, Int, Bool)
colorBits f = (redBits f, greenBits f, blueBits f, alphaBits f, isSrgb f)

depthBits :: Format Depth  -> Int
depthBits Depth16 = 16
depthBits Depth24 = 24
depthBits Depth32 = 32
depthBits Depth32F = 32

stencilBits :: Format Stencil -> Int
stencilBits Stencil1 = 1
stencilBits Stencil4 = 4
stencilBits Stencil8 = 8
stencilBits Stencil16 = 16

depthStencilBits :: Format DepthStencil -> (Int, Int)
depthStencilBits Depth32FStencil8 = (32, 8)
depthStencilBits Depth24Stencil8 = (24, 8)

data ContextFormat c ds where
    ContextFormatNone :: ContextFormat () ()
    ContextFormatColor :: ContextColorFormat c => Format c -> ContextFormat c ()
    ContextFormatColorDepth :: ContextColorFormat c => Format c -> Format Depth -> ContextFormat c Depth
    ContextFormatColorStencil :: ContextColorFormat c => Format c -> Format Stencil -> ContextFormat c Stencil
    ContextFormatColorDepthStencilSeparate :: ContextColorFormat c => Format c -> Format Depth -> Format Stencil -> ContextFormat c DepthStencil
    ContextFormatColorDepthStencilCombined :: ContextColorFormat c => Format c -> Format DepthStencil -> ContextFormat c DepthStencil
    ContextFormatDepth :: Format Depth -> ContextFormat () Depth
    ContextFormatStencil :: Format Stencil -> ContextFormat () Stencil
    ContextFormatDepthStencilSeparate :: Format Depth -> Format Stencil -> ContextFormat () DepthStencil
    ContextFormatDepthStencilCombined :: Format DepthStencil -> ContextFormat () DepthStencil

contextBits :: ContextFormat c ds -> ((Int,Int,Int,Int,Bool),Int,Int)
contextBits ContextFormatNone = ((0,0,0,0, False),0,0)
contextBits (ContextFormatColor c) = (colorBits c, 0, 0)
contextBits (ContextFormatColorDepth c d) = (colorBits c, depthBits d, 0)
contextBits (ContextFormatColorStencil c s) = (colorBits c, 0, stencilBits s)
contextBits (ContextFormatColorDepthStencilSeparate c d s) = (colorBits c, depthBits d, stencilBits s)
contextBits (ContextFormatColorDepthStencilCombined c ds) = let (d,s) = depthStencilBits ds in (colorBits c, d, s)
contextBits (ContextFormatDepth d) = ((0,0,0,0, False), depthBits d, 0)
contextBits (ContextFormatStencil s) = ((0,0,0,0, False), 0, stencilBits s)
contextBits (ContextFormatDepthStencilSeparate d s) = ((0,0,0,0, False), depthBits d, stencilBits s)
contextBits (ContextFormatDepthStencilCombined ds) = let (d,s) = depthStencilBits ds in ((0,0,0,0, False), d, s)
