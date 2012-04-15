{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Graphics.GPipe.Format where

import Graphics.GPipe.Buffer

class BufferFormat b => InternalFormat b f 

instance InternalFormat BFloat AlphaFormat
instance InternalFormat BFloat LuminanceFormat
instance InternalFormat BFloat2 LuminanceAlphaFormat
instance InternalFormat BFloat3 RGBFormat
instance InternalFormat BFloat4 RGBAFormat
instance InternalFormat BFloat DepthFormat
instance InternalFormat BFloat StencilFormat
-- TODO: Add more InternalFormats

-- | A GPU format with only an alpha value.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat AlphaFormat@] 'CPUFormat1Comp'
--
-- [@Color AlphaFormat a@] @Alpha a@
data AlphaFormat = Alpha4 | Alpha8 | Alpha12 | Alpha16 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format with a single color component.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat LuminanceFormat@] 'CPUFormat1Comp'
--
-- [@Color LuminanceFormat a@] @Luminance a@
data LuminanceFormat = Luminance4 | Luminance8 | Luminance12 | Luminance16 | SLuminance8 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format with a single color component and an alpha value.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat LuminanceAlphaFormat@] 'CPUFormat2Comp'
--
-- [@Color LuminanceAlphaFormat a@] @LuminanceAlpha a a@
data LuminanceAlphaFormat = Luminance4Alpha4 | Luminance6Alpha2 | Luminance8Alpha8 | Luminance12Alpha4 | Luminance12Alpha12 | Luminance16Alpha16 | SLuminance8Alpha8 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format with color components for red, green and blue.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat RGBFormat@] 'CPUFormat3Comp'
--
-- [@Color RGBFormat a@] @RGB (@'Vec3'@ a)@
data RGBFormat = R3G3B2 | RGB4 | RGB5 | RGB8 | RGB10 | RGB12 | RGB16 | SRGB8 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format with color components for red, green and blue, and an alpha value.
-- These are the associated types in 'GPUFormat' and 'ColorFormat':
--
-- [@CPUFormat RGBAFormat@] 'CPUFormat4Comp'
--
-- [@Color RGBAFormat a@] @RGBA (@'Vec3'@ a) a@
data RGBAFormat = RGBA2 | RGBA4 | RGB5A1 | RGBA8 | RGB10A2 | RGBA12 | RGBA16 | SRGBA8 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format for a depth buffer value.
-- This is the associated type in 'GPUFormat':
--
-- [@CPUFormat DepthFormat@] 'CPUFormat1Comp'
data DepthFormat = Depth16 | Depth24 | Depth32 deriving (Eq,Ord,Bounded,Enum,Show)
-- | A GPU format for a stencil buffer value.
-- This is the associated type in 'GPUFormat':
--
-- [@CPUFormat StencilFormat@] 'CPUFormat1Comp'
data StencilFormat = StencilFormat deriving (Eq,Ord,Bounded,Enum,Show)

-- | This context is used to select which types can be used in a frame buffers color buffer, and also
-- to restrict the type of a texture.
class ColorFormat f where
    data Color f :: * -> *
    fromColor :: a -> a -> Color f a -> (a,a,a,a)
    toColor :: (a,a,a,a) -> Color f a

instance ColorFormat AlphaFormat where
    data Color AlphaFormat a = Alpha a deriving (Eq,Ord,Show)
    fromColor x _ (Alpha a) = (x,x,x,a)
    toColor (_,_,_,d) = Alpha d
instance ColorFormat LuminanceFormat where
    data Color LuminanceFormat a = Luminance a deriving (Eq,Ord,Show)
    fromColor x w (Luminance a) = (a,x,x,w)
    toColor (a,_,_,_) = Luminance a
instance ColorFormat LuminanceAlphaFormat where
    data Color LuminanceAlphaFormat a = LuminanceAlpha a a deriving (Eq,Ord,Show)
    fromColor x _ (LuminanceAlpha a b) = (a,x,x,b)
    toColor (a,_,_,d) = LuminanceAlpha a d
instance ColorFormat RGBFormat where
    data Color RGBFormat a = RGB (a,a,a) deriving (Eq,Ord,Show)
    fromColor _ w (RGB (a,b,c)) = (a,b,c,w)
    toColor (a,b,c,_) = RGB (a, b, c)
instance ColorFormat RGBAFormat where
    data Color RGBAFormat a = RGBA (a,a,a) a deriving (Eq,Ord,Show)
    fromColor _ _ (RGBA (a,b,c) d) = (a,b,c,d)
    toColor (a,b,c,d) = RGBA (a,b,c) d
    
    
class (ColorFormat (FramebufferColorFormat a)) => FramebufferFormat a where
    type FramebufferColorFormat a 
   
class FramebufferFormat a => DepthFramebufferFormat a where
class FramebufferFormat a => StencilFramebufferFormat a 

instance FramebufferFormat AlphaFormat where
    type FramebufferColorFormat AlphaFormat = AlphaFormat
instance FramebufferFormat LuminanceFormat where
    type FramebufferColorFormat LuminanceFormat = LuminanceFormat
instance FramebufferFormat LuminanceAlphaFormat where
    type FramebufferColorFormat LuminanceAlphaFormat = LuminanceAlphaFormat
instance FramebufferFormat RGBFormat where
    type FramebufferColorFormat RGBFormat = RGBFormat
instance FramebufferFormat RGBAFormat where
    type FramebufferColorFormat RGBAFormat = RGBAFormat

-- TODO: Add multiple colors in some way

instance ColorFormat a => FramebufferFormat (a, DepthFormat) where
    type FramebufferColorFormat (a, DepthFormat) = a
instance ColorFormat a => FramebufferFormat (a, (DepthFormat, StencilFormat)) where
    type FramebufferColorFormat (a, (DepthFormat, StencilFormat)) = a   
instance ColorFormat a => FramebufferFormat (a, StencilFormat) where
    type FramebufferColorFormat (a, StencilFormat) = a

instance ColorFormat a => DepthFramebufferFormat (a, DepthFormat)
instance ColorFormat a => DepthFramebufferFormat (a, (DepthFormat, StencilFormat))
instance ColorFormat a => StencilFramebufferFormat (a, (DepthFormat, StencilFormat))
instance ColorFormat a => StencilFramebufferFormat (a, StencilFormat)
    