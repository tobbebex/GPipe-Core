{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module Graphics.GPipe.Texture where

import Graphics.GPipe.Format
import Graphics.GPipe.Shader
import Graphics.GPipe.Context
import Graphics.GPipe.Stream
import Graphics.GPipe.Buffer
import Control.Monad.IO.Class
import Data.Word
import Data.Int

-- | A structure describing how a texture is sampled
--data Sampler = Sampler Filter EdgeMode deriving (Eq, Ord)

-- | Filter mode used in sampler state
data Filter = Point | Linear
            deriving (Eq,Ord,Bounded,Enum,Show)
-- | Edge mode used in sampler state
data EdgeMode = Wrap | Mirror | Clamp
              deriving (Eq,Ord,Bounded,Enum,Show)

data Texture1D os a = Texture1D
data Texture2D os a = Texture2D
data Texture3D os a = Texture3D
data TextureCube os a = TextureCube

-- | The type of a color sample made by a texture t in a context c 
type ColorSample c t = Color (TextureFormat t) (S c (ColorElement (TextureFormat t)))

class Texture t where
    -- | The color format of the texture, affects the type of the samples from the texture. 
    type TextureFormat t
    -- | The type that is used for the dimension of texture. 
    type TextureSize t
    -- | The sample coordinate in a context c
    type TextureCoord c t
    -- | Calculates the byte size of all mipmaps for a specific format and size, which eases the useage of
    -- 'newTexture' and 'newDepthTexture'. The first parameter is not used.
    textureByteSize :: ColorFormat (TextureFormat t) => t -> TextureFormat t -> TextureSize t -> [Int]
    -- | Samples the texture using mipmaps in a 'Fragment'. 
    sample :: Sampler t fr -> TextureCoord F t -> ColorSample F t
    -- | Samples the texture using mipmaps in a 'Fragment', with a bias to add to the mipmap level. 
    sampleBias :: Sampler t fr -> t -> FFloat -> TextureCoord F t -> ColorSample F t
    -- | Samples the texture using a specific mipmap in a 'Vertex'. 
    sampleLod :: Sampler t fr -> t -> VFloat -> TextureCoord V t -> ColorSample V t

    mkTexture :: ColorFormat (TextureFormat t) => TextureFormat t -> TextureSize t -> IO t
   
newTexture :: (MonadIO m, ColorFormat (TextureFormat t)) => TextureFormat t -> TextureSize t -> ContextT os f m t
newTexture = undefined

writeTexture :: (MonadIO m, ColorFormat (TextureFormat t), PixelElementFormat a) => t -> TextureSize t -> TextureSize t -> [Color (TextureFormat t) a] -> ContextT os f m t
writeTexture = undefined

writeTextureFromBuffer :: (MonadIO m, ColorFormat (TextureFormat t), PixelElementFormat a) => t -> TextureSize t -> TextureSize t -> (b -> BufferPixel t a) -> Buffer os b -> ContextT os f m t
writeTextureFromBuffer = undefined

class PixelElementFormat a 
instance PixelElementFormat Int32
instance PixelElementFormat Int16
instance PixelElementFormat Int8
instance PixelElementFormat Word32
instance PixelElementFormat Word16
instance PixelElementFormat Word8

class BufferColorFormat a where
        type BufferColor a
instance BufferColorFormat (R a) where
        type BufferColor (R a) = B a
instance BufferColorFormat (RG a) where
        type BufferColor (RG a) = B2 (B a)
instance BufferColorFormat (RGB a) where
        type BufferColor (RGB a) = B3 (B a)
instance BufferColorFormat (RGBA a) where
        type BufferColor (RGBA a) = B4 (B a)

type BufferPixel t a = BufferColor (Color (TextureFormat t) a)

usingTexture :: Texture t => Stream fr x a -> (t, Filter, EdgeMode) -> Stream fr x (a, Sampler t fr)
usingTexture = undefined

data Sampler t fr = SamplerOpaque

samplerFilter :: Sampler t fr -> Filter
samplerFilter = undefined

samplerEdgeMode :: Sampler t fr -> EdgeMode
samplerEdgeMode = undefined
