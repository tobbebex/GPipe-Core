{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables #-}

module Graphics.GPipe.Texture where

import Graphics.GPipe.Format
import Graphics.GPipe.Shader
import Graphics.GPipe.Shader.Operations
import Graphics.GPipe.Context
import Graphics.GPipe.Stream
import Graphics.GPipe.Buffer
import Control.Monad.IO.Class
import Data.Word
import Data.Int

-- | A structure describing how a texture is sampled
--data Sampler = Sampler Filter EdgeMode deriving (Eq, Ord)


type TextureName = Int

data Texture1D os a = Texture1D TextureName Int
data Texture1DArray os a = Texture1DArray TextureName (Int, Int) 
data Texture2D os a = Texture2D TextureName (Int, Int)
data Texture2DRect os a = Texture2DRect TextureName (Int, Int)
data Texture2DArray os a = Texture2DArray TextureName (Int, Int, Int)
data Texture3D os a = Texture3D TextureName (Int, Int, Int)
data TextureCube os a = TextureCube TextureName Int

-- | The type of a color sample made by a texture t in a context c 
type ColorSample c t = Color (TextureFormat t) (S c (ColorElement (TextureFormat t)))

class Texture t where
    -- | The object space (gl context) of the texture  
    type TextureObjectSpace t
    -- | The color format of the texture, affects the type of the samples from the texture. 
    type TextureFormat t
    -- | The type that is used for the dimension of the texture. 
    type TextureSize t
    -- | The type that is used to specify a region of the texture. 
    type TextureRegion t
    -- | The type for sample coordinates in a context c
    type TextureCoord c t
    -- | Calculates the byte size of all regions for a texture, which eases the useage of
    -- setting up texture data.
    textureByteSize :: ColorFormat (TextureFormat t) => t -> [(TextureRegion t, Int)]
    -- | Samples the texture using mipmaps in a 'Fragment'. 
    sample :: Sampler t fr -> TextureCoord F t -> ColorSample F t
    -- | Samples the texture using mipmaps in a 'Fragment', with a bias to add to the mipmap level. 
    sampleBias :: Sampler t fr -> t -> FFloat -> TextureCoord F t -> ColorSample F t
    -- | Samples the texture using a specific mipmap in a 'Vertex'. 
    sampleLod :: Sampler t fr -> t -> VFloat -> TextureCoord V t -> ColorSample V t

    mkTexture :: ColorFormat (TextureFormat t) => TextureFormat t -> TextureSize t -> IO t
    textureName :: t -> TextureName
    samplerTypeName :: t -> String

instance Texture (Texture1D os a) where
        type TextureObjectSpace (Texture1D os a) = os
        type TextureFormat (Texture1D os a) = a
        type TextureSize (Texture1D os a) = Int
        type TextureRegion (Texture1D os a) = (Int, Int, Int)
        type TextureCoord c (Texture1D os a) = S c Float
        samplerTypeName _ = "1D"
instance Texture (Texture1DArray os a) where
        type TextureObjectSpace (Texture1DArray os a) = os
        type TextureFormat (Texture1DArray os a) = a
        type TextureSize (Texture1DArray os a) = (Int, Int)
        type TextureRegion (Texture1DArray os a) = ((Int, Int), (Int, Int), Int)
        type TextureCoord c (Texture1DArray os a) = (S c Float, S c Float)
        samplerTypeName _ = "1DArray"
instance ColorFormat a => Texture (Texture2D os a) where
        type TextureObjectSpace (Texture2D os a) = os
        type TextureFormat (Texture2D os a) = a
        type TextureSize (Texture2D os a) = (Int, Int)
        type TextureRegion (Texture2D os a) = ((Int, Int), (Int, Int), Int)
        type TextureCoord c (Texture2D os a) = (S c Float, S c Float)
        samplerTypeName _ = "2D"
        sample s (S x, S y) = let prefix = typePrefix (undefined :: a)
                                  m = do sName <- useSampler (samplerName s)
                                         x' <- x
                                         y' <- y 
                                         tellAssignment 
                                                (STypeDyn $ prefix:"vec4") 
                                                ("texture(" ++ sName  ++ ", vec2(" ++ x' ++ ',' : y' ++ "))")
                                  f p = S $ fmap (++ p) m
                              in toColor (undefined :: a) (f ".r", f ".g", f".b", f ".a")
instance Texture (Texture2DRect os a) where
        type TextureObjectSpace (Texture2DRect os a) = os
        type TextureFormat (Texture2DRect os a) = a
        type TextureSize (Texture2DRect os a) = (Int, Int)
        type TextureRegion (Texture2DRect os a) = ((Int, Int), (Int, Int))
        type TextureCoord c (Texture2DRect os a) = (S c Float, S c Float)
        samplerTypeName _ = "2DRect"
instance Texture (Texture2DArray os a) where
        type TextureObjectSpace (Texture2DArray os a) = os
        type TextureFormat (Texture2DArray os a) = a
        type TextureSize (Texture2DArray os a) = (Int, Int, Int)
        type TextureRegion (Texture2DArray os a) = ((Int, Int, Int), (Int, Int, Int), Int)
        type TextureCoord c (Texture2DArray os a) = (S c Float, S c Float, S c Float)
        samplerTypeName _ = "2DArray"
instance Texture (Texture3D os a) where
        type TextureObjectSpace (Texture3D os a) = os
        type TextureFormat (Texture3D os a) = a
        type TextureSize (Texture3D os a) = (Int, Int, Int)
        type TextureRegion (Texture3D os a) = ((Int, Int, Int), (Int, Int, Int), Int)
        type TextureCoord c (Texture3D os a) = (S c Float, S c Float, S c Float)
        samplerTypeName _ = "3D"
instance Texture (TextureCube os a) where
        type TextureObjectSpace (TextureCube os a) = os
        type TextureFormat (TextureCube os a) = a
        type TextureSize (TextureCube os a) = Int
        type TextureRegion (TextureCube os a) = ((Int, Int), (Int, Int), TextureCubeSide, Int)
        type TextureCoord c (TextureCube os a) = (S c Float, S c Float, S c Float)
        samplerTypeName _ = "Cube"

data TextureCubeSide = TextureCubePosX | TextureCubeNegX | TextureCubePosY | TextureCubeNegY | TextureCubePosZ | TextureCubeNegZ

data RenderTarget os f where
    RenderBuffer :: RenderBuffer os f -> RenderTarget os f 
    RenderTexture :: (Texture t, TextureObjectSpace t ~ os) => t -> TextureRegion t -> RenderTarget os (TextureFormat f) 

type RenderTargetRegion = ((Int, Int), (Int, Int))

data RenderBuffer os f = RB

data DepthStencil os cd where
    Depth :: DepthRenderable d => RenderTarget os d -> DepthStencil os DepthFormat   
    Stencil :: StencilRenderable s => RenderTarget os s -> DepthStencil os StencilFormat
    DepthStencil :: (DepthRenderable d , StencilRenderable s) => RenderTarget os d -> RenderTarget os s -> DepthStencil os DepthStencilFormat
    NoDepthStencil :: DepthStencil os ()
   
newTexture :: (MonadIO m, ColorFormat (TextureFormat t), TextureObjectSpace t ~ os) => TextureFormat t -> TextureSize t -> ContextT os f m t
newTexture = undefined
-- glGenTEx, glTexImageXD(NULL)

writeTexture :: (MonadIO m, ColorFormat (TextureFormat t), PixelElementFormat (ColorElement (TextureFormat t)) a, TextureObjectSpace t ~ os) => t -> TextureRegion t -> [Color (TextureFormat t) a] -> ContextT os f m ()
writeTexture = undefined
-- glSubTexImageXD

writeTextureFromBuffer :: (MonadIO m, ColorFormat (TextureFormat t), PixelElementFormat (ColorElement (TextureFormat t)) a, TextureObjectSpace t ~ os) => t -> TextureRegion t -> Int -> Buffer os (BufferPixel (TextureFormat t) a) -> ContextT os f m ()
writeTextureFromBuffer = undefined
-- glBindBuffer PIXEL_UNPACK, glSubTexImageXD

newRenderBuffer :: RenderBufferFormat f => f -> ContextT os f2 m (RenderBuffer os f)   
newRenderBuffer = undefined 


readRenderTarget :: (MonadIO m, ColorFormat f, PixelElementFormat (ColorElement f) a) => RenderTarget os f -> RenderTargetRegion -> ([Color f a] -> m x) -> ContextT os f2 m x
readRenderTarget = undefined
-- glReadPixels

writeBufferFromRenderTarget ::  (MonadIO m, ColorFormat f, PixelElementFormat (ColorElement f) a) => RenderTarget os f -> RenderTargetRegion -> Buffer os (BufferPixel f a) -> Int -> ContextT os f2 m ()
writeBufferFromRenderTarget = undefined
-- glBindBuffer PIXEL_PACK, glReadPixels


writeTextureFromRenderTarget :: (MonadIO m, Texture t, ColorFormat (TextureFormat t), TextureObjectSpace t ~ os) =>
                                        RenderTarget os (TextureFormat t) -> (Int, Int) -> t -> TextureRegion t -> ContextT os f m ()
                                        -- Too restrictive format? Enable conversion?
writeTextureFromRenderTarget = undefined
-- glCopySubTexImageXD
writeTextureFromDepthRenderTarget :: (MonadIO m, Texture t, TextureFormat t ~ DepthFormat, TextureObjectSpace t ~ os, DepthRenderable ds) =>
                                        RenderTarget os ds -> (Int, Int) -> t -> TextureRegion t -> ContextT os f m ()
writeTextureFromDepthRenderTarget = undefined
-- glCopySubTexImageXD


readStencilRenderTarget :: (MonadIO m, StencilRenderable f, PixelElementFormat Word32 a) => RenderTarget os f -> RenderTargetRegion -> ([a] -> m x) -> ContextT os f2 m x
readStencilRenderTarget = undefined
-- glReadPixels
writeBufferFromStencilRenderTarget ::  (MonadIO m, StencilRenderable f, PixelElementFormat Word32 a) => RenderTarget os f -> RenderTargetRegion -> Buffer os a -> Int -> ContextT os f2 m ()
writeBufferFromStencilRenderTarget = undefined
-- glBindBuffer PIXEL_PACK, glReadPixels

writeBufferFromContextColor  ::  (MonadIO m, ColorFormat c, PixelElementFormat (ColorElement c) a) => RenderTargetRegion -> Buffer os (BufferPixel c a) -> Int -> ContextT os (ContextFormat c ds) m ()
writeBufferFromContextColor = undefined
writeBufferFromContextDepth  ::  (MonadIO m, DepthRenderable ds, PixelElementFormat Float a) => RenderTargetRegion -> Buffer os a -> Int -> ContextT os (ContextFormat c ds) m ()
writeBufferFromContextDepth = undefined
writeBufferFromContextStencil  ::  (MonadIO m, StencilRenderable ds, PixelElementFormat Word32 a) => RenderTargetRegion -> Buffer os a -> Int -> ContextT os (ContextFormat c ds) m ()
writeBufferFromContextStencil = undefined

readContextColor :: (MonadIO m, ColorFormat c, PixelElementFormat (ColorElement c) a) => RenderTargetRegion -> ([Color c a] -> m x) -> ContextT os (ContextFormat c ds) m x
readContextColor = undefined
readContextDepth :: (MonadIO m, DepthRenderable ds, PixelElementFormat Float a) => RenderTargetRegion -> ([a] -> m x) -> ContextT os (ContextFormat c ds) m x
readContextDepth = undefined
readContextStencil :: (MonadIO m, StencilRenderable ds, PixelElementFormat Word32 a) => RenderTargetRegion -> ([a] -> m x) -> ContextT os (ContextFormat c ds) m x
readContextStencil = undefined

writeTextureFromContextColor :: (MonadIO m, Texture t, ColorFormat (TextureFormat t), TextureObjectSpace t ~ os) =>
                                        (Int, Int) -> t -> TextureRegion t -> ContextT os (ContextFormat (TextureFormat t) ds) m ()
writeTextureFromContextColor = undefined

writeTextureFromContextDepth :: (MonadIO m, Texture t, TextureFormat t ~ DepthFormat, TextureObjectSpace t ~ os, DepthRenderable ds) =>
                                        (Int, Int) -> t -> TextureRegion t -> ContextT os (ContextFormat c ds) m ()
writeTextureFromContextDepth = undefined


class PixelElementFormat f a 
instance PixelElementFormat Float Int32
instance PixelElementFormat Float Int16
instance PixelElementFormat Float Int8
instance PixelElementFormat Float Word32
instance PixelElementFormat Float Word16
instance PixelElementFormat Float Word8
instance PixelElementFormat Float Float
instance PixelElementFormat Int32 Int32
instance PixelElementFormat Int32 Int16
instance PixelElementFormat Int32 Int8
instance PixelElementFormat Word32 Word32
instance PixelElementFormat Word32 Word16
instance PixelElementFormat Word32 Word8


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

type BufferPixel f a = BufferColor (Color f a)

usingTexture :: Texture t => Stream fr x a -> (t, Filter, EdgeMode) -> Stream fr x (a, Sampler t fr)
usingTexture =
        let f (a, blockId, sampId, x) = ((a, Sampler sampId), blockId, sampId+1, undefined {- TODO -}, x)
        in \ (Stream s) (_t, _f, _e)  ->         
            let sIO sampId cs = do binding <- getNext
                                   return ()
                                    -- bind texture t to texunit binding and give uniform with name sampId value binding
                                    -- and set f and e to it 
                g (a, blockId, sampId, decl, PrimitiveStreamData x uBinds sBinds) = (a, blockId, sampId, PrimitiveStreamData x uBinds ((decl, sIO sampId):sBinds))
                -- TODO: More StreamData types, eg FragmentStreams
            in Stream $ map (g . f) s

newtype Sampler t fr = Sampler { samplerName :: Int }
















