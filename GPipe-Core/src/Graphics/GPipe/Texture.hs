-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Texture
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- A texture is a spatially arranged map of pixels that resides on the GPU. A texture can be a 1D, 2D or 3D image, or an array of several same sized 1D or 2D images, or a cube map with six square images.
-- A texture may also have several levels of detail, in decreasing size.
--
-- A texture has a strongly typed format and immutable size and number of levels, but its content is mutable. A texture lives in
-- an object space and may be shared between contexts.
--
-- The main purpose for a texture is to be sampled in a 'Shader', by turning it into a sampler object (See 'Graphics.Gpipe.Sampler.Sampler1D' and friends). A texture may
-- also be used as a render target into which 'Graphics.Gpipe.FragmentStream.FragmentStream's are drawn, thus generating the texture on the GPU. To that end, a texture may not only be written
-- from the host (i.e. the normal Haskell world) but also read back.
-----------------------------------------------------------------------------

module Graphics.GPipe.Texture (
    -- * Texture data types
    Texture1D(), Texture1DArray(), Texture2D(), Texture2DArray(), Texture3D(), TextureCube(), 
    CubeSide(..),

    -- * Creating textures
    -- | All of these functions take a format and a texture size in the dimensionality of the specific texture type. It also takes a 'MaxLevels' parameter to limit number of
    --   levels to be created. The maximum number of levels created is always constrained by the texture size, so using 'maxBound' will effectively create a texture with
    --   maximum possible number of levels.  
    newTexture1D,
    newTexture1DArray,
    newTexture2D,
    newTexture2DArray,
    newTexture3D,
    newTextureCube,

    -- * Texture properties
    -- | The following functions retrieve number of levels a texture has. This number is always smaller or equal to the 'MaxLevels' parameter provided when the texture was created.
    texture1DLevels, 
    texture1DArrayLevels, 
    texture2DLevels, 
    texture2DArrayLevels, 
    texture3DLevels, 
    textureCubeLevels, 
    -- | The following functions retrieve a list of texture sizes for each level of detail, from the largest to the smallest, where the first has the size as defined by the 'newTextureX' call. 
    texture1DSizes, 
    texture1DArraySizes, 
    texture2DSizes, 
    texture2DArraySizes, 
    texture3DSizes, 
    textureCubeSizes, 

    -- * Writing texture data
    -- | The following functions write the texture data from the host (i.e. the normal Haskell world), using a compatible 'Graphics.GPipe.Buffer.HostFormat' of the texture's format, see 'Graphics.GPipe.Buffer.BufferColor'.  
    writeTexture1D,
    writeTexture1DArray,
    writeTexture2D,
    writeTexture2DArray,
    writeTexture3D,
    writeTextureCube,    
    -- | The following functions write the texture data using values in a 'Graphics.GPipe.Buffer.Buffer' with a format compatible with the texture's format, see 'Graphics.GPipe.Buffer.BufferColor'.
    writeTexture1DFromBuffer,
    writeTexture1DArrayFromBuffer,
    writeTexture2DFromBuffer,
    writeTexture2DArrayFromBuffer,
    writeTexture3DFromBuffer,
    writeTextureCubeFromBuffer,
    -- | The following functions uses the level of detail 0 to generate all other levels of detail. The common pattern is to call this directly after a call to
    --   'writeTextureX' where parameter 'Level' is 0.    
    generateTexture1DMipmap,
    generateTexture1DArrayMipmap,
    generateTexture2DMipmap,
    generateTexture2DArrayMipmap,
    generateTexture3DMipmap,
    generateTextureCubeMipmap,

    -- * Reading texture data
    -- | Read textures to the host (i.e. the normal Haskell world), using a compatible 'Graphics.GPipe.Buffer.HostFormat' of the texture's format, see 'Graphics.GPipe.Buffer.BufferColor'.
    --   This works like any 'fold' like function.
    readTexture1D,
    readTexture1DArray,
    readTexture2D,
    readTexture2DArray,
    readTexture3D,
    readTextureCube,
    
    -- | The following read textures into a 'Graphics.GPipe.Buffer.Buffer' with a format compatible with the texture's format, see 'Graphics.GPipe.Buffer.BufferColor'.
    readTexture1DToBuffer,
    readTexture1DArrayToBuffer,
    readTexture2DToBuffer,
    readTexture2DArrayToBuffer,
    readTexture3DToBuffer,
    readTextureCubeToBuffer,
    
    -- * Type synonyms
    MaxLevels, Level,
    Size1, Size2, Size3,
    StartPos1, StartPos2, StartPos3, 
)
where

import Graphics.GPipe.Internal.Texture
