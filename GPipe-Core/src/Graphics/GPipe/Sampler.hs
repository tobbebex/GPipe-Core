-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Sampler
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- A sampler is a value from which filtered color samples may be taken inside a shader. A sampler is created from a texture and some sampling parameters. There also exist
-- 'Shadow' samplers that doesnt return a sampled color value, but instead compare a reference value to the texture value.  
-----------------------------------------------------------------------------

module Graphics.GPipe.Sampler (
    -- * Sampler data types
    Sampler1D(), Sampler1DArray(), Sampler2D(), Sampler2DArray(), Sampler3D(), SamplerCube(),
    Shadow,

    -- * Creating samplers
    -- | These 'Shader' actions all take a texture and some filtering and edge options from the shader environment, and return a sampler. 
    newSampler1D, newSampler1DArray, newSampler2D, newSampler2DArray, newSampler3D, newSamplerCube,
    newSampler1DShadow, newSampler1DArrayShadow, newSampler2DShadow, newSampler2DArrayShadow, newSamplerCubeShadow,

    -- * Types for specifying sampler filter and edge mode
    Filter(..),
    EdgeMode(..),
    EdgeMode2, 
    EdgeMode3,
    BorderColor,
    Anisotropy,
    MinFilter,
    MagFilter,
    LodFilter,
    SamplerFilter(..),
    ComparisonFunction(..),
    
    -- * Sampler properties
    -- | These functions can be used to get the size of a sampler inside the shader.
    sampler1DSize, sampler1DArraySize, sampler2DSize, sampler2DArraySize, sampler3DSize, samplerCubeSize,
   
    -- * Sampling functions
    -- | These functions sample a sampler using its filter and edge mode. Besides the sampler and the coordinate, many additional parameters are provided to enable many
    --   different variations of sampling. In most cases when sampling in a 'FragmentStream', use 'Nothing' or 'SampleAuto' to get what you need.
    --   Float coordinates are given with components in range [0,1]. 
    sample1D, sample1DArray, sample2D, sample2DArray, sample3D, sampleCube,
    -- | The following functions sample a shadow sampler using a 'ReferenceValue' to compare the texture values to. The returned value is a @S x Float@ value in the range [0,1] where 0 means false, 1 means true and any value in between is a fuzzy boolean value indicating how many adjacent texels compared true and how many compared false.
    sample1DShadow, sample1DArrayShadow, sample2DShadow, sample2DArrayShadow, sampleCubeShadow, 
    -- | The following functions retrieve a texel value from a samplers texture without using any filtering. Coordinates for these functions are integer texel indices, and not normalized coordinates.
    texelFetch1D, texelFetch1DArray, texelFetch2D, texelFetch2DArray, texelFetch3D,   

    -- * Sample parameter types
    SampleLod(..), 
    SampleLod1, 
    SampleLod2, 
    SampleLod3, 
    SampleLod'(..), 
    SampleLod2', 
    SampleLod3',
    fromLod',
    SampleProj,
    SampleOffset1,
    SampleOffset2,
    SampleOffset3, 
    ReferenceValue,
    ColorSample,
)
where

import Graphics.GPipe.Internal.Texture

