-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.FragmentStream
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- A 'Graphics.GPipe.PrimitiveStream.PrimitiveStream' can be rasterized, i.e. chopped up in pixel sized fragments, each of which contains an interpolated value of the primitives vertices, producing
-- a 'FragmentStream'.

-----------------------------------------------------------------------------

module Graphics.GPipe.FragmentStream (
    -- * The data type
    FragmentStream(),
    FragmentInput(..),
    ToFragment(),
    FlatVFloat(..),
    NoPerspectiveVFloat(..),

    -- * Creating FragmentStreams
    rasterize,
    VPos,
    Side(..),
    ViewPort(..),
    DepthRange(..),
    
    -- * Various FragmentStream operations   
    filterFragments,   
    withRasterizedInfo,
    RasterizedInfo(..),
)
where

import Graphics.GPipe.Internal.FragmentStream