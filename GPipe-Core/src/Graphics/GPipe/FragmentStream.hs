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
-- A 'PrimitiveStream' can be rasterized, that is chopped up in pixel sized fragments, each of which contains an interpolated value of the primitives vertices, producing
-- a 'FragmentStream'.

-----------------------------------------------------------------------------

module Graphics.GPipe.FragmentStream (
    FragmentStream(),
    FragmentInput(..),
    ToFragment(),

    rasterize,
    VPos,
    Side(..),
    ViewPort(..),
    DepthRange(..),
    
    Flat(..),
    NoPerspective(..),
    
    filterFragments,   
    withRasterizedInfo,
    RasterizedInfo(..),
)
where

import Graphics.GPipe.Internal.FragmentStream