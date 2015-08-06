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