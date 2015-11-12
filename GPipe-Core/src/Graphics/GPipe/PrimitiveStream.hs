-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.PrimitiveStream
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- A 'Graphics.GPipe.PrimitiveArray.PrimitiveArray' can be turned into a 'PrimitiveStream' in a 'Graphics.GPipe.Shader.Shader', in order to operate on the vertices of it and ultimately rasterize it into
-- a 'Graphics.GPipe.FragmentStream.FragmentStream'.

-----------------------------------------------------------------------------

module Graphics.GPipe.PrimitiveStream (
    -- * The data type
    PrimitiveStream(),
    VertexInput(..),
    ToVertex(),

    -- * Creating PrimitiveStreams
    toPrimitiveStream,
   
    -- * Various PrimitiveStream operations   
    withInputIndices,   
    InputIndices(..),
    withPointSize,
    PointSize
)
where

import Graphics.GPipe.Internal.PrimitiveStream 