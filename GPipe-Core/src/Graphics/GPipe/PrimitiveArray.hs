-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.PrimitiveArray
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | PrimitiveArrays (aka VAOs in OpenGl) are the main input to compiled shaders. A primitive array is created from one or more zipped vertex arrays.
--   A primitive array may also be instanced, using one or more zipped vertex arrays as instance arrays. And lastly, an index array may also be used to pick vertices for the primitive array.   
--
--   Any possible combination of interleaved or non-interleaved vertex buffers may be used, for example:
--
--  Buffer @a@ = @{(A,B),(A,B),(A,B)...}@
--  Buffer @b@ = @{(X,Y,Z),(X,Y,Z),(X,Y,Z),...}@
--
--  @
--   do
--      aArr <- newVertexArray a        
--      bArr <- newVertexArray b 
--      let primArr = toPrimitiveArray TriangleList (zipVertices (\(a,b) y -> (a,b,y)) aArr (fmap (\(_,y,_) -> y) bArr))
--  @
--  
--  will create a primitive array @primArr@ with this layout: @{(A,B,Y),(A,B,Y),(A,B,Y)...}@
-----------------------------------------------------------------------------
module Graphics.GPipe.PrimitiveArray (          
    -- * Vertex arrays
    VertexArray(),
    Instances,
    newVertexArray,
    vertexArrayLength,
    zipVertices,
    Combine,
    takeVertices,
    dropVertices,
    replicateEach,
    
    -- * Index arrays
    IndexArray(),    
    newIndexArray,
    IndexFormat,
    indexArrayLength,
    takeIndices,
    dropIndices,
   
    -- * Primitive arrays
    PrimitiveArray(),   
    PrimitiveTopology(..),
    Triangles,
    Lines,
    Points,    
    toPrimitiveArray,
    toPrimitiveArrayIndexed,
    toPrimitiveArrayInstanced,
    toPrimitiveArrayIndexedInstanced,

    -- * Operations on buffer values
    -- | You may split up a @B4 a@, @B3 a@ and @B2 a@ value into its components, if the parts are representable buffer types (e.g. due to alignment, you may for instance not split a @B4 Word8@).
    --   Note that there are no functions to combine smaller parts together again. 
    toB22, toB3, toB21, toB12, toB11,
)
where

import Graphics.GPipe.Internal.PrimitiveArray
import Graphics.GPipe.Internal.Buffer
