module Graphics.GPipe.PrimitiveArray (
    VertexArray(),
    Instances,
    newVertexArray,
    zipVertices,
    takeVertices,
    dropVertices,
    replicateEach,
    
    IndexArray(),    
    IndexFormat,
    newIndexArray,
    takeIndices,
    dropIndices,
    
    PrimitiveArray(),   
    PrimitiveTopology(),
    Triangles(..),
    Lines(..),
    Points(..),    
    toPrimitiveArray,
    toPrimitiveArrayIndexed,
    toPrimitiveArrayInstanced,
    toPrimitiveArrayIndexedInstanced,
)
where

import Graphics.GPipe.Internal.PrimitiveArray
