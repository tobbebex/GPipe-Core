module Graphics.GPipe.PrimitiveArray (
    VertexArray(),
    Instances,
    newVertexArray,
    vertexArrayLength,
    zipVertices,
    takeVertices,
    dropVertices,
    replicateEach,
    
    IndexArray(),    
    IndexFormat,
    newIndexArray,
    indexArrayLength,
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
