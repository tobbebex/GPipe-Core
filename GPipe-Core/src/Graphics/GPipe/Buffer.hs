module Graphics.GPipe.Buffer (
    BufferFormat(..),
    Buffer(),
    ToBuffer(),
    B(), B2(..), B3(..), B4(..),
    toB3B1, toB2B2, toB1B3, toB2B1, toB1B2, toB1B1,
    BNormalized(..),
    newBuffer,
    writeBuffer,
    copyBuffer,
    BufferTextureFormat(),
    BufferColor,
    BFloat, BInt32, BInt16, BInt8, BWord32, BWord16, BWord8, 
    BInt32Norm, BInt16Norm, BInt8Norm, BWord32Norm, BWord16Norm, BWord8Norm    
)
where

import Graphics.GPipe.Internal.Buffer 