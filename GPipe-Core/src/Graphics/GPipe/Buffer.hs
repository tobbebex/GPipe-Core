{-# LANGUAGE TypeFamilies #-}

module Graphics.GPipe.Buffer (
    BufferFormat(type HostFormat, toBuffer),
    Buffer(),
    ToBuffer(),
    B(), B2(..), B3(..), B4(..),
    toB3B1, toB2B2, toB1B3, toB2B1, toB1B2, toB1B1,
    BNormalized(..),
    newBuffer,
    writeBuffer,
    copyBuffer,
    BufferColor
)
where

import Graphics.GPipe.Internal.Buffer 