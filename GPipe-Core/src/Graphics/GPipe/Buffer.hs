{-# LANGUAGE TypeFamilies #-}

module Graphics.GPipe.Buffer (
    BufferFormat(type HostFormat, toBuffer),
    Buffer(),
    ToBuffer(),
    B(), B2(), B3(), B4(), BPacked(),
    Normalized(..), 
    newBuffer,
    writeBuffer,
    copyBuffer,
    BufferColor
)
where

import Graphics.GPipe.Internal.Buffer 