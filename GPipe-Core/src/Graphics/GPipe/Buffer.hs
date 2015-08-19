{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Buffer
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- Buffers are arrays of data that resides on the graphics hardware. A 'Buffer os b' lives in the object space 'os' and contains elements of type 'b'. The
-- atomic buffer element types are 'B a', 'B2 a', 'B3 a' and 'B4 a' where 'a' is a normal haskell type such as 'Int32' or 'Float'. By creating instances of the
-- type class 'BufferFormat' you may create new composite buffer types. 
-- Buffers in GPipe are used to create vertex arrays, index arrays and uniform blocks and can also be used to copy pixel data to and from textures. They can only be written 
-- from the host (ie normal haskell world) and can not be read back (but textures can).
-----------------------------------------------------------------------------

module Graphics.GPipe.Buffer (
    BufferFormat(type HostFormat, toBuffer),
    Buffer(),
    ToBuffer(),
    B(), B2(), B3(), B4(), BPacked(),
    toB22, toB21, toB12, toB11,
    Normalized(..), 
    newBuffer,
    bufferLength,
    writeBuffer,
    copyBuffer,
    BufferColor
)
where

import Graphics.GPipe.Internal.Buffer 