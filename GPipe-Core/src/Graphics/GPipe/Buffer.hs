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
-- Buffers are arrays of data that resides on the GPU. A buffer is strongly typed with an immutable size, but it's content is mutable. A buffer lives in
-- an object space and may be shared between contexts.
--
-- Buffers in GPipe are used to store vertices, indices and uniform values and can also be used to copy pixel data to and from textures. They can be written 
-- from the host (ie the normal Haskell world) but cannot be read back (but textures can).
--
-- The atomic buffer element types are @'B' a@, @'B2' a@, @'B3' a@ and @'B4' a@ where @a@ is a normal haskell type such as 'Int32' or 'Float'. By creating instances of the
-- type class 'BufferFormat' you may create new composite buffer types. 
-----------------------------------------------------------------------------

module Graphics.GPipe.Buffer (
    -- * Buffer data type
    Buffer(),
    BufferFormat(type HostFormat, toBuffer),
    ToBuffer(),
    
    -- * Atomic buffer types
    -- | These types represent primitive host values such as 'Float' and 'Int32' in a buffer. @'B2' a@, @'B3' a@ and @'B4' a@ represent vectors of 2, 3 and 4 values of host type @a@.
    --   You cannot do anything special with values of these lifted types (like add two @'B' 'Float'@s), only convert it into something useful later, e.g. in a 'PrimitiveStream'.
    --
    --   Since vertex arrays have to be 4 byte aligned, only combinations that add up to a multiple of 4 byte is provided (except for some instances of @'B3' a@ which will be automatically padded when necessary).       
    B(), B2(), B3(), B4(), BPacked(),
    Normalized(..), 

    -- * Operating on Buffers
    newBuffer,
    bufferLength,
    writeBuffer,
    copyBuffer,
    BufferStartPos,        
    
    -- * Buffer colors
    BufferColor,
)
where

import Graphics.GPipe.Internal.Buffer 