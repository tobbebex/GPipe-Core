{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Graphics.GPipe.Format where

class BufferFormat f where
    type HostFormat f

class BufferFormat (BufferColorFormat f) => ColorFormat f where
    type BufferColorFormat f 

class BufferFormat (BufferDepthFormat f) => DepthFormat f where
    type BufferDepthFormat f 

class BufferFormat (BufferStencilFormat f) => StencilFormat f where
    type BufferStencilFormat f 

type HostColorFormat f = HostFormat (BufferColorFormat f)
type HostDepthFormat f = HostFormat (BufferDepthFormat f)
type HostStencilFormat f = HostFormat (BufferStencilFormat f)

data ContextFormat c d s = ContextFormat c d s
