{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs, TypeSynonymInstances #-}

module Graphics.GPipe.Uniform where

import Graphics.GPipe.Buffer 
import Graphics.GPipe.Shader

class BufferFormat (UniformBufferFormat a) => Uniform a where
    type UniformBufferFormat a
    --TODO: Add if needed:  uniform :: Shader os (HostFormat (UniformBufferFormat a)) a
    loadUniform :: Shader os (UniformBufferFormat a) a

data UniformBuffer os a where
    UniformBuffer :: (b -> a) -> Buffer os b -> UniformBuffer os a 

toUniformBuffer :: Buffer os a -> UniformBuffer os a
toUniformBuffer = UniformBuffer id

uniformBuffer :: Uniform a => Shader os (UniformBuffer os (UniformBufferFormat a)) a
uniformBuffer = undefined

instance Functor (UniformBuffer os) where
    fmap f (UniformBuffer g b) = UniformBuffer (f.g) b
    
instance Uniform VFloat where
    type UniformBufferFormat VFloat = BFloat
