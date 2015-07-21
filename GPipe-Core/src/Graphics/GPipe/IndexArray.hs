{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
module Graphics.GPipe.IndexArray (
    IndexArray(..), 
    IndexFormat(),
    newIndexArray,
    take,
    drop
) where

import Graphics.GPipe.Buffer
import Graphics.GPipe.Shader
import Prelude hiding (length, take, drop)
import Foreign.C.Types
import Data.IORef

class BufferFormat a => IndexFormat a where
    indexToInt :: a -> HostFormat a -> Int
    glType :: a -> Int
        
instance IndexFormat BWord32 where
    indexToInt _ = fromIntegral  
    glType _ = glINT
instance IndexFormat BWord16 where
    indexToInt _ = fromIntegral  
    glType _ = glSHORT
instance IndexFormat BWord8 where
    indexToInt _ = fromIntegral    
    glType _ = glBYTE
    
data IndexArray = IndexArray { iArrName :: IORef CUInt, length:: Int, offset:: Int, restart:: Maybe Int, indexType :: Int } 
newIndexArray :: forall os f a. IndexFormat a => Buffer os a -> Maybe (HostFormat a) -> Render os f IndexArray
newIndexArray buf r = let a = undefined :: a in Render $ return $ IndexArray (bufName buf) (bufElementCount buf) 0 (fmap (indexToInt a) r) (glType a) 
 
take :: Int -> IndexArray -> IndexArray
take n i = i { length = min n (length i) }

drop :: Int -> IndexArray -> IndexArray
drop n i = i { length = max (l - n) 0, offset = offset i + n } where l = length i
 
glINT :: Int
glINT = undefined
glSHORT :: Int
glSHORT = undefined
glBYTE :: Int
glBYTE = undefined