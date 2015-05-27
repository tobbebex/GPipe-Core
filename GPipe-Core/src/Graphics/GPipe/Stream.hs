{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}
module Graphics.GPipe.Stream where
{--

import Data.StableFunctor
import Graphics.GPipe.Shader
import Control.Monad.Trans.State
import qualified Data.IntSet as IntSet

-- | Filter mode used in sampler state
data Filter = Nearest | Linear
            deriving (Eq,Ord,Bounded,Enum,Show)
-- | EData.IntSettsed in sampler state
data EdgeMode = Wrap | Mirror | Clamp
              deriving (Eq,Ord,Bounded,Enum,Show)
              
              
type SamplerSpec = (Int, Filter, EdgeMode)


type UniformStreamBufferBindings = [(ShaderGlobDeclM (), CompiledShader -> StateT Int IO ())] -- Going backwards, ie index 0 is last
type SamplerStreamBindings = UniformStreamBufferBindings -- Going backwards, ie index 0 is last

selectReversedIndexed :: IntSet.IntSet -> [a] -> [a]
selectReversedIndexed s xs = select (IntSet.toAscList s) (reverse xs) 0 
        where
            select (xxs@(x:xs)) (y:ys) p | x == p    = y : select xs ys (p+1)
                                         | otherwise = select xxs ys (p+1)
            select [] _ p                            = []
            select _ [] p                            = error "selectReversedIndexed: Tried selecting elements outside list"                                    

type NextUniformBlock = Int 
type NextSampler = Int 
type UsedVaryingIndices = IntSet.IntSet  
data PrimitiveStreamData = PrimitiveStreamData (InputNameToIndex -> (ShaderGlobDeclM (), IO())) UniformStreamBufferBindings SamplerStreamBindings |
                           FragmentStreamData (UsedVaryingIndices -> (ShaderGlobDeclM (), IO (ShaderSource, CompiledShader -> IO ()))) UniformStreamBufferBindings SamplerStreamBindings 
--data Stream t a = Stream [(StableFunctor a, NextUniformBlock, NextSampler)] [PrimitiveStreamData] 

{--
data Stream t a = Stream [(a, PrimitiveStreamData)]

instance Functor (Stream t) where
        fmap f (Stream xs) = Stream (map (\(a, x)-> (f a, x)) xs)
--}


data Geometries
data Fragments

--}
