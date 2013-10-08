{-# LANGUAGE TypeFamilies #-}
module Graphics.GPipe.FragmentStream where

import Graphics.GPipe.Shader
import Graphics.GPipe.Stream
import Control.Category
import Control.Arrow
import Control.Monad.Trans.State

class FragmentInput a where
    type FragmentFormat a
    toFragment :: ToFragment a (FragmentFormat a)  

data ToFragment a b = ToFragment (Kleisli (State Int) a b)

type VertexPosition = (VFloat, VFloat, VFloat, VFloat) 

data Side = Front | Back | FrontAndBack

rasterize :: (PrimitiveTopology p, FragmentInput a)
          => Side 
          -> Stream fr p (VertexPosition, a)
          -> Stream fr Fragment (FBool, FragmentFormat a)
rasterize x = undefined

