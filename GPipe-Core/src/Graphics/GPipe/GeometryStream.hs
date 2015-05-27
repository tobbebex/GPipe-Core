{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}

module Graphics.GPipe.GeometryStream where
{-

import Prelude hiding (length, id, (.))
import Graphics.GPipe.Buffer
import Graphics.GPipe.Shader
import Graphics.GPipe.Stream
import Graphics.GPipe.VertexArray hiding (length)
import qualified Graphics.GPipe.VertexArray as VertexArray  (length)
import Graphics.GPipe.IndexArray hiding (length)
import qualified Graphics.GPipe.IndexArray as IndexArray (length)
import Control.Category
import Control.Arrow
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import qualified Data.IntMap as Map
import Data.Foldable (forM_)



data VertexList p a = VList

nullVertexList :: PrimitiveTopology p => p -> VertexList p a 
nullVertexList = undefined

consVertex :: PrimitiveTopology p => a -> VertexList p a -> VertexList p a
consVertex = undefined 

toGeometryStream :: PrimitiveTopology p => Stream fr p a -> Stream fr Geometries (Geometry p a)
toGeometryStream = undefined
-}