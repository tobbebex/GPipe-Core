
module Graphics.GPipe.VertexArray where

import Graphics.GPipe.Buffer
import Control.Applicative
import qualified Data.Map as Map

data VertexArray os a = VertexArray (Map.Map Int (Int,Int)) a

toVertexArray :: Buffer os a -> VertexArray os a
toVertexArray (Buffer a b c d _) = VertexArray (Map.singleton a (b,c)) d

genericVertexArray :: HostFormat a -> VertexArray os a
genericVertexArray = undefined 

instance Functor (VertexArray os) where
    fmap f (VertexArray bs a) = VertexArray bs $ f a

instance Applicative (VertexArray os) where
    pure = VertexArray Map.empty
    VertexArray a b <*> VertexArray c d = VertexArray (a `Map.union` c) (b d)







    