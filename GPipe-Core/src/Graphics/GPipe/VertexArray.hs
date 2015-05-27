{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, EmptyDataDecls, RankNTypes, ImpredicativeTypes #-}
module Graphics.GPipe.VertexArray where

import Graphics.GPipe.Buffer
import Graphics.GPipe.Frame
import Control.Applicative
import Control.Arrow (arr)

data VertexArray t a = VertexArray  { length :: Int, bArrBFunc:: BInput -> a }

data Instances

newVertexArray :: Frame os f (Buffer os a) (VertexArray x a)
newVertexArray = arr $ \buffer -> VertexArray (bufElementCount buffer) $ bufBElement buffer

instance Functor (VertexArray t) where
    fmap f (VertexArray n g) = VertexArray n (f . g)

zipWith :: (a -> b -> c) -> VertexArray t a -> VertexArray t b -> VertexArray t c 
zipWith h (VertexArray n f) (VertexArray m g) = VertexArray (min n m) (\x -> h (f x) (g x))

take :: Int -> VertexArray t a -> VertexArray t a
take n (VertexArray m f) = VertexArray (min n m) f

drop :: Int -> VertexArray () a -> VertexArray t a
drop n (VertexArray m f) = VertexArray n' g
        where
            n' = max (m - n) 0
            g bIn = f $ bIn { bInSkipElems = bInSkipElems bIn + n'}

replicateEach :: Int -> VertexArray t a -> VertexArray Instances a
replicateEach n (VertexArray m f) = VertexArray (n*m) (\x -> f $ x {bInInstanceDiv = bInInstanceDiv x * n})

instance Applicative (VertexArray t) where
    pure = VertexArray maxBound . const
    (VertexArray n f) <*> (VertexArray m g) = VertexArray (min n m) (\x -> f x (g x))

-- TODO: add zipWithIndex to add gl_VertexId and gl_InstanceId