{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, EmptyDataDecls, RankNTypes, ImpredicativeTypes #-}
module Graphics.GPipe.VertexArray where

import Graphics.GPipe.Buffer
import Graphics.GPipe.Frame
import Control.Applicative

data VertexArray fr t a = VertexArray  { length :: Int, bArrBFunc:: BInput -> a }

data Instances

newVertexArray :: Buffer os a -> Frame fr os f (VertexArray fr x a)
newVertexArray buffer = return $ VertexArray (bufElementCount buffer) $ bufBElement buffer

instance Functor (VertexArray fr t) where
    fmap f (VertexArray n g) = VertexArray n (f . g)

zipWith :: (a -> b -> c) -> VertexArray fr t a -> VertexArray fr t b -> VertexArray fr t c 
zipWith h (VertexArray n f) (VertexArray m g) = VertexArray (min n m) (\x -> h (f x) (g x))

take :: Int -> VertexArray fr t a -> VertexArray fr t a
take n (VertexArray m f) = VertexArray (min n m) f

drop :: Int -> VertexArray fr () a -> VertexArray fr t a
drop n (VertexArray m f) = VertexArray n' g
        where
            n' = max (m - n) 0
            g bIn = f $ bIn { bInSkipElems = bInSkipElems bIn + n'}

replicateEach :: Int -> VertexArray fr t a -> VertexArray fr Instances a
replicateEach n (VertexArray m f) = VertexArray (n*m) (\x -> f $ x {bInInstanceDiv = bInInstanceDiv x * n})

instance Applicative (VertexArray fr t) where
    pure = VertexArray maxBound . const
    (VertexArray n f) <*> (VertexArray m g) = VertexArray (min n m) (\x -> f x (g x))

