{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving #-}

module Graphics.GPipe.VertexStream where

import Prelude hiding (length, id, (.))
import Graphics.GPipe.Buffer
import Graphics.GPipe.Shader
import Graphics.GPipe.Frame
import Graphics.GPipe.FrameCompiler
import Graphics.GPipe.VertexArray hiding (length)
import qualified Graphics.GPipe.VertexArray as VertexArray  (length)
import Graphics.GPipe.IndexArray hiding (length)
import qualified Graphics.GPipe.IndexArray as IndexArray (length)
import Control.Category
import Control.Arrow
import Data.Foldable (forM_)
import Data.Monoid (Monoid)

newtype VertexStream t a = VertexStream [(a, VertexStreamData)] deriving Monoid

instance Functor (VertexStream t) where
        fmap f (VertexStream xs) = VertexStream $ map (first f) xs

class PrimitiveTopology p where
    data Geometry p :: * -> *
    toGLtopology :: p -> Int
    makeGeometry :: [a] -> Geometry p a  
   
data Triangles = TriangleStrip | TriangleList
data TrianglesWithAdjacency = TriangleStripWithAdjacency
data Lines = LineStrip | LineList
data LinesWithAdjacency = LinesWithAdjacencyList | LinesWithAdjacencyStrip   
data Points = PointList

instance PrimitiveTopology Triangles where
    data Geometry Triangles a = Triangle a a a
instance PrimitiveTopology TrianglesWithAdjacency where
    data Geometry TrianglesWithAdjacency a = TriangleWithAdjacency a a a a a a
instance PrimitiveTopology Lines where
    data Geometry Lines a = Line a a
instance PrimitiveTopology LinesWithAdjacency where
    data Geometry LinesWithAdjacency a = LineWithAdjacency a a a a
instance PrimitiveTopology Points where
    data Geometry Points a = Point a



class VertexInput a where
    type VertexFormat a
    toVertex :: ToVertex a (VertexFormat a)  

newtype ToVertex a b = ToVertex (IntFrame a b) deriving (Category, Arrow)

   
instance VertexInput BFloat where
    type VertexFormat BFloat = VFloat
    toVertex = ToVertex $ dynInStatOut $ do 
                                 n <- getName
                                 return (S $ useVInput STypeFloat n
                                         ,\b -> do setupForName n $ \ _ _ _ -> glBindBuffer (bName b) glVERTEX_ARRAY
                                                   doForName n $ \ _ ix _ -> glAttribArray ix 1 glFLOAT False (bName b) (bStride b) (bStride b * bSkipElems b + bOffset b)
                                         )
instance VertexInput BInt32Norm where
    type VertexFormat BInt32Norm = VFloat
    toVertex = ToVertex $ dynInStatOut $ do 
                                 n <- getName
                                 return (S $ useVInput STypeFloat n 
                                        ,\(BNormalized b) -> do
                                                setupForName n $ \ _ _ _ -> glBindBuffer (bName b) glVERTEX_ARRAY
                                                doForName n $ \ _ ix _ -> glAttribArray ix 1 glINT32 False (bName b) (bStride b) (bStride b * bSkipElems b + bOffset b)
                                        )
                                                               
toPrimitiveStream :: forall os f a p. (VertexInput a, PrimitiveTopology p) 
    => Frame os f (p, VertexArray () a) (VertexStream p (VertexFormat a))
toPrimitiveStream = IntFrame $ proc d@(_, ba) -> do
                        b <- iFrame -< bArrBFunc ba (BInput 0 0)
                        name <- drawcall -< d
                        returnA -< VertexStream [(b, VertexStreamData  name)]
    where 
        ToVertex iFrame = toVertex :: ToVertex a (VertexFormat a)
        drawcall = dynInStatOut $ do n <- getName
                                     return (n,
                                        \(p, ba) ->
                                            doForName n $ \ _ _ _ -> glDrawArrays (toGLtopology p) 0 (VertexArray.length ba)
                                        )
                
toIndexedPrimitiveStream :: forall os f i a p. (IndexFormat i, VertexInput a, PrimitiveTopology p) 
    => Frame os f (p, VertexArray () a, IndexArray i) (VertexStream p (VertexFormat a))
toIndexedPrimitiveStream = IntFrame $ proc (p, ba, iba) -> do
                        b <- iFrame -< bArrBFunc ba (BInput 0 0)
                        name <- drawcall -< (p, iba)
                        returnA -< VertexStream [(b, VertexStreamData  name)]
    where 
        ToVertex iFrame = toVertex :: ToVertex a (VertexFormat a)
        drawcall = dynInStatOut $ do n <- getName
                                     return (n,
                                        \(p, iba) ->
                                            doForName n $ \ _ _ _ -> do 
                                              forM_ (restart iba) glRestartIndex
                                              glBindBuffer (iArrName iba) glELEMENT_ARRAY
                                              glDrawElements (toGLtopology p) (IndexArray.length iba) (indexType iba) (offset iba)
                                        )


toInstancedPrimitiveStream :: forall os f a b c p. (VertexInput c, PrimitiveTopology p) 
    => Frame os f (p, VertexArray () a, a -> b -> c, VertexArray Instances b) (VertexStream p (VertexFormat c))
toInstancedPrimitiveStream = IntFrame $ proc (p, va, f, ina) -> do
                        b <- iFrame -< f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput 0 1)
                        name <- drawcall -< (p, va, ina)
                        returnA -< VertexStream [(b, VertexStreamData  name)]
    where 
        ToVertex iFrame = toVertex :: ToVertex c (VertexFormat c)
        drawcall = dynInStatOut $ do n <- getName
                                     return (n,
                                        \(p, va, ina) ->
                                            doForName n $ \ _ _ _ -> glDrawArraysInstanced (toGLtopology p) 0 (VertexArray.length va) (VertexArray.length ina)
                                        )

   
toInstancedIndexedPrimitiveStream :: forall os f i a b c p .(IndexFormat i, VertexInput c, PrimitiveTopology p) 
    => Frame os f (p, VertexArray () a, IndexArray i, a -> b -> c, VertexArray Instances b) (VertexStream p (VertexFormat c))
toInstancedIndexedPrimitiveStream = IntFrame $ proc (p, va, ia, f, ina) -> do
                        b <- iFrame -< f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput 0 1)
                        name <- drawcall -< (p, ia, ina)
                        returnA -< VertexStream [(b, VertexStreamData  name)]
    where 
        ToVertex iFrame = toVertex :: ToVertex c (VertexFormat c)
        drawcall = dynInStatOut $ do n <- getName
                                     return (n,
                                        \(p, ia, ina) ->
                                            doForName n $ \ _ _ _ -> do 
                                              forM_ (restart ia) glRestartIndex
                                              glBindBuffer (iArrName ia) glELEMENT_ARRAY
                                              glDrawElementsInstanced (toGLtopology p) (IndexArray.length ia) (indexType ia) (offset ia) (VertexArray.length ina)
                                        )


glDrawArrays :: Int -> Int -> Int -> IO ()
glDrawArrays = undefined

glDrawArraysInstanced :: Int -> Int -> Int -> Int -> IO ()
glDrawArraysInstanced = undefined

glDrawElements :: Int -> Int -> Int -> Int -> IO ()
glDrawElements = undefined

glDrawElementsInstanced :: Int -> Int -> Int -> Int -> Int -> IO ()
glDrawElementsInstanced = undefined
 
glRestartIndex :: Int -> IO ()
glRestartIndex = undefined


glBindBuffer :: Int -> Int -> IO ()
glBindBuffer = undefined                                

glAttribArray :: Int -> Int -> Int -> Bool -> Int -> Int -> Int -> IO () 
glAttribArray = undefined                               

glINT32 :: Int                                      
glINT32 = 0

glFLOAT :: Int
glFLOAT = 1

glVERTEX_ARRAY :: Int
glVERTEX_ARRAY = 1

glELEMENT_ARRAY :: Int
glELEMENT_ARRAY = 1
