{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}

module Graphics.GPipe.VertexStream where

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

class VertexInput a where
    type VertexFormat a
    toVertex :: ToVertex a (VertexFormat a)  

data ToVertex a b = ToVertex
    (Kleisli (State Int) a b)
    (Kleisli (Writer [Int -> (IO (), SType)]) a b) -- order of this list is attribname, parameter to func in list is index
instance Category ToVertex where
    id = ToVertex id id
    ToVertex a b . ToVertex x y = ToVertex (a.x) (b.y)  
instance Arrow ToVertex where
    arr f = ToVertex (arr f) (arr f)
    first (ToVertex a b) = ToVertex (first a) (first b)

instance VertexInput BFloat where
    type VertexFormat BFloat = VFloat
    toVertex = ToVertex (Kleisli $ makeShaderInput id) (Kleisli $ setupF glFLOAT)

instance VertexInput BInt32Norm where
    type VertexFormat BInt32Norm = VFloat
    toVertex = ToVertex (Kleisli $ makeShaderInput id) (Kleisli $ setupF glINT32 . (\(BNormalized b)->b)) where

setupF :: Int -> B a -> Writer [Int -> (IO (), SType)] b
setupF t b = do tell $ (:[]) $ \index -> (
                        do glBindBuffer (bName b) glVERTEX_ARRAY
                           glAttribArray index 1 t False (bName b) (bStride b) (bStride b * bSkipElems b + bOffset b)
                       , STypeFloat)
                return undefined

makeShaderInput :: Monad m => (S c a -> b) -> t -> StateT Int m b
makeShaderInput f _ =
            do attrib <- getNext
               return $ f $ S $ useInput attrib 

applyMapValueToKeyedList :: [a -> x] -> Map.IntMap a -> [(Int, x)]
applyMapValueToKeyedList ls m = 
    zipMap 0 assocs ls
        where assocs = Map.assocs m 
              zipMap n ((i,x):xs) (f:fs) = let rest = zipMap (n+1) xs fs
                                           in if i == n then (i, f x) : rest else rest
              zipMap _ [] _ = []   
              zipMap _ _ [] = error "applyMapValueToKeyedList: Map key not present in list"   
        
makeDrawDeclIO drawcall = 
    foldr f (return (), drawcall)
        where f (attrib, (io, stype)) (decl, ios)= (gDeclInput stype attrib >> decl, io >> ios)

toPrimitiveStream :: forall fr a p. (VertexInput a, PrimitiveTopology p) 
    => p 
    -> VertexArray fr () a 
    -> Stream fr p (VertexFormat a)
toPrimitiveStream =
    let ToVertex (Kleisli makeShader) (Kleisli ioF) = toVertex :: ToVertex a (VertexFormat a)
        a = evalState (makeShader (undefined :: a)) 0
    in \p ba -> let declIos = execWriter $ ioF $ bArrBFunc ba $ BInput 0 0       
                    drawcall = glDrawArrays (toGLtopology p) 0 (VertexArray.length ba)
                    drawioF = makeDrawDeclIO drawcall . applyMapValueToKeyedList declIos
                in Stream [(a, 0, PrimitiveStreamData drawioF [])]

toIndexedPrimitiveStream :: forall fr i a p. (IndexFormat i, VertexInput a, PrimitiveTopology p) 
    => p
    -> VertexArray fr () a 
    -> IndexArray fr i 
    -> Stream fr p (VertexFormat a)
toIndexedPrimitiveStream =
    let ToVertex (Kleisli makeShader) (Kleisli ioF) = toVertex :: ToVertex a (VertexFormat a)
        a = evalState (makeShader (undefined :: a)) 0
    in \p ba iba -> 
                let declIos = execWriter $ ioF $ bArrBFunc ba $ BInput 0 0
                    drawcall = do forM_ (restart iba) glRestartIndex
                                  glBindBuffer (iArrName iba) glELEMENT_ARRAY
                                  glDrawElements (toGLtopology p) (IndexArray.length iba) (indexType iba) (offset iba)
                    drawioF = makeDrawDeclIO drawcall . applyMapValueToKeyedList declIos
                in Stream [(a, 0, PrimitiveStreamData drawioF [])]

toInstancedPrimitiveStream :: forall fr a b c p. (VertexInput c, PrimitiveTopology p) 
    => p
    -> VertexArray fr () a 
    -> (a -> b -> c) 
    -> VertexArray fr Instances b 
    -> Stream fr p (VertexFormat c)
toInstancedPrimitiveStream =
    let ToVertex (Kleisli makeShader) (Kleisli ioF) = toVertex :: ToVertex c (VertexFormat c)
        c = evalState (makeShader (undefined :: c)) 0
    in \p va f ina -> 
                let
                    declIos = execWriter $ ioF $ f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput 0 1)
                    drawcall = glDrawArraysInstanced (toGLtopology p) 0 (VertexArray.length va) (VertexArray.length ina)
                    drawioF = makeDrawDeclIO drawcall . applyMapValueToKeyedList declIos
                in Stream [(c, 0, PrimitiveStreamData drawioF [])]
    
toInstancedIndexedPrimitiveStream :: forall fr i a b c p .(IndexFormat i, VertexInput c, PrimitiveTopology p) 
    => p
    -> VertexArray fr () a 
    -> IndexArray fr i 
    -> (a -> b -> c) 
    -> VertexArray fr Instances b 
    -> Stream fr p (VertexFormat c)
toInstancedIndexedPrimitiveStream =
    let ToVertex (Kleisli makeShader) (Kleisli ioF) = toVertex :: ToVertex c (VertexFormat c)
        c = evalState (makeShader (undefined :: c)) 0
    in \p va ia f ina -> 
                let
                    declIos = execWriter $ ioF $ f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput 0 1)
                    drawcall = do forM_ (restart ia) glRestartIndex
                                  glBindBuffer (iArrName ia) glELEMENT_ARRAY
                                  glDrawElementsInstanced (toGLtopology p) (IndexArray.length ia) (indexType ia) (offset ia) (VertexArray.length ina)
                    drawioF = makeDrawDeclIO drawcall . applyMapValueToKeyedList declIos
                in Stream [(c, 0, PrimitiveStreamData drawioF [])]


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
