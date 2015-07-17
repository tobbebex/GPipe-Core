{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes #-}
module Main where

import Data.Boolean
import Graphics.GPipe.Context
import Graphics.GPipe.Buffer
import Graphics.GPipe.Format
import Graphics.GPipe.VertexArray as VertexArray
import Graphics.GPipe.PrimitiveArray
import Graphics.GPipe.Shader
import Graphics.GPipe.PrimitiveStream
import Graphics.GPipe.FragmentStream
import Graphics.GPipe.FrameBuffer
import Graphics.GPipe.Expr
import Graphics.GPipe.Uniform
import Control.Category (Category(..))
import Prelude hiding ((.), id, length)
import System.Mem.StableName (makeStableName, hashStableName)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Graphics.GPipe.Texture
import Data.Vec


debugContext :: ContextFactory c ds 
debugContext f = return $ ContextHandle 
                (error "newSharedContext not supported")
                (putStrLn "sync:" >>)
                (putStrLn "async:" >>)
                (putStrLn "swap")
                (return (1,1))
                (putStrLn "Delete context")
                
main :: IO ()
main = do runContextT debugContext (ContextFormatColorDepthStencil RGB8 (DepthStencilFormat Depth16 Stencil8)) $ myProg >> myProg1
          putStrLn "Finished!"

myProg1 = do 
            (myVertices1 :: Buffer os BFloat) <- newBuffer 12
            myUniform1 <- newBuffer 45
            f <- compileShader myShader1
            render $ do
                myVertArray1 <- newVertexArray myVertices1
                let p1 = toPrimitiveArray TriangleList myVertArray1 
                f (p1, (Nothing, 1))
                f (p1, (Just myUniform1, 2))
            swap
         
myShader1 = do
              p1 <- toPrimitiveStream fst
              u <- chooseShader
                (\s -> case snd s of 
                        (Nothing, i) -> Left i
                        (Just u, i) -> Right (u,i))
                        (return 2.8)
                        (toUniformBlock id)
              fragStream <- rasterize (const (Front,ViewPort (0,0) (9,9))) (fmap (\x -> (V4 u u x x, u + x)) p1)
              drawContextColor (fmap (\x -> V3 x x x) fragStream) (const (ColorOption NoBlending (V3 True True True)))


myProg = do (myVertices1 :: Buffer os BFloat) <- newBuffer 12
            (myVertices2) <- newBuffer 45
            (myVertices2') <- newBuffer 45
            myUniform1 <- newBuffer 45
            
            myTex <- newTexture2D R8 (V2 10 10)
            
            liftIO $ printStableName myShader -- TODO: Use this
            f <- compileShader myShader
            render $ do
                myVertArray1 <- newVertexArray myVertices1
                myVertArray11 <- newVertexArray myVertices1
                myVertArray12 <- newVertexArray myVertices2
                myVertArray2 <- newVertexArray myVertices2
                myVertArray2' <- newVertexArray myVertices2'
                let p1 = toPrimitiveArray TriangleList myVertArray1 <> toPrimitiveArray TriangleList myVertArray2
                let p2 = toPrimitiveArrayInstanced TriangleList (VertexArray.zipWith (,) myVertArray2 myVertArray12) myVertArray11 (,)
                let p2' = toPrimitiveArrayInstanced TriangleList (VertexArray.zipWith (,) myVertArray2' myVertArray12) myVertArray11 (,)
                f ((p1, p2'), ((myUniform1, myTex), (True, False)))
                f ((p1, p2), ((myUniform1, myTex), (True, True)))
                f ((p1, p2), ((myUniform1, myTex), (False, False)))
                f ((p1, p2), ((myUniform1, myTex), (False, True)))
            swap
         
myShader = do
              u1 <- toUniformBlock (\s -> (fst $ fst $ snd s, 10)) 
              u2 <- toUniformBlock (\s -> (fst $ fst $ snd s, 9)) 
              u3 <- toUniformBlock (\s -> (fst $ fst $ snd s, 8))
              u4 <- toUniformBlock (\s -> (fst $ fst $ snd s, 7))
              s <- newSampler2D (\s -> (snd $ fst $ snd s, (Linear,Linear), Linear, (Wrap, Wrap)))
              (p1, p2) <- mapShader fst $ do
                                          p1 <- toPrimitiveStream fst
                                          p2 <- toPrimitiveStream snd
                                          return (p1,p2)
              let p1' = fmap (func u1) p1
              fragStream <- rasterize (const (Front,ViewPort (0,0) (9,9))) (fmap (\x -> let y = x+u1 in (V4 x (cont (x + y)) (u1 + x) u2, y + u1)) (p1 <> fmap (\((a,b),c) -> a + b + c) p2))
              fragStream2 <- rasterize (const (Front,ViewPort (0,0) (9,9))) (fmap (\x -> let y = x+u1
                                                                                             V1 r = sample2D s (SampleLod 0) Nothing Nothing (V2 x y)  
                                                                                         in (V4 x (sin x + cos y * r) (u1 - x) u3, y + u1)) p1')
              let fragStream3 = (\ f -> V3 f u4 f) <$> filterFragments (<* 5) (fragStream2 <> fragStream)
              let fragStream4 = fmap (\f -> (V3 (f+u4) f 1, u4)) (fragStream <> fragStream2)
              maybeShader (\ s -> if snd $ snd $ snd s then Just (fst $ snd $ snd s) else Nothing) $
                    maybeShader (\ s -> if s then Just (1::Int) else Nothing) $
                        drawContextColor fragStream3 (\1 -> ColorOption NoBlending (V3 True True True))
--              draw fragStream4 $ \(a, d) -> do
  --                  drawColor a (const (Image RGB8, ColorOption NoBlending (RGB True True True)))
    --                drawColor a (const (Image RGB4, ColorOption NoBlending (RGB True True True)))
              drawContextColorDepthStencil fragStream4 (const (ColorOption NoBlending (V3 True True True), undefined))
                                                                                      
printStableName x = (makeStableName $! x) >>= print . hashStableName

func :: VFloat -> VFloat -> VFloat
func x y = (\(x,_,_) -> x) $ while (\(x,y,z) -> x <* y -z) (\(x,y,z) -> (x*2+z,y-2,z)) (x,y,x*y)

cont x = contr (contr (contr x)) 
contr x = x+x+x+x 

