{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes #-}
module Main where

import Data.Boolean
import Graphics.GPipe.Context
import Graphics.GPipe.Buffer
import Graphics.GPipe.Format
import Graphics.GPipe.VertexArray as VertexArray
import Graphics.GPipe.Frame
import Graphics.GPipe.VertexStream
import Graphics.GPipe.FragmentStream
import Graphics.GPipe.FrameBuffer
import Graphics.GPipe.Shader
import Graphics.GPipe.Uniform
import Control.Category (Category(..))
import Prelude hiding ((.), id, length)
import System.Mem.StableName (makeStableName, hashStableName)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))


debugContext :: ContextFactory c ds 
debugContext f = return $ ContextHandle 
                (error "newSharedContext not supported")
                (putStrLn "sync:" >>)
                (putStrLn "async:" >>)
                (putStrLn "swap")
                (return (1,1))
                (putStrLn "Delete context")
                
main :: IO ()
main = do runContextT debugContext (ContextFormatColorDepth RGBA8 Depth16) myProg
          putStrLn "Finished!"

myProg = do (myVertices1 :: Buffer os BFloat) <- newBuffer 12
            (myVertices2) <- newBuffer 45
            myUniform1 <- newBuffer 45
            (_ :: Buffer os BFloat) <- newBuffer 12
      
            
            liftIO $ printStableName myFrame -- TODO: Use this
            f <- compileFrame myFrame
            render $ do
                myVertArray1 <- newVertexArray myVertices1
                myVertArray11 <- newVertexArray myVertices1
                myVertArray12 <- newVertexArray myVertices2
                myVertArray2 <- newVertexArray myVertices2
                let p1 = toPrimitiveArray TriangleList myVertArray1 <> toPrimitiveArray TriangleList myVertArray2
                let p2 = toPrimitiveArrayInstanced TriangleList (VertexArray.zipWith (,) myVertArray2 myVertArray12) myVertArray11 (,)
                runFrame f ((p1, p2), myUniform1)
            swap
         
 
           
myFrame = do
              u1 <- toUniformBlock (\s -> (snd s, 10)) 
              u2 <- toUniformBlock (\s -> (snd s, 9)) 
              u3 <- toUniformBlock (\s -> (snd s, 8))
              u4 <- toUniformBlock (\s -> (snd s, 7))
              (p1, p2) <- mapFrame fst $ do
                                          p1 <- toPrimitiveStream fst
                                          p2 <- toPrimitiveStream snd
                                          return (p1,p2)
              let p1' = fmap (func u1) p1
              let fragStream = rasterize Front (fmap (\x -> let y = x+u1 in ((x,cont (x+y),u1+x,u2), y+u1)) (p1 <> fmap (\((a,b),c) -> a + b + c) p2))
              let fragStream2 = rasterize Front (fmap (\x -> let y = x+u1 in ((x, sin x + cos y, u1 - x, u3), y+u1)) p1')
              let fragStream3 = (\ f -> RGBA f u4 f 1) <$> filterFragments (<* 5) (fragStream2 <> fragStream)
              let fragStream4 = fmap (\f -> RGBA (f*2) (f+u4) f 1) (fragStream <> fragStream2)
              drawContextColor fragStream3 (const $ ColorOption NoBlending (RGBA True True True True))
              drawContextColor fragStream4 (const $ ColorOption NoBlending (RGBA True True True True))
                                                                                      
printStableName x = (makeStableName $! x) >>= print . hashStableName

func :: VFloat -> VFloat -> VFloat
func x y = (\(x,_,_) -> x) $ while (\(x,y,z) -> x <* y -z) (\(x,y,z) -> (x*2+z,y-2,z)) (x,y,x*y)

{-# NOINLINE cont #-}
cont x = contr (contr (contr x)) 
{-# NOINLINE contr #-}
contr x = x+x+x+x 

