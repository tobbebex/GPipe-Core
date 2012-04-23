{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs, TypeSynonymInstances, ScopedTypeVariables #-}

module Graphics.GPipe.Uniform where

import Graphics.GPipe.Buffer 
import Graphics.GPipe.Shader
import Control.Arrow
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, replicateM_)
import Foreign.Storable (sizeOf)
import Data.IntMap (toAscList)

class BufferFormat (UniformBufferFormat a) => Uniform a where
    type UniformBufferFormat a
    --TODO: Add if needed:  uniform :: Shader os (HostFormat (UniformBufferFormat a)) a
    loadUniform :: Shader os (UniformBufferFormat a) a

uniformBuffer :: forall os a. Uniform a => Shader os (Buffer os (UniformBufferFormat a)) a
uniformBuffer = Shader (Kleisli shader) (Kleisli setup)
        where
            shader _ = do   pushUMap
                            u <- runKleisli shaderLoad (bElement sampleBuffer)
                            blockMap <- popUMap 
                            blockId <- getNextGlobal           
                            tellGlobal $ "uniform uBlock" ++ show blockId ++ " {\n"
                            tellUMap 0 (toAscList blockMap) 
                            tellGlobal "};\n"
                            return u                             
            setup buffer = do
                               blockId <- getNextGlobal
                               liftIO $ glBindBufferToBlock (bName buffer) blockId
                               void $ runKleisli setupLoad (bElement buffer)
                               return undefined
            sampleBuffer = makeBuffer undefined undefined :: Buffer os (UniformBufferFormat a)
            Shader shaderLoad setupLoad = loadUniform :: Shader os (UniformBufferFormat a) a
            tellUMap _ [] = return ()
            tellUMap pos ((off,(decl, size)):xs) = do replicateM_ (off - pos `div` 4) $ do v <- getTempVar
                                                                                           tellGlobalDecl ("float pad" ++ show v)
                                                      tellGlobalDecl decl
                                                      tellUMap (pos + size) xs

type UniformHostFormat a = HostFormat (UniformBufferFormat a)

uniform :: Uniform a => Shader os (UniformHostFormat  a) a
uniform = undefined

    
instance Uniform VFloat where
    type UniformBufferFormat VFloat = BFloat
    loadUniform = Shader (Kleisli shader) (Kleisli setup)
        where
            shader (B _ off) = do
                          uniId <- getNextGlobal
                          let uni = 'u' : show uniId
                          addToUMap off ("float " ++ uni) (sizeOf (undefined :: Float))
                          tellGlobalDecl $ "uniform float " ++ uni                           
                          return (S $ return uni)
            shader (BConst _) = do
                          uniId <- getNextGlobal
                          let uni = 'u' : show uniId
                          tellGlobalDecl $ "uniform float " ++ uni                           
                          return (S $ return uni)
                          
            setup (BConst a) =  do uniId <- getNextGlobal
                                   liftIO $ glLoadUniformFloat uniId a
                                   return undefined
    

glBindBufferToBlock :: Int -> Int -> IO ()
glBindBufferToBlock = undefined    


glLoadUniformFloat :: Int -> Float -> IO () 
glLoadUniformFloat = undefined