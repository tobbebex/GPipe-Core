{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs, TypeSynonymInstances, ScopedTypeVariables #-}

module Graphics.GPipe.Uniform where

import Graphics.GPipe.Buffer 
import Graphics.GPipe.Shader
import Control.Arrow
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when)
import Data.IntMap (toAscList)

class BufferFormat (UniformBufferFormat a) => Uniform a where
    type UniformBufferFormat a
    --TODO: Add if needed:  uniform :: Shader os (HostFormat (UniformBufferFormat a)) a
    loadUniform :: Shader os (UniformBufferFormat a) a

uniformBuffer :: forall os a. Uniform a => Shader os (Buffer os (UniformBufferFormat a)) a
uniformBuffer = Shader (Kleisli shader) (Kleisli setup)
        where
            shader _ = do   blockId <- getNextGlobal
                            pushUMap blockId
                            u <- runKleisli shaderLoad (bElement sampleBuffer)
                            blockMap <- popUMap 
                            tellGlobal $ "uniform uBlock" ++ show blockId ++ " {\n"
                            tellUMap 0 (toAscList blockMap) 
                            tellGlobal $ "} u" ++ show blockId ++ ";\n"
                            return u                             
            setup buffer = do
                               blockId <- getNextGlobal
                               liftIO $ glBindBufferToBlock (bName buffer) blockId
                               void $ runKleisli setupLoad (bElement buffer)
                               return undefined
            sampleBuffer = makeBuffer undefined undefined :: Buffer os (UniformBufferFormat a)
            Shader shaderLoad setupLoad = loadUniform :: Shader os (UniformBufferFormat a) a
            tellUMap _ [] = return ()
            tellUMap pos ((off,t):xs) = do let pad = off - pos `div` 4
                                           when (pad > 0) $ do
                                                v <- getTempVar
                                                tellGlobalDecl $ "float pad" ++ show v ++ "[" ++ show pad ++ "]"
                                           tellGlobalDecl $ stypeName t ++ " u" ++ show off
                                           tellUMap (pos + stypeSize t) xs

type UniformHostFormat a = HostFormat (UniformBufferFormat a)

uniform :: forall os a. Uniform a => Shader os (UniformHostFormat a) a
uniform = Shader (Kleisli shader) (Kleisli setup)
    where
        shader _ = runKleisli shaderLoad (f (error "Hmm, does this work?")) -- TODO: Investigate if this really works
        setup a = runKleisli setupLoad (f a)
        Shader shaderLoad setupLoad = loadUniform :: Shader os (UniformBufferFormat a) a
        ToBuffer _ _ f = toBuffer :: ToBuffer (UniformHostFormat a) (UniformBufferFormat a)

blockUniformName :: Int -> Int -> String
blockUniformName blockId off = 'u' : show blockId ++ ".u" ++ show off 
    
instance Uniform VFloat where
    type UniformBufferFormat VFloat = BFloat
    loadUniform = Shader (Kleisli shader) (Kleisli setup)
        where
            shader (B _ off) = do
                          blockId <- addToUMap off STypeFloat                          
                          return (S $ return $ blockUniformName blockId off)
            shader (BConst _) = do
                          uniId <- getNextGlobal
                          let uni = "uu" ++ show uniId
                          tellGlobalDecl $ "uniform float " ++ uni                           
                          return (S $ return uni)                          
            setup (BConst a) =  do uniId <- getNextGlobal
                                   let uni = "uu" ++ show uniId
                                   liftIO $ glLoadUniformFloat uni a
                                   return undefined
            setup (B _ _) = error "Shouldnt be able to run setup on uniform value shader on non const B value"                                   
    

glBindBufferToBlock :: Int -> Int -> IO ()
glBindBufferToBlock = undefined    


glLoadUniformFloat :: String -> Float -> IO () 
glLoadUniformFloat = undefined