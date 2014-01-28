{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs, TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Graphics.GPipe.Uniform where

import Graphics.GPipe.Buffer 
import Graphics.GPipe.Frame
import Graphics.GPipe.Stream
import Graphics.GPipe.Shader
import Control.Arrow
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Category hiding ((.))
import qualified Data.IntMap as Map

class BufferFormat (UniformBufferFormat a) => Uniform a where
    type UniformBufferFormat a
    toUniform :: ToUniform (UniformBufferFormat a) a 

usingUniform :: forall fr os f x a b. Uniform b => Stream fr x a -> (Buffer os (BUniform (UniformBufferFormat b)), Int) -> Stream fr x (a, b)
usingUniform (Stream s) = 
        let sampleBuffer = makeBuffer undefined undefined :: Buffer os (BUniform (UniformBufferFormat b))
            ToUniform (Kleisli shaderGenF) = toUniform :: ToUniform (UniformBufferFormat b) b
            fromBUnifom (BUniform b) = b
            shaderGen = runReader $ runWriterT $ shaderGenF $ fromBUnifom $ bufBElement sampleBuffer $ BInput 0 0
            f (a, blockId, x) = 
                let (u, offToStype) = shaderGen blockId
                    decl = buildUDecl offToStype
                in  ((a,u), blockId+1, decl, x)
            s' = map f s
        in \(ub, i) ->         
            let uIO blockId cs = do binding <- getNext 
                                    lift $ do glBindBufferRange glUNIFORM_ARRAY binding (bufName ub) (i * bufElementSize ub) (bufElementSize ub)                                   
                                              glUniformBlockBinding (cshaderName cs) (cshaderUniBlockNameToIndex cs Map.! blockId) binding
                                                   
                g (a, blockId, decl, PrimitiveStreamData x uBinds) = (a, blockId, PrimitiveStreamData x ((decl, uIO blockId):uBinds))
                -- TODO: More StreamData types
            in Stream $ map g s'

buildUDecl = buildUDecl' 0 . Map.assocs 
    where buildUDecl' p ((off, stype):xs) | off == p = do tellGlobal $ stypeName stype
                                                          tellGlobal " u"
                                                          tellGlobalLn $ show off
                                                          buildUDecl' (p + stypeSize stype) xs
                                          | off > p = do tellGlobal " float pad"
                                                         tellGlobalLn $ show p
                                                         buildUDecl' (p + 4) xs
                                          | otherwise = error "buildUDecl: Expected all sizes to be multiple of 4"
          buildUDecl' _ [] = return ()

type OffsetToSType = Map.IntMap SType  

glUniformBlockBinding :: Int -> Int -> Int -> IO ()
glUniformBlockBinding = undefined

glBindBufferRange :: Int -> Int -> Int -> Int -> Int -> IO ()
glBindBufferRange = undefined                                

glUNIFORM_ARRAY = 0

newtype ToUniform a b = ToUniform (Kleisli (WriterT OffsetToSType (Reader Int)) a b) deriving (Category, Arrow) 

instance Uniform VFloat where
    type UniformBufferFormat VFloat = BFloat
    toUniform = ToUniform $ Kleisli $ \bIn -> do let offset = bOffset bIn
                                                 tell $ Map.singleton offset STypeFloat
                                                 blockId <- lift ask
                                                 return $ S $ useUniform blockId offset  


