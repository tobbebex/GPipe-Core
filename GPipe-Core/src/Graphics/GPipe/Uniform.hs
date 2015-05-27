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

data Proxy a = Proxy
 
{-# NOINLINE usingUniform #-}
usingUniform :: forall os f b. Uniform b => Frame os f (Buffer os (BUniform (UniformBufferFormat b)), Proxy b, Int) b
usingUniform = IntFrame $ dynInStatOut $ do 
                   blockId <- getName
                   let (u, offToStype) = shaderGen blockId
                       decl :: ShaderGlobDeclM () 
                       decl = gDeclUniformBlock blockId $ buildUDecl offToStype 
                   return (u, 
                           tellShaderGlobDecl blockId decl,
                           \(ub, _, i) -> do doForName blockId $ \ p i -> do
                                             binding <- getNextUniformBinding
                                             glBindBufferRange glUNIFORM_ARRAY binding (bufName ub) (i * bufElementSize ub) (bufElementSize ub) 
                                             glUniformBlockBinding p i binding)
    where
            sampleBuffer = makeBuffer undefined undefined :: Buffer os (BUniform (UniformBufferFormat b))
            ToUniform (Kleisli shaderGenF) = toUniform :: ToUniform (UniformBufferFormat b) b
            fromBUnifom (BUniform b) = b
            shaderGen :: Int -> (b, OffsetToSType) -- Int is name of uniform block
            shaderGen = runReader $ runWriterT $ shaderGenF $ fromBUnifom $ bufBElement sampleBuffer $ BInput 0 0

buildUDecl :: OffsetToSType -> ShaderGlobDeclM ()
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
