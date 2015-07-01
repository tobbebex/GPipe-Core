{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs, TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows #-}

module Graphics.GPipe.Uniform where

import Graphics.GPipe.Buffer 
import Graphics.GPipe.Shader
import Graphics.GPipe.Compiler
import Graphics.GPipe.Expr
import Control.Arrow
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Category hiding ((.))
import qualified Data.IntMap as Map
import Data.IntMap.Lazy (insert)
import Data.Word (Word)


class BufferFormat (UniformBufferFormat a) => Uniform a where
    type UniformBufferFormat a
    toUniform :: ToUniform (UniformBufferFormat a) a 

type UniformHostFormat x = HostFormat (UniformBufferFormat x)

-- uniformBlock ::  forall os f b. Uniform b => Shader os f (UniformHostFormat b, Proxy b) b
 
toUniformBlock :: forall os f s b. Uniform b => (s -> (Buffer os (BUniform (UniformBufferFormat b)), Int)) -> Shader os f s b
toUniformBlock sf = Shader $ do 
                   blockId <- getName
                   let (u, offToStype) = shaderGen (useUniform (buildUDecl offToStype) blockId)
                   doForUniform blockId $ \s bind -> let (ub, i) = sf s 
                                                     in  glBindBufferRange glUNIFORM_ARRAY bind (bufName ub) (i * bufElementSize ub) (bufElementSize ub)
                   return u
    where
            sampleBuffer = makeBuffer undefined undefined :: Buffer os (BUniform (UniformBufferFormat b))
            ToUniform (Kleisli shaderGenF) = toUniform :: ToUniform (UniformBufferFormat b) b
            fromBUnifom (BUniform b) = b
            shaderGen :: (Int -> ExprM String) -> (b, OffsetToSType) -- Int is name of uniform block
            shaderGen = runReader $ runWriterT $ shaderGenF $ fromBUnifom $ bufBElement sampleBuffer $ BInput 0 0

            doForUniform :: Int -> (s -> Binding -> IO()) -> ShaderM s ()
            doForUniform n io = modifyRenderIO (\s -> s { uniformNameToRenderIO = insert n io (uniformNameToRenderIO s) } )

buildUDecl :: OffsetToSType -> GlobDeclM ()
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
glUniformBlockBinding a b c = putStrLn $ "glUniformBlockBinding " ++ show (a,b,c)

glBindBufferRange :: Int -> Int -> Int -> Int -> Int -> IO ()
glBindBufferRange a b c d e = putStrLn $ "glBindBufferRange " ++ show (a,b,c,d,e)                            

glUNIFORM_ARRAY = 0

newtype ToUniform a b = ToUniform (Kleisli (WriterT OffsetToSType (Reader (Int -> ExprM String))) a b) deriving (Category, Arrow) 

makeUniform styp = ToUniform $ Kleisli $ \bIn -> do let offset = bOffset bIn
                                                    tell $ Map.singleton offset STypeFloat
                                                    useF <- lift ask
                                                    return $ S $ useF offset

instance Uniform (S x Float) where
    type UniformBufferFormat (S x Float) = BFloat
    toUniform = makeUniform STypeFloat

instance Uniform (S x Int) where
    type UniformBufferFormat (S x Int) = BInt32
    toUniform = makeUniform STypeInt

instance Uniform (S x Word) where
    type UniformBufferFormat (S x Word) = BWord32
    toUniform = makeUniform STypeUInt

instance (Uniform a, Uniform b) => Uniform (a,b) where
    type UniformBufferFormat (a,b) = (UniformBufferFormat a, UniformBufferFormat b)
    toUniform = proc (a,b) -> do a' <- toUniform -< a
                                 b' <- toUniform -< b
                                 returnA -< (a', b')

instance (Uniform a, Uniform b, Uniform c) => Uniform (a,b,c) where
    type UniformBufferFormat (a,b,c) = (UniformBufferFormat a, UniformBufferFormat b, UniformBufferFormat c)
    toUniform = proc (a,b,c) -> do a' <- toUniform -< a
                                   b' <- toUniform -< b
                                   c' <- toUniform -< c
                                   returnA -< (a', b', c')

instance (Uniform a, Uniform b, Uniform c, Uniform d) => Uniform (a,b,c,d) where
    type UniformBufferFormat (a,b,c,d) = (UniformBufferFormat a, UniformBufferFormat b, UniformBufferFormat c, UniformBufferFormat d)
    toUniform = proc (a,b,c,d) -> do a' <- toUniform -< a
                                     b' <- toUniform -< b
                                     c' <- toUniform -< c
                                     d' <- toUniform -< d
                                     returnA -< (a', b', c', d')                                                   
