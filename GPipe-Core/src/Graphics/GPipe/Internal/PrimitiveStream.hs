{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving #-}

module Graphics.GPipe.Internal.PrimitiveStream where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Prelude hiding (length, id, (.))
import Graphics.GPipe.Internal.Buffer
import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.Shader
import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.PrimitiveArray
import Graphics.GPipe.Internal.Context
import Control.Category
import Control.Arrow
import Data.Monoid (Monoid(..))
import Data.IntMap.Lazy (insert)
import Data.Word
import Data.Int

import Graphics.Rendering.OpenGL.Raw.Core33
import Foreign.Marshal.Utils
import Foreign.Ptr (intPtrToPtr)
import Data.IORef

--------------------

--------------------
type DrawCallName = Int
data PrimitiveStreamData = PrimitiveStreamData DrawCallName

newtype PrimitiveStream t a = PrimitiveStream [(a, PrimitiveStreamData)] deriving Monoid

instance Functor (PrimitiveStream t) where
        fmap f (PrimitiveStream xs) = PrimitiveStream $ map (first f) xs

class BufferFormat a => VertexInput a where
    type VertexFormat a
    toVertex :: ToVertex a (VertexFormat a)  

newtype ToVertex a b = ToVertex (Kleisli (StateT Int (Writer [Binding -> (IO VAOKey, IO ())])) a b) deriving (Category, Arrow)


toPrimitiveStream :: forall os f s a p. (VertexInput a, PrimitiveTopology p) => (s -> PrimitiveArray p a) -> Shader os f s (PrimitiveStream p (VertexFormat a))   
toPrimitiveStream sf = Shader $ do n <- getName
                                   let sampleBuffer = makeBuffer undefined undefined :: Buffer os a
                                       x = fst $ runWriter (evalStateT (mf $ bufBElement sampleBuffer $ BInput 0 0) 0)
                                   doForInputArray n (map drawcall . getPrimitiveArray . sf)
                                   return $ PrimitiveStream [(x, PrimitiveStreamData n)] 
    where 
        ToVertex (Kleisli mf) = toVertex :: ToVertex a (VertexFormat a)
        drawcall (PrimitiveArraySimple p l a) binds = (attribs a binds, glDrawArrays (toGLtopology p) 0 (fromIntegral l)) 
        drawcall (PrimitiveArrayIndexed p i a) binds = (attribs a binds, do
                                                    bindIndexBuffer i
                                                    glDrawElements (toGLtopology p) (fromIntegral $ indexArrayLength i) (fromIntegral $ indexType i) (intPtrToPtr $ fromIntegral $ offset i))
        drawcall (PrimitiveArrayInstanced p il l a) binds = (attribs a binds, glDrawArraysInstanced (toGLtopology p) 0 (fromIntegral l) (fromIntegral il))
        drawcall (PrimitiveArrayIndexedInstanced p i il a) binds = (attribs a binds, do
                                                      bindIndexBuffer i
                                                      glDrawElementsInstanced (toGLtopology p) (fromIntegral $ indexArrayLength i) (fromIntegral $ indexType i) (intPtrToPtr $ fromIntegral $ offset i) (fromIntegral il))
        bindIndexBuffer i = do case restart i of Just x -> do glEnable gl_PRIMITIVE_RESTART 
                                                              glPrimitiveRestartIndex (fromIntegral x)
                                                 Nothing -> glDisable gl_PRIMITIVE_RESTART
                               bname <- readIORef (iArrName i)
                               glBindBuffer bname gl_ELEMENT_ARRAY_BUFFER

        assignIxs :: Int -> Binding -> [Int] -> [Binding -> (IO VAOKey, IO ())] -> [(IO VAOKey, IO ())] 
        assignIxs n ix xxs@(x:xs) (f:fs) | x == n    = f ix : assignIxs (n+1) (ix+1) xs fs
                                         | otherwise = assignIxs (n+1) ix xxs fs
        assignIxs _ _ _ [] = []                                          
        assignIxs _ _ _ _ = error "Too few attributes generated in toPrimitiveStream"
                
        attribs a binds = first sequence $ second sequence_ $ unzip $ assignIxs 0 0 binds $ execWriter (runStateT (mf a) 0)

        doForInputArray :: Int -> (s -> [[Binding] -> ((IO [VAOKey], IO ()), IO ())]) -> ShaderM s ()
        doForInputArray n io = modifyRenderIO (\s -> s { inputArrayToRenderIOs = insert n io (inputArrayToRenderIOs s) } )

data InputIndices = InputIndices {
        inputVertexID :: VInt,
        inputInstanceID :: VInt
    }

withInputIndices :: PrimitiveStream p a -> PrimitiveStream p (a, InputIndices)  
withInputIndices = fmap (\a -> (a, InputIndices (scalarS' "gl_VertexID") (scalarS' "gl_InstanceID")) )

makeVertexFx norm x f styp typ b = do 
                             n <- get
                             put $ n + 1
                             let combOffset = bStride b * bSkipElems b + bOffset b
                             lift $ tell [\ix -> ( do bn <- readIORef $ bName b
                                                      return $ VAOKey bn combOffset x norm (bInstanceDiv b)
                                                 , do bn <- readIORef $ bName b
                                                      let ix' = fromIntegral ix
                                                      glEnableVertexAttribArray ix'
                                                      glBindBuffer bn gl_ARRAY_BUFFER 
                                                      glVertexAttribDivisor ix' (fromIntegral $ bInstanceDiv b)
                                                      glVertexAttribPointer ix' x typ (fromBool norm) (fromIntegral $ bStride b) (intPtrToPtr $ fromIntegral combOffset))]
                             return (f styp $ useVInput styp n)

makeVertexFnorm = makeVertexFx True 
makeVertexF = makeVertexFx False

makeVertexI x f styp typ b = do 
                             n <- get
                             put $ n + 1
                             let combOffset = bStride b * bSkipElems b + bOffset b
                             lift $ tell [\ix -> ( do bn <- readIORef $ bName b
                                                      return $ VAOKey bn combOffset x False (bInstanceDiv b)
                                                 , do bn <- readIORef $ bName b
                                                      let ix' = fromIntegral ix
                                                      glEnableVertexAttribArray ix'
                                                      glBindBuffer bn gl_ARRAY_BUFFER
                                                      glVertexAttribDivisor ix' (fromIntegral $ bInstanceDiv b) 
                                                      glVertexAttribIPointer ix' x typ (fromIntegral $ bStride b) (intPtrToPtr $ fromIntegral combOffset))]
                             return (f styp $ useVInput styp n) 

-- scalars

unBnorm (BNormalized a) = a
instance VertexInput BFloat where
    type VertexFormat BFloat = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexF 1 (const S) STypeFloat gl_FLOAT
instance VertexInput BInt32Norm where
    type VertexFormat BInt32Norm = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 1 (const S) STypeFloat gl_INT . unBnorm
instance VertexInput BInt16Norm where
    type VertexFormat BInt16Norm = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 1 (const S) STypeFloat gl_SHORT . unBnorm
instance VertexInput BInt8Norm where
    type VertexFormat BInt8Norm = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 1 (const S) STypeFloat gl_BYTE . unBnorm
instance VertexInput BWord32Norm where
    type VertexFormat BWord32Norm = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 1 (const S) STypeFloat gl_UNSIGNED_INT . unBnorm
instance VertexInput BWord16Norm where
    type VertexFormat BWord16Norm = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 1 (const S) STypeFloat gl_UNSIGNED_SHORT . unBnorm
instance VertexInput BWord8Norm where
    type VertexFormat BWord8Norm = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 1 (const S) STypeFloat gl_UNSIGNED_BYTE . unBnorm
instance VertexInput BInt32 where
    type VertexFormat BInt32 = VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 1 (const S) STypeInt gl_INT
instance VertexInput BInt16 where
    type VertexFormat BInt16 = VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 1 (const S) STypeInt gl_SHORT
instance VertexInput BInt8 where
    type VertexFormat BInt8 = VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 1 (const S) STypeInt gl_BYTE
instance VertexInput BWord32 where
    type VertexFormat BWord32 = VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 1 (const S) STypeUInt gl_UNSIGNED_INT
instance VertexInput BWord16 where
    type VertexFormat BWord16 = VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 1 (const S) STypeUInt gl_UNSIGNED_SHORT
instance VertexInput BWord8 where
    type VertexFormat BWord8 = VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 1 (const S) STypeUInt gl_UNSIGNED_BYTE

       
-- B2

instance VertexInput (B2 Float) where
    type VertexFormat (B2 Float) = (VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexF 2 vec2S STypeFloat gl_FLOAT . unB2
instance VertexInput (BNormalized (B2 Int32)) where
    type VertexFormat (BNormalized (B2 Int32)) = (VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S STypeFloat gl_INT . unB2 . unBnorm
instance VertexInput (BNormalized (B2 Int16)) where
    type VertexFormat (BNormalized (B2 Int16)) = (VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S STypeFloat gl_SHORT . unB2 . unBnorm
instance VertexInput (BNormalized (B2 Int8)) where
    type VertexFormat (BNormalized (B2 Int8)) = (VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S STypeFloat gl_BYTE . unB2 . unBnorm
instance VertexInput (BNormalized (B2 Word32)) where
    type VertexFormat (BNormalized (B2 Word32)) = (VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S STypeFloat gl_UNSIGNED_INT . unB2 . unBnorm
instance VertexInput (BNormalized (B2 Word16)) where
    type VertexFormat (BNormalized (B2 Word16)) = (VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S STypeFloat gl_UNSIGNED_SHORT . unB2 . unBnorm
instance VertexInput (BNormalized (B2 Word8)) where
    type VertexFormat (BNormalized (B2 Word8)) = (VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S STypeFloat gl_UNSIGNED_BYTE . unB2 . unBnorm
instance VertexInput  (B2 Int32) where
    type VertexFormat  (B2 Int32) = (VInt, VInt)
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S STypeInt gl_INT . unB2
instance VertexInput  (B2 Int16) where
    type VertexFormat  (B2 Int16) = (VInt, VInt)
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S STypeInt gl_SHORT . unB2
instance VertexInput  (B2 Int8) where
    type VertexFormat  (B2 Int8) = (VInt, VInt)
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S STypeInt gl_BYTE . unB2
instance VertexInput  (B2 Word32) where
    type VertexFormat  (B2 Word32) = (VWord, VWord)
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S STypeUInt gl_UNSIGNED_INT . unB2
instance VertexInput  (B2 Word16) where
    type VertexFormat  (B2 Word16) = (VWord, VWord)
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S STypeUInt gl_UNSIGNED_SHORT . unB2
instance VertexInput  (B2 Word8) where
    type VertexFormat  (B2 Word8) = (VWord, VWord)
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S STypeUInt gl_UNSIGNED_BYTE . unB2

-- B3

instance VertexInput (B3 Float) where
    type VertexFormat (B3 Float) = (VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexF 3 vec3S STypeFloat gl_FLOAT . unB3
instance VertexInput (BNormalized (B3 Int32)) where
    type VertexFormat (BNormalized (B3 Int32)) = (VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S STypeFloat gl_INT . unB3 . unBnorm
instance VertexInput (BNormalized (B3 Int16)) where
    type VertexFormat (BNormalized (B3 Int16)) = (VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S STypeFloat gl_SHORT . unB3 . unBnorm
instance VertexInput (BNormalized (B3 Int8)) where
    type VertexFormat (BNormalized (B3 Int8)) = (VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S STypeFloat gl_BYTE . unB3 . unBnorm
instance VertexInput (BNormalized (B3 Word32)) where
    type VertexFormat (BNormalized (B3 Word32)) = (VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S STypeFloat gl_UNSIGNED_INT . unB3 . unBnorm
instance VertexInput (BNormalized (B3 Word16)) where
    type VertexFormat (BNormalized (B3 Word16)) = (VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S STypeFloat gl_UNSIGNED_SHORT . unB3 . unBnorm
instance VertexInput (BNormalized (B3 Word8)) where
    type VertexFormat (BNormalized (B3 Word8)) = (VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S STypeFloat gl_UNSIGNED_BYTE . unB3 . unBnorm
instance VertexInput  (B3 Int32) where
    type VertexFormat  (B3 Int32) = (VInt, VInt, VInt)
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S STypeInt gl_INT . unB3
instance VertexInput  (B3 Int16) where
    type VertexFormat  (B3 Int16) = (VInt, VInt, VInt)
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S STypeInt gl_SHORT . unB3
instance VertexInput  (B3 Int8) where
    type VertexFormat  (B3 Int8) = (VInt, VInt, VInt)
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S STypeInt gl_BYTE . unB3
instance VertexInput  (B3 Word32) where
    type VertexFormat  (B3 Word32) = (VWord, VWord, VWord)
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S STypeUInt gl_UNSIGNED_INT . unB3
instance VertexInput  (B3 Word16) where
    type VertexFormat  (B3 Word16) = (VWord, VWord, VWord)
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S STypeUInt gl_UNSIGNED_SHORT . unB3
instance VertexInput  (B3 Word8) where
    type VertexFormat  (B3 Word8) = (VWord, VWord, VWord)
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S STypeUInt gl_UNSIGNED_BYTE . unB3

-- B4

instance VertexInput (B4 Float) where
    type VertexFormat (B4 Float) = (VFloat, VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexF 4 vec4S STypeFloat gl_FLOAT . unB4
instance VertexInput (BNormalized (B4 Int32)) where
    type VertexFormat (BNormalized (B4 Int32)) = (VFloat, VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S STypeFloat gl_INT . unB4 . unBnorm
instance VertexInput (BNormalized (B4 Int16)) where
    type VertexFormat (BNormalized (B4 Int16)) = (VFloat, VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S STypeFloat gl_SHORT . unB4 . unBnorm
instance VertexInput (BNormalized (B4 Int8)) where
    type VertexFormat (BNormalized (B4 Int8)) = (VFloat, VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S STypeFloat gl_BYTE . unB4 . unBnorm
instance VertexInput (BNormalized (B4 Word32)) where
    type VertexFormat (BNormalized (B4 Word32)) = (VFloat, VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S STypeFloat gl_UNSIGNED_INT . unB4 . unBnorm
instance VertexInput (BNormalized (B4 Word16)) where
    type VertexFormat (BNormalized (B4 Word16)) = (VFloat, VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S STypeFloat gl_UNSIGNED_SHORT . unB4 . unBnorm
instance VertexInput (BNormalized (B4 Word8)) where
    type VertexFormat (BNormalized (B4 Word8)) = (VFloat, VFloat, VFloat, VFloat)
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S STypeFloat gl_UNSIGNED_BYTE . unB4 . unBnorm
instance VertexInput  (B4 Int32) where
    type VertexFormat  (B4 Int32) = (VInt, VInt, VInt, VInt)
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S STypeInt gl_INT . unB4
instance VertexInput  (B4 Int16) where
    type VertexFormat  (B4 Int16) = (VInt, VInt, VInt, VInt)
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S STypeInt gl_SHORT . unB4
instance VertexInput  (B4 Int8) where
    type VertexFormat  (B4 Int8) = (VInt, VInt, VInt, VInt)
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S STypeInt gl_BYTE . unB4
instance VertexInput  (B4 Word32) where
    type VertexFormat  (B4 Word32) = (VWord, VWord, VWord, VWord)
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S STypeUInt gl_UNSIGNED_INT . unB4
instance VertexInput  (B4 Word16) where
    type VertexFormat  (B4 Word16) = (VWord, VWord, VWord, VWord)
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S STypeUInt gl_UNSIGNED_SHORT . unB4
instance VertexInput  (B4 Word8) where
    type VertexFormat  (B4 Word8) = (VWord, VWord, VWord, VWord)
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S STypeUInt gl_UNSIGNED_BYTE . unB4

                                             
instance (VertexInput a, VertexInput b) => VertexInput (a,b) where
    type VertexFormat (a,b) = (VertexFormat a, VertexFormat b)
    toVertex = proc (a,b) -> do a' <- toVertex -< a
                                b' <- toVertex -< b
                                returnA -< (a', b')

instance (VertexInput a, VertexInput b, VertexInput c) => VertexInput (a,b,c) where
    type VertexFormat (a,b,c) = (VertexFormat a, VertexFormat b, VertexFormat c)
    toVertex = proc (a,b,c) -> do a' <- toVertex -< a
                                  b' <- toVertex -< b
                                  c' <- toVertex -< c
                                  returnA -< (a', b', c')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d) => VertexInput (a,b,c,d) where
    type VertexFormat (a,b,c,d) = (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d)
    toVertex = proc (a,b,c,d) -> do a' <- toVertex -< a
                                    b' <- toVertex -< b
                                    c' <- toVertex -< c
                                    d' <- toVertex -< d
                                    returnA -< (a', b', c', d')

