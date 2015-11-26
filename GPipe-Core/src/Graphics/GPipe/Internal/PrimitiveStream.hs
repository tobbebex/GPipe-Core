{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving, PatternSynonyms #-}

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

import Graphics.GL.Core33
import Foreign.Marshal.Utils
import Foreign.Ptr (intPtrToPtr)
import Data.IORef
import Linear.V4
import Linear.V3
import Linear.V2
import Linear.V1
import Linear.V0
import Linear.Plucker (Plucker(..))
import Linear.Quaternion (Quaternion(..))
import Linear.Affine (Point(..))
import Data.Maybe (fromMaybe)

type DrawCallName = Int
data PrimitiveStreamData = PrimitiveStreamData DrawCallName

-- | A @'PrimitiveStream' t a @ is a stream of primitives of type @t@ where the vertices are values of type @a@. You
--   can operate a stream's vertex values using the 'Functor' instance (this will result in a shader running on the GPU).
--   You may also append 'PrimitiveStream's using the 'Monoid' instance, but if possible append the origin 'PrimitiveArray's instead, as this will create more optimized
--   draw calls.
newtype PrimitiveStream t a = PrimitiveStream [(a, (Maybe PointSize, PrimitiveStreamData))] deriving Monoid

instance Functor (PrimitiveStream t) where
        fmap f (PrimitiveStream xs) = PrimitiveStream $ map (first f) xs

-- | This class constraints which buffer types can be turned into vertex values, and what type those values have.
class BufferFormat a => VertexInput a where
    -- | The type the buffer value will be turned into once it becomes a vertex value.
    type VertexFormat a
    -- | An arrow action that turns a value from it's buffer representation to it's vertex representation. Use 'toVertex' from
    --   the GPipe provided instances to operate in this arrow. Also note that this arrow needs to be able to return a value
    --   lazily, so ensure you use
    --
    --  @proc ~pattern -> do ...@.
    toVertex :: ToVertex a (VertexFormat a)

-- | The arrow type for 'toVertex'.
newtype ToVertex a b = ToVertex (Kleisli (StateT Int (Writer [Binding -> (IO VAOKey, IO ())])) a b) deriving (Category, Arrow)


-- | Create a primitive stream from a primitive array provided from the shader environment.
toPrimitiveStream :: forall os f s a p. VertexInput a => (s -> PrimitiveArray p a) -> Shader os f s (PrimitiveStream p (VertexFormat a))
toPrimitiveStream sf = Shader $ do n <- getName
                                   uniAl <- askUniformAlignment
                                   let sampleBuffer = makeBuffer undefined undefined uniAl :: Buffer os a
                                       x = fst $ runWriter (evalStateT (mf $ bufBElement sampleBuffer $ BInput 0 0) 0)
                                   doForInputArray n (map drawcall . getPrimitiveArray . sf)
                                   return $ PrimitiveStream [(x, (Nothing, PrimitiveStreamData n))]
    where
        ToVertex (Kleisli mf) = toVertex :: ToVertex a (VertexFormat a)
        drawcall (PrimitiveArraySimple p l a) binds = (attribs a binds, glDrawArrays (toGLtopology p) 0 (fromIntegral l))
        drawcall (PrimitiveArrayIndexed p i a) binds = (attribs a binds, do
                                                    bindIndexBuffer i
                                                    glDrawElements (toGLtopology p) (fromIntegral $ indexArrayLength i) (indexType i) (intPtrToPtr $ fromIntegral $ offset i * glSizeOf (indexType i)))
        drawcall (PrimitiveArrayInstanced p il l a) binds = (attribs a binds, glDrawArraysInstanced (toGLtopology p) 0 (fromIntegral l) (fromIntegral il))
        drawcall (PrimitiveArrayIndexedInstanced p i il a) binds = (attribs a binds, do
                                                      bindIndexBuffer i
                                                      glDrawElementsInstanced (toGLtopology p) (fromIntegral $ indexArrayLength i) (indexType i) (intPtrToPtr $ fromIntegral $ offset i * glSizeOf (indexType i)) (fromIntegral il))
        bindIndexBuffer i = do case restart i of Just x -> do glEnable GL_PRIMITIVE_RESTART
                                                              glPrimitiveRestartIndex (fromIntegral x)
                                                 Nothing -> glDisable GL_PRIMITIVE_RESTART
                               bname <- readIORef (iArrName i)
                               glBindBuffer GL_ELEMENT_ARRAY_BUFFER bname
        glSizeOf GL_UNSIGNED_INT = 4
        glSizeOf GL_UNSIGNED_SHORT = 2
        glSizeOf GL_UNSIGNED_BYTE = 1
        glSizeOf _ = error "toPrimitiveStream: Unknown indexArray type"

        assignIxs :: Int -> Binding -> [Int] -> [Binding -> (IO VAOKey, IO ())] -> [(IO VAOKey, IO ())]
        assignIxs n ix xxs@(x:xs) (f:fs) | x == n    = f ix : assignIxs (n+1) (ix+1) xs fs
                                         | otherwise = assignIxs (n+1) ix xxs fs
        assignIxs _ _ [] _ = []
        assignIxs _ _ _ _ = error "Too few attributes generated in toPrimitiveStream"

        attribs a binds = first sequence $ second sequence_ $ unzip $ assignIxs 0 0 binds $ execWriter (runStateT (mf a) 0)

        doForInputArray :: Int -> (s -> [[Binding] -> ((IO [VAOKey], IO ()), IO ())]) -> ShaderM s ()
        doForInputArray n io = modifyRenderIO (\s -> s { inputArrayToRenderIOs = insert n io (inputArrayToRenderIOs s) } )

data InputIndices = InputIndices {
        inputVertexID :: VInt,
        inputInstanceID :: VInt
    }

-- | Like 'fmap', but where the vertex and instance IDs are provided as arguments as well.
withInputIndices :: (a -> InputIndices -> b) -> PrimitiveStream p a -> PrimitiveStream p b
withInputIndices f = fmap (\a -> f a (InputIndices (scalarS' "gl_VertexID") (scalarS' "gl_InstanceID")))

type PointSize = VFloat
-- | Like 'fmap', but where each point's size is provided as arguments as well, and a new point size is set for each point in addition to the new vertex value.
--
--   When a 'PrimitiveStream' of 'Points' is created, all points will have the default size of 1.
withPointSize :: (a -> PointSize -> (b, PointSize)) -> PrimitiveStream Points a -> PrimitiveStream Points b
withPointSize f (PrimitiveStream xs) = PrimitiveStream $ map (\(a, (ps, d)) -> let (b, ps') = f a (fromMaybe (scalarS' "1") ps) in (b, (Just ps', d))) xs

makeVertexFx norm x f styp typ b = do
                             n <- get
                             put $ n + 1
                             let combOffset = bStride b * bSkipElems b + bOffset b
                             lift $ tell [\ix -> ( do bn <- readIORef $ bName b
                                                      return $ VAOKey bn combOffset x norm (bInstanceDiv b)
                                                 , do bn <- readIORef $ bName b
                                                      let ix' = fromIntegral ix
                                                      glEnableVertexAttribArray ix'
                                                      glBindBuffer GL_ARRAY_BUFFER bn
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
                                                      glBindBuffer GL_ARRAY_BUFFER bn
                                                      glVertexAttribDivisor ix' (fromIntegral $ bInstanceDiv b)
                                                      glVertexAttribIPointer ix' x typ (fromIntegral $ bStride b) (intPtrToPtr $ fromIntegral combOffset))]
                             return (f styp $ useVInput styp n)

-- scalars

unBnorm :: Normalized t -> t
unBnorm (Normalized a) = a

instance VertexInput (B Float) where
    type VertexFormat (B Float) = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexF 1 (const S) STypeFloat GL_FLOAT
instance VertexInput (Normalized (B Int32)) where
    type VertexFormat (Normalized (B Int32)) = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 1 (const S) STypeFloat GL_INT . unBnorm
instance VertexInput (Normalized (B Word32)) where
    type VertexFormat (Normalized (B Word32)) = VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 1 (const S) STypeFloat GL_UNSIGNED_INT . unBnorm
instance VertexInput (B Int32) where
    type VertexFormat (B Int32) = VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 1 (const S) STypeInt GL_INT
instance VertexInput (B Word32) where
    type VertexFormat (B Word32) = VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 1 (const S) STypeUInt GL_UNSIGNED_INT


-- B2

instance VertexInput (B2 Float) where
    type VertexFormat (B2 Float) = V2 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexF 2 vec2S (STypeVec 2) GL_FLOAT . unB2
instance VertexInput (Normalized (B2 Int32)) where
    type VertexFormat (Normalized (B2 Int32)) = V2 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S (STypeVec 2) GL_INT . unB2 . unBnorm
instance VertexInput (Normalized (B2 Int16)) where
    type VertexFormat (Normalized (B2 Int16)) = V2 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S (STypeVec 2) GL_SHORT . unB2 . unBnorm
instance VertexInput (Normalized (B2 Word32)) where
    type VertexFormat (Normalized (B2 Word32)) = V2 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S (STypeVec 2) GL_UNSIGNED_INT . unB2 . unBnorm
instance VertexInput (Normalized (B2 Word16)) where
    type VertexFormat (Normalized (B2 Word16)) = V2 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 2 vec2S (STypeVec 2) GL_UNSIGNED_SHORT . unB2 . unBnorm
instance VertexInput (B2 Int32) where
    type VertexFormat (B2 Int32) = V2 VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S (STypeIVec 2) GL_INT . unB2
instance VertexInput (B2 Int16) where
    type VertexFormat (B2 Int16) = V2 VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S (STypeIVec 2) GL_SHORT . unB2
instance VertexInput (B2 Word32) where
    type VertexFormat (B2 Word32) = V2 VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S (STypeUVec 2) GL_UNSIGNED_INT . unB2
instance VertexInput (B2 Word16) where
    type VertexFormat (B2 Word16) = V2 VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 2 vec2S (STypeUVec 2) GL_UNSIGNED_SHORT . unB2

-- B3

instance VertexInput (B3 Float) where
    type VertexFormat (B3 Float) = V3 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexF 3 vec3S (STypeVec 3) GL_FLOAT . unB3
instance VertexInput (Normalized (B3 Int32)) where
    type VertexFormat (Normalized (B3 Int32)) = V3 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S (STypeVec 3) GL_INT . unB3 . unBnorm
instance VertexInput (Normalized (B3 Int16)) where
    type VertexFormat (Normalized (B3 Int16)) = V3 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S (STypeVec 3) GL_SHORT . unB3 . unBnorm
instance VertexInput (Normalized (B3 Int8)) where
    type VertexFormat (Normalized (B3 Int8)) = V3 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S (STypeVec 3) GL_BYTE . unB3 . unBnorm
instance VertexInput (Normalized (B3 Word32)) where
    type VertexFormat (Normalized (B3 Word32)) = V3 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S (STypeVec 3) GL_UNSIGNED_INT . unB3 . unBnorm
instance VertexInput (Normalized (B3 Word16)) where
    type VertexFormat (Normalized (B3 Word16)) = V3 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S (STypeVec 3) GL_UNSIGNED_SHORT . unB3 . unBnorm
instance VertexInput (Normalized (B3 Word8)) where
    type VertexFormat (Normalized (B3 Word8)) = V3 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 3 vec3S (STypeVec 3) GL_UNSIGNED_BYTE . unB3 . unBnorm
instance VertexInput (B3 Int32) where
    type VertexFormat (B3 Int32) = V3 VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S (STypeIVec 3) GL_INT . unB3
instance VertexInput (B3 Int16) where
    type VertexFormat (B3 Int16) = V3 VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S (STypeIVec 3) GL_SHORT . unB3
instance VertexInput (B3 Int8) where
    type VertexFormat (B3 Int8) = V3 VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S (STypeIVec 3) GL_BYTE . unB3
instance VertexInput (B3 Word32) where
    type VertexFormat (B3 Word32) = V3 VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S (STypeUVec 3) GL_UNSIGNED_INT . unB3
instance VertexInput (B3 Word16) where
    type VertexFormat (B3 Word16) = V3 VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S (STypeUVec 3) GL_UNSIGNED_SHORT . unB3
instance VertexInput (B3 Word8) where
    type VertexFormat (B3 Word8) = V3 VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 3 vec3S (STypeUVec 3) GL_UNSIGNED_BYTE . unB3

-- B4

instance VertexInput (B4 Float) where
    type VertexFormat (B4 Float) = V4 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexF 4 vec4S (STypeVec 4) GL_FLOAT . unB4
instance VertexInput (Normalized (B4 Int32)) where
    type VertexFormat (Normalized (B4 Int32)) = V4 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S (STypeVec 4) GL_INT . unB4 . unBnorm
instance VertexInput (Normalized (B4 Int16)) where
    type VertexFormat (Normalized (B4 Int16)) = V4 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S (STypeVec 4) GL_SHORT . unB4 . unBnorm
instance VertexInput (Normalized (B4 Int8)) where
    type VertexFormat (Normalized (B4 Int8)) = V4 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S (STypeVec 4) GL_BYTE . unB4 . unBnorm
instance VertexInput (Normalized (B4 Word32)) where
    type VertexFormat (Normalized (B4 Word32)) = V4 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S (STypeVec 4) GL_UNSIGNED_INT . unB4 . unBnorm
instance VertexInput (Normalized (B4 Word16)) where
    type VertexFormat (Normalized (B4 Word16)) = V4 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S (STypeVec 4) GL_UNSIGNED_SHORT . unB4 . unBnorm
instance VertexInput (Normalized (B4 Word8)) where
    type VertexFormat (Normalized (B4 Word8)) = V4 VFloat
    toVertex = ToVertex $ Kleisli $ makeVertexFnorm 4 vec4S (STypeVec 4) GL_UNSIGNED_BYTE . unB4 . unBnorm
instance VertexInput (B4 Int32) where
    type VertexFormat (B4 Int32) = V4 VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S (STypeIVec 4) GL_INT . unB4
instance VertexInput (B4 Int16) where
    type VertexFormat (B4 Int16) = V4 VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S (STypeIVec 4) GL_SHORT . unB4
instance VertexInput (B4 Int8) where
    type VertexFormat (B4 Int8) = V4 VInt
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S (STypeIVec 4) GL_BYTE . unB4
instance VertexInput (B4 Word32) where
    type VertexFormat (B4 Word32) = V4 VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S (STypeUVec 4) GL_UNSIGNED_INT . unB4
instance VertexInput (B4 Word16) where
    type VertexFormat (B4 Word16) = V4 VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S (STypeUVec 4) GL_UNSIGNED_SHORT . unB4
instance VertexInput (B4 Word8) where
    type VertexFormat (B4 Word8) = V4 VWord
    toVertex = ToVertex $ Kleisli $ makeVertexI 4 vec4S (STypeUVec 4) GL_UNSIGNED_BYTE . unB4

instance VertexInput () where
    type VertexFormat () = ()
    toVertex = arr (const ())

instance (VertexInput a, VertexInput b) => VertexInput (a,b) where
    type VertexFormat (a,b) = (VertexFormat a, VertexFormat b)
    toVertex = proc ~(a,b) -> do a' <- toVertex -< a
                                 b' <- toVertex -< b
                                 returnA -< (a', b')

instance (VertexInput a, VertexInput b, VertexInput c) => VertexInput (a,b,c) where
    type VertexFormat (a,b,c) = (VertexFormat a, VertexFormat b, VertexFormat c)
    toVertex = proc ~(a,b,c) -> do a' <- toVertex -< a
                                   b' <- toVertex -< b
                                   c' <- toVertex -< c
                                   returnA -< (a', b', c')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d) => VertexInput (a,b,c,d) where
    type VertexFormat (a,b,c,d) = (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d)
    toVertex = proc ~(a,b,c,d) -> do a' <- toVertex -< a
                                     b' <- toVertex -< b
                                     c' <- toVertex -< c
                                     d' <- toVertex -< d
                                     returnA -< (a', b', c', d')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d, VertexInput e) => VertexInput (a,b,c,d,e) where
    type VertexFormat (a,b,c,d,e) = (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d, VertexFormat e)
    toVertex = proc ~(a,b,c,d,e) -> do a' <- toVertex -< a
                                       b' <- toVertex -< b
                                       c' <- toVertex -< c
                                       d' <- toVertex -< d
                                       e' <- toVertex -< e
                                       returnA -< (a', b', c', d', e')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d, VertexInput e, VertexInput f) => VertexInput (a,b,c,d,e,f) where
    type VertexFormat (a,b,c,d,e,f) = (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d, VertexFormat e, VertexFormat f)
    toVertex = proc ~(a,b,c,d,e,f) -> do a' <- toVertex -< a
                                         b' <- toVertex -< b
                                         c' <- toVertex -< c
                                         d' <- toVertex -< d
                                         e' <- toVertex -< e
                                         f' <- toVertex -< f
                                         returnA -< (a', b', c', d', e', f')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d, VertexInput e, VertexInput f, VertexInput g) => VertexInput (a,b,c,d,e,f,g) where
    type VertexFormat (a,b,c,d,e,f,g) = (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d, VertexFormat e, VertexFormat f, VertexFormat g)
    toVertex = proc ~(a,b,c,d,e,f,g) -> do a' <- toVertex -< a
                                           b' <- toVertex -< b
                                           c' <- toVertex -< c
                                           d' <- toVertex -< d
                                           e' <- toVertex -< e
                                           f' <- toVertex -< f
                                           g' <- toVertex -< g
                                           returnA -< (a', b', c', d', e', f', g')

instance VertexInput a => VertexInput (V0 a) where
    type VertexFormat (V0 a) = V0 (VertexFormat a)
    toVertex = arr (const V0)

instance VertexInput a => VertexInput (V1 a) where
    type VertexFormat (V1 a) = V1 (VertexFormat a)
    toVertex = proc ~(V1 a) -> do a' <- toVertex -< a
                                  returnA -< V1 a'

instance VertexInput a => VertexInput (V2 a) where
    type VertexFormat (V2 a) = V2 (VertexFormat a)
    toVertex = proc ~(V2 a b) -> do a' <- toVertex -< a
                                    b' <- toVertex -< b
                                    returnA -< V2 a' b'

instance VertexInput a => VertexInput (V3 a) where
    type VertexFormat (V3 a) = V3 (VertexFormat a)
    toVertex = proc ~(V3 a b c) -> do a' <- toVertex -< a
                                      b' <- toVertex -< b
                                      c' <- toVertex -< c
                                      returnA -< V3 a' b' c'

instance VertexInput a => VertexInput (V4 a) where
    type VertexFormat (V4 a) = V4 (VertexFormat a)
    toVertex = proc ~(V4 a b c d) -> do a' <- toVertex -< a
                                        b' <- toVertex -< b
                                        c' <- toVertex -< c
                                        d' <- toVertex -< d
                                        returnA -< V4 a' b' c' d'


instance VertexInput a => VertexInput (Quaternion a) where
    type VertexFormat (Quaternion a) = Quaternion (VertexFormat a)
    toVertex = proc ~(Quaternion a v) -> do
                a' <- toVertex -< a
                v' <- toVertex -< v
                returnA -< Quaternion a' v'

instance (VertexInput (f a), VertexInput a, HostFormat (f a) ~ f (HostFormat a), VertexFormat (f a) ~ f (VertexFormat a)) => VertexInput (Point f a) where
    type VertexFormat (Point f a) = Point f (VertexFormat a)
    toVertex = proc ~(P a) -> do
                a' <- toVertex -< a
                returnA -< P a'

instance VertexInput a => VertexInput (Plucker a) where
    type VertexFormat (Plucker a) = Plucker (VertexFormat a)
    toVertex = proc ~(Plucker a b c d e f) -> do
                a' <- toVertex -< a
                b' <- toVertex -< b
                c' <- toVertex -< c
                d' <- toVertex -< d
                e' <- toVertex -< e
                f' <- toVertex -< f
                returnA -< Plucker a' b' c' d' e' f'


