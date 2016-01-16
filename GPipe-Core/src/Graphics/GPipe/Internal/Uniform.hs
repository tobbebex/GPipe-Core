{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs, TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, MultiParamTypeClasses, AllowAmbiguousTypes #-}

module Graphics.GPipe.Internal.Uniform where

import Graphics.GPipe.Internal.Buffer
import Graphics.GPipe.Internal.Shader
import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.Expr
import Control.Arrow
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Category hiding ((.))
import qualified Data.IntMap as Map
import Data.IntMap.Lazy (insert)
import Data.Word

import Graphics.GL.Core33
import Data.IORef
import Data.Int
import Linear.V4
import Linear.V3
import Linear.V2
import Linear.V1
import Linear.V0
import Linear.Plucker (Plucker(..))
import Linear.Quaternion (Quaternion(..))

-- | This class constraints which buffer types can be loaded as uniforms, and what type those values have.
class BufferFormat a => UniformInput a where
    -- | The type the buffer value will be turned into once it becomes a vertex or fragment value (the @x@ parameter is either 'V' or 'F').
    type UniformFormat a x
    -- | An arrow action that turns a value from it's buffer representation to it's vertex or fragment representation. Use 'toUniform' from
    --   the GPipe provided instances to operate in this arrow. Also note that this arrow needs to be able to return a value
    --   lazily, so ensure you use
    --
    --  @proc ~pattern -> do ...@.
    toUniform :: ToUniform x a (UniformFormat a x)

-- | Load a uniform value from a 'Buffer' into a 'Shader'. The argument function is used to retrieve the buffer and the index into this buffer from the shader environment.
getUniform :: forall os f s b x. (UniformInput b) => (s -> (Buffer os (Uniform b), Int)) -> Shader os f s (UniformFormat b x)
getUniform sf = Shader $ do
                   uniAl <- askUniformAlignment
                   blockId <- getName
                   let (u, offToStype) = shaderGen (useUniform (buildUDecl offToStype) blockId)
                       sampleBuffer = makeBuffer undefined undefined uniAl :: Buffer os (Uniform b)
                       shaderGen :: (Int -> ExprM String) -> (UniformFormat b x, OffsetToSType) -- Int is name of uniform block
                       shaderGen = runReader $ runWriterT $ shaderGenF $ fromBUnifom $ bufBElement sampleBuffer $ BInput 0 0
                   doForUniform blockId $ \s bind -> let (ub, i) = sf s
                                                     in if i < 0 || i >= bufferLength ub
                                                            then error "toUniformBlock, uniform buffer offset out of bounds"
                                                            else do
                                                                bname <- readIORef $ bufName ub
                                                                glBindBufferRange GL_UNIFORM_BUFFER (fromIntegral bind) bname (fromIntegral $ i * bufElementSize ub) (fromIntegral $ bufElementSize ub)
                   return u
    where
            ToUniform (Kleisli shaderGenF) = toUniform :: ToUniform x b (UniformFormat b x)
            fromBUnifom (Uniform b) = b

            doForUniform :: Int -> (s -> Binding -> IO()) -> ShaderM s ()
            doForUniform n io = modifyRenderIO (\s -> s { uniformNameToRenderIO = insert n io (uniformNameToRenderIO s) } )

buildUDecl :: OffsetToSType -> GlobDeclM ()
buildUDecl = buildUDecl' 0 . Map.toAscList
    where buildUDecl' p xxs@((off, stype):xs) | off == p = do tellGlobal $ stypeName stype
                                                              tellGlobal " u"
                                                              tellGlobalLn $ show off
                                                              buildUDecl' (p + stypeSize stype) xs
                                              | off > p = do tellGlobal "float pad"
                                                             tellGlobalLn $ show p
                                                             buildUDecl' (p + 4) xxs
                                              | otherwise = error "buildUDecl: Expected all offsets to be multiple of 4"
          buildUDecl' _ [] = return ()

type OffsetToSType = Map.IntMap SType

-- | The arrow type for 'toUniform'.
newtype ToUniform x a b = ToUniform (Kleisli (WriterT OffsetToSType (Reader (Int -> ExprM String))) a b) deriving (Category, Arrow)

makeUniform :: SType -> ToUniform x (B a) (S x b)
makeUniform styp = ToUniform $ Kleisli $ \bIn -> do let offset = bOffset bIn
                                                    tell $ Map.singleton offset styp
                                                    useF <- lift ask
                                                    return $ S $ useF offset

instance UniformInput (B Float) where
    type UniformFormat (B Float) x = (S x Float)
    toUniform = makeUniform STypeFloat

instance UniformInput (B Int32) where
    type UniformFormat (B Int32) x = (S x Int)
    toUniform = makeUniform STypeInt

instance UniformInput (B Word32) where
    type UniformFormat (B Word32) x = (S x Word)
    toUniform = makeUniform STypeUInt

instance UniformInput (B2 Float) where
    type UniformFormat (B2 Float) x = V2 (S x Float)
    toUniform = arr unB2 >>> makeUniform (STypeVec 2) >>> arr vec2S''

instance UniformInput (B2 Int32) where
    type UniformFormat (B2 Int32) x = V2 (S x Int)
    toUniform = arr unB2 >>> makeUniform (STypeIVec 2) >>> arr vec2S''

instance UniformInput (B2 Word32) where
    type UniformFormat (B2 Word32) x = V2 (S x Word)
    toUniform = arr unB2 >>> makeUniform (STypeVec 2) >>> arr vec2S''

instance UniformInput (B3 Float) where
    type UniformFormat (B3 Float) x = V3 (S x Float)
    toUniform = arr unB3 >>> makeUniform (STypeVec 3) >>> arr vec3S''

instance UniformInput (B3 Int32) where
    type UniformFormat (B3 Int32) x = V3 (S x Int)
    toUniform = arr unB3 >>> makeUniform (STypeIVec 3) >>> arr vec3S''

instance UniformInput (B3 Word32) where
    type UniformFormat (B3 Word32) x = V3 (S x Word)
    toUniform = arr unB3 >>> makeUniform (STypeVec 3) >>> arr vec3S''

instance UniformInput (B4 Float) where
    type UniformFormat (B4 Float) x = V4 (S x Float)
    toUniform = arr unB4 >>> makeUniform (STypeVec 4) >>> arr vec4S''

instance UniformInput (B4 Int32) where
    type UniformFormat (B4 Int32) x = V4 (S x Int)
    toUniform = arr unB4 >>> makeUniform (STypeIVec 4) >>> arr vec4S''

instance UniformInput (B4 Word32) where
    type UniformFormat (B4 Word32) x = V4 (S x Word)
    toUniform = arr unB4 >>> makeUniform (STypeVec 4) >>> arr vec4S''


instance UniformInput a => UniformInput (V0 a) where
    type UniformFormat (V0 a) x = V0 (UniformFormat a x)
    toUniform = arr (const V0)

instance UniformInput a => UniformInput (V1 a) where
    type UniformFormat (V1 a) x = V1 (UniformFormat a x)
    toUniform = proc ~(V1 a) -> do a' <- toUniform -< a
                                   returnA -< V1 a'

instance UniformInput a => UniformInput (V2 a) where
    type UniformFormat (V2 a) x = V2 (UniformFormat a x)
    toUniform = proc ~(V2 a b) -> do a' <- toUniform -< a
                                     b' <- toUniform -< b
                                     returnA -< V2 a' b'

instance UniformInput a => UniformInput (V3 a) where
    type UniformFormat (V3 a) x = V3 (UniformFormat a x)
    toUniform = proc ~(V3 a b c) -> do a' <- toUniform -< a
                                       b' <- toUniform -< b
                                       c' <- toUniform -< c
                                       returnA -< V3 a' b' c'

instance UniformInput a => UniformInput (V4 a)  where
    type UniformFormat (V4 a) x = V4 (UniformFormat a x)
    toUniform = proc ~(V4 a b c d) -> do a' <- toUniform -< a
                                         b' <- toUniform -< b
                                         c' <- toUniform -< c
                                         d' <- toUniform -< d
                                         returnA -< V4 a' b' c' d'

instance UniformInput () where
    type UniformFormat () x = ()
    toUniform = arr (const ())

instance (UniformInput a, UniformInput b) => UniformInput (a,b) where
    type UniformFormat (a,b) x = (UniformFormat a x, UniformFormat b x)
    toUniform = proc ~(a,b) -> do a' <- toUniform -< a
                                  b' <- toUniform -< b
                                  returnA -< (a', b')

instance (UniformInput a, UniformInput b, UniformInput c) => UniformInput (a,b,c) where
    type UniformFormat (a,b,c) x = (UniformFormat a x, UniformFormat b x, UniformFormat c x)
    toUniform = proc ~(a,b,c) -> do a' <- toUniform -< a
                                    b' <- toUniform -< b
                                    c' <- toUniform -< c
                                    returnA -< (a', b', c')

instance (UniformInput a, UniformInput b, UniformInput c, UniformInput d) => UniformInput (a,b,c,d) where
    type UniformFormat (a,b,c,d) x = (UniformFormat a x, UniformFormat b x, UniformFormat c x, UniformFormat d x)
    toUniform = proc ~(a,b,c,d) -> do a' <- toUniform -< a
                                      b' <- toUniform -< b
                                      c' <- toUniform -< c
                                      d' <- toUniform -< d
                                      returnA -< (a', b', c', d')

instance (UniformInput a, UniformInput b, UniformInput c, UniformInput d, UniformInput e) => UniformInput (a,b,c,d,e) where
    type UniformFormat (a,b,c,d,e) x = (UniformFormat a x, UniformFormat b x, UniformFormat c x, UniformFormat d x, UniformFormat e x)
    toUniform = proc ~(a,b,c,d,e) -> do a' <- toUniform -< a
                                        b' <- toUniform -< b
                                        c' <- toUniform -< c
                                        d' <- toUniform -< d
                                        e' <- toUniform -< e
                                        returnA -< (a', b', c', d', e')

instance (UniformInput a, UniformInput b, UniformInput c, UniformInput d, UniformInput e, UniformInput f) => UniformInput (a,b,c,d,e,f) where
    type UniformFormat (a,b,c,d,e,f) x = (UniformFormat a x, UniformFormat b x, UniformFormat c x, UniformFormat d x, UniformFormat e x, UniformFormat f x)
    toUniform = proc ~(a,b,c,d,e,f) -> do a' <- toUniform -< a
                                          b' <- toUniform -< b
                                          c' <- toUniform -< c
                                          d' <- toUniform -< d
                                          e' <- toUniform -< e
                                          f' <- toUniform -< f
                                          returnA -< (a', b', c', d', e', f')

instance (UniformInput a, UniformInput b, UniformInput c, UniformInput d, UniformInput e, UniformInput f, UniformInput g) => UniformInput (a,b,c,d,e,f,g) where
    type UniformFormat (a,b,c,d,e,f,g) x = (UniformFormat a x, UniformFormat b x, UniformFormat c x, UniformFormat d x, UniformFormat e x, UniformFormat f x, UniformFormat g x)
    toUniform = proc ~(a,b,c,d,e,f,g) -> do a' <- toUniform -< a
                                            b' <- toUniform -< b
                                            c' <- toUniform -< c
                                            d' <- toUniform -< d
                                            e' <- toUniform -< e
                                            f' <- toUniform -< f
                                            g' <- toUniform -< g
                                            returnA -< (a', b', c', d', e', f', g')


instance UniformInput a => UniformInput (Quaternion a) where
    type UniformFormat (Quaternion a) x = Quaternion (UniformFormat a x)
    toUniform = proc ~(Quaternion a v) -> do
                    a' <- toUniform -< a
                    v' <- toUniform -< v
                    returnA -< Quaternion a' v'

instance UniformInput a => UniformInput (Plucker a) where
    type UniformFormat (Plucker a) x = Plucker (UniformFormat a x)
    toUniform = proc ~(Plucker a b c d e f) -> do
                    a' <- toUniform -< a
                    b' <- toUniform -< b
                    c' <- toUniform -< c
                    d' <- toUniform -< d
                    e' <- toUniform -< e
                    f' <- toUniform -< f
                    returnA -< Plucker a' b' c' d' e' f'


