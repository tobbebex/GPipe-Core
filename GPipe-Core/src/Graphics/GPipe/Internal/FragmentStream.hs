{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, FlexibleContexts  #-}
module Graphics.GPipe.Internal.FragmentStream where

import Control.Category hiding ((.))
import Control.Arrow
import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.Shader
import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.PrimitiveStream
import Graphics.GPipe.Internal.PrimitiveArray
import Control.Monad.Trans.State.Lazy
import Data.Monoid (Monoid)
import Data.Boolean
import Data.IntMap.Lazy (insert)
import Linear.V4
import Linear.V3
import Linear.V2
import Linear.V1
import Linear.V0
import Linear.Plucker (Plucker(..))
import Linear.Quaternion (Quaternion(..))
import Linear.Affine (Point(..))

import Graphics.GL.Core33
import Control.Monad (when)
import Data.Maybe (isJust)

type VPos = V4 VFloat

type ExprPos = ExprM ()
type RasterizationName = Int
data FragmentStreamData = FragmentStreamData RasterizationName ExprPos PrimitiveStreamData FBool

-- | A @'FragmentStream' a @ is a stream of fragments of type @a@. You may append 'FragmentStream's using the 'Monoid' instance, and you
--   can operate a stream's values using the 'Functor' instance (this will result in a shader running on the GPU).
newtype FragmentStream a = FragmentStream [(a, FragmentStreamData)] deriving Monoid

instance Functor FragmentStream where
        fmap f (FragmentStream xs) = FragmentStream $ map (first f) xs

-- | The arrow type for 'toFragment'.
newtype ToFragment a b = ToFragment (Kleisli (State Int) a b) deriving (Category, Arrow)

-- | This class constraints which vertex types can be turned into fragment values, and what type those values have.
class FragmentInput a where
    -- | The type the vertex value will be turned into once it becomes a fragment value.
    type FragmentFormat a
    -- | An arrow action that turns a value from it's vertex representation to it's fragment representation. Use 'toFragment' from
    --   the GPipe provided instances to operate in this arrow. Also note that this arrow needs to be able to return a value
    --   lazily, so ensure you use
    --
    --  @proc ~pattern -> do ...@.
    toFragment :: ToFragment a (FragmentFormat a)

-- | Rasterize a stream of primitives into fragments, using a 'Side', 'Viewport' and 'DepthRange' from the shader environment.
--   Primitives will be transformed from canonical view space, i.e. [(-1,-1,-1),(1,1,1)], to the 2D space defined by the 'ViewPort' parameter and the depth range
--   defined by the 'DepthRange' parameter.
rasterize:: forall p a s os f. FragmentInput a
          => (s -> (Side, ViewPort, DepthRange))
          -> PrimitiveStream p (VPos, a)
          -> Shader os f s (FragmentStream (FragmentFormat a))
rasterize sf (PrimitiveStream xs) = Shader $ do
        n <- getName
        modifyRenderIO (\s -> s { rasterizationNameToRenderIO = insert n io (rasterizationNameToRenderIO s) } )
        return (FragmentStream $ map (f n) xs)
    where
        ToFragment (Kleisli m) = toFragment :: ToFragment a (FragmentFormat a)
        f n ((p, x),(ps, s)) = (evalState (m x) 0, FragmentStreamData n (makePos p >> makePointSize ps) s true)
        makePos (V4 (S x) (S y) (S z) (S w)) = do
                                       x' <- x
                                       y' <- y
                                       z' <- z
                                       w' <- w
                                       tellAssignment' "gl_Position" $ "vec4("++x'++',':y'++',':z'++',':w'++")"
        makePointSize Nothing = return ()
        makePointSize (Just (S ps)) = ps >>= tellAssignment' "gl_PointSize"
        io s = let (side, ViewPort (V2 x y) (V2 w h), DepthRange dmin dmax) = sf s in if w < 0 || h < 0
                                                                                        then error "ViewPort, negative size"
                                                                                        else do setGlCullFace side
                                                                                                glScissor (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
                                                                                                glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
                                                                                                glDepthRange (realToFrac dmin) (realToFrac dmax)
                                                                                                setGLPointSize

        setGlCullFace Front = glEnable GL_CULL_FACE >> glCullFace GL_BACK -- Back is culled when front is rasterized
        setGlCullFace Back = glEnable GL_CULL_FACE >> glCullFace GL_FRONT
        setGlCullFace _ = glDisable GL_CULL_FACE
        setGLPointSize = if any (isJust.fst.snd) xs then glEnable GL_PROGRAM_POINT_SIZE else glDisable GL_PROGRAM_POINT_SIZE

-- | Defines which side to rasterize. Non triangle primitives only has a front side.
data Side = Front | Back | FrontAndBack
-- | The viewport in pixel coordinates (where (0,0) is the lower left corner) in to which the canonical view volume [(-1,-1,-1),(1,1,1)] is transformed and clipped/scissored.
data ViewPort = ViewPort { viewPortLowerLeft :: V2 Int, viewPortSize :: V2 Int }
-- | The fragment depth range to map the canonical view volume's z-coordinate to. Depth values are clamped to [0,1], so @DepthRange 0 1@ gives maximum depth resolution.
data DepthRange = DepthRange { minDepth :: Float, maxDepth :: Float }

-- | Filter out fragments from the stream where the predicate in the first argument evaluates to 'true', and discard all other fragments.
filterFragments :: (a -> FBool) -> FragmentStream a -> FragmentStream a
filterFragments f (FragmentStream xs) = FragmentStream $ map g xs
    where g (a,FragmentStreamData x y z w) = (a,FragmentStreamData x y z (w &&* f a))

data RasterizedInfo = RasterizedInfo {
        rasterizedFragCoord :: V4 FFloat,
        rasterizedFrontFacing :: FBool,
        rasterizedPointCoord :: V2 FFloat
    }

-- | Like 'fmap', but where various auto generated information from the rasterization is provided for each vertex.
withRasterizedInfo :: (a -> RasterizedInfo -> b) -> FragmentStream a -> FragmentStream b
withRasterizedInfo f = fmap (\a -> f a (RasterizedInfo (vec4S' "gl_FragCoord") (scalarS' "gl_FrontFacing") (vec2S' "gl_PointCoord")))

-- | A float value that is not interpolated (like integers), and all fragments will instead get the value of the primitive's last vertex
data FlatVFloat = Flat VFloat
-- | A float value that doesn't get divided by the interpolated position's w-component during interpolation.
data NoPerspectiveVFloat = NoPerspective VFloat

makeFragment :: String -> SType -> (a -> ExprM String) -> ToFragment a (S c a1)
makeFragment qual styp f = ToFragment $ Kleisli $ \ x -> do n <- get
                                                            put (n+1)
                                                            return $ S $ useFInput qual "vf" styp n $ f x
unFlat :: FlatVFloat -> VFloat
unFlat (Flat s) = s
unNPersp :: NoPerspectiveVFloat -> VFloat
unNPersp (NoPerspective s) = s

instance FragmentInput () where
    type FragmentFormat () = ()
    toFragment = arr (const ())

instance FragmentInput VFloat where
        type FragmentFormat VFloat = FFloat
        toFragment = makeFragment "" STypeFloat unS

instance FragmentInput FlatVFloat where
        type FragmentFormat FlatVFloat = FFloat
        toFragment = makeFragment "flat" STypeFloat (unS . unFlat)

instance FragmentInput NoPerspectiveVFloat where
        type FragmentFormat NoPerspectiveVFloat = FFloat
        toFragment = makeFragment "noperspective" STypeFloat (unS . unNPersp)

instance FragmentInput VInt where
        type FragmentFormat VInt = FInt
        toFragment = makeFragment "flat" STypeInt unS

instance FragmentInput VWord where
        type FragmentFormat VWord = FWord
        toFragment = makeFragment "flat" STypeUInt unS

instance FragmentInput VBool where
        type FragmentFormat VBool = FBool
        toFragment = proc b -> do i <- toFragment -< ifB b 1 0 :: VInt
                                  returnA -< i ==* 1

instance (FragmentInput a) => FragmentInput (V0 a) where
    type FragmentFormat (V0 a) = V0 (FragmentFormat a)
    toFragment = arr (const V0)

instance (FragmentInput a) => FragmentInput (V1 a) where
    type FragmentFormat (V1 a) = V1 (FragmentFormat a)
    toFragment = proc ~(V1 a) -> do a' <- toFragment -< a
                                    returnA -< V1 a'

instance (FragmentInput a) => FragmentInput (V2 a) where
    type FragmentFormat (V2 a) = V2 (FragmentFormat a)
    toFragment = proc ~(V2 a b) -> do a' <- toFragment -< a
                                      b' <- toFragment -< b
                                      returnA -< V2 a' b'

instance (FragmentInput a) => FragmentInput (V3 a) where
    type FragmentFormat (V3 a) = V3 (FragmentFormat a)
    toFragment = proc ~(V3 a b c) -> do a' <- toFragment -< a
                                        b' <- toFragment -< b
                                        c' <- toFragment -< c
                                        returnA -< V3 a' b' c'

instance (FragmentInput a) => FragmentInput (V4 a) where
    type FragmentFormat (V4 a) = V4 (FragmentFormat a)
    toFragment = proc ~(V4 a b c d) -> do a' <- toFragment -< a
                                          b' <- toFragment -< b
                                          c' <- toFragment -< c
                                          d' <- toFragment -< d
                                          returnA -< V4 a' b' c' d'

instance (FragmentInput a, FragmentInput b) => FragmentInput (a,b) where
    type FragmentFormat (a,b) = (FragmentFormat a, FragmentFormat b)
    toFragment = proc ~(a,b) -> do a' <- toFragment -< a
                                   b' <- toFragment -< b
                                   returnA -< (a', b')

instance (FragmentInput a, FragmentInput b, FragmentInput c) => FragmentInput (a,b,c) where
    type FragmentFormat (a,b,c) = (FragmentFormat a, FragmentFormat b, FragmentFormat c)
    toFragment = proc ~(a,b,c) -> do a' <- toFragment -< a
                                     b' <- toFragment -< b
                                     c' <- toFragment -< c
                                     returnA -< (a', b', c')

instance (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d) => FragmentInput (a,b,c,d) where
    type FragmentFormat (a,b,c,d) = (FragmentFormat a, FragmentFormat b, FragmentFormat c, FragmentFormat d)
    toFragment = proc ~(a,b,c,d) -> do a' <- toFragment -< a
                                       b' <- toFragment -< b
                                       c' <- toFragment -< c
                                       d' <- toFragment -< d
                                       returnA -< (a', b', c', d')

instance (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d, FragmentInput e) => FragmentInput (a,b,c,d,e) where
    type FragmentFormat (a,b,c,d,e) = (FragmentFormat a, FragmentFormat b, FragmentFormat c, FragmentFormat d, FragmentFormat e)
    toFragment = proc ~(a,b,c,d,e) -> do a' <- toFragment -< a
                                         b' <- toFragment -< b
                                         c' <- toFragment -< c
                                         d' <- toFragment -< d
                                         e' <- toFragment -< e
                                         returnA -< (a', b', c', d', e')

instance (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d, FragmentInput e, FragmentInput f) => FragmentInput (a,b,c,d,e,f) where
    type FragmentFormat (a,b,c,d,e,f) = (FragmentFormat a, FragmentFormat b, FragmentFormat c, FragmentFormat d, FragmentFormat e, FragmentFormat f)
    toFragment = proc ~(a,b,c,d,e,f) -> do a' <- toFragment -< a
                                           b' <- toFragment -< b
                                           c' <- toFragment -< c
                                           d' <- toFragment -< d
                                           e' <- toFragment -< e
                                           f' <- toFragment -< f
                                           returnA -< (a', b', c', d', e', f')

instance (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d, FragmentInput e, FragmentInput f, FragmentInput g) => FragmentInput (a,b,c,d,e,f,g) where
    type FragmentFormat (a,b,c,d,e,f,g) = (FragmentFormat a, FragmentFormat b, FragmentFormat c, FragmentFormat d, FragmentFormat e, FragmentFormat f, FragmentFormat g)
    toFragment = proc ~(a,b,c,d,e,f,g) -> do a' <- toFragment -< a
                                             b' <- toFragment -< b
                                             c' <- toFragment -< c
                                             d' <- toFragment -< d
                                             e' <- toFragment -< e
                                             f' <- toFragment -< f
                                             g' <- toFragment -< g
                                             returnA -< (a', b', c', d', e', f', g')

instance FragmentInput a => FragmentInput (Quaternion a) where
    type FragmentFormat (Quaternion a) = Quaternion (FragmentFormat a)
    toFragment = proc ~(Quaternion a v) -> do
                a' <- toFragment -< a
                v' <- toFragment -< v
                returnA -< Quaternion a' v'

instance (FragmentInput (f a), FragmentInput a, FragmentFormat (f a) ~ f (FragmentFormat a)) => FragmentInput (Point f a) where
    type FragmentFormat (Point f a) = Point f (FragmentFormat a)
    toFragment = proc ~(P a) -> do
                a' <- toFragment -< a
                returnA -< P a'

instance FragmentInput a => FragmentInput (Plucker a) where
    type FragmentFormat (Plucker a) = Plucker (FragmentFormat a)
    toFragment = proc ~(Plucker a b c d e f) -> do
                a' <- toFragment -< a
                b' <- toFragment -< b
                c' <- toFragment -< c
                d' <- toFragment -< d
                e' <- toFragment -< e
                f' <- toFragment -< f
                returnA -< Plucker a' b' c' d' e' f'
