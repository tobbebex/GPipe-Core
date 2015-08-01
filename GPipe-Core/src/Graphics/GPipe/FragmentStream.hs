{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows  #-}
module Graphics.GPipe.FragmentStream where

import Control.Category hiding ((.))
import Control.Arrow 
import Graphics.GPipe.Expr
import Graphics.GPipe.Shader
import Graphics.GPipe.Compiler
import Graphics.GPipe.PrimitiveStream
import Graphics.GPipe.PrimitiveArray
import Control.Monad.Trans.State.Lazy
import Data.Monoid (Monoid)
import Data.Boolean
import Data.IntMap.Lazy (insert)

import Graphics.Rendering.OpenGL.Raw.Core33

type VPos = (VFloat, VFloat, VFloat, VFloat)

data Side = Front | Back | FrontAndBack
type ExprPos = ExprM ()
type RasterizationName = Int
data FragmentStreamData = FragmentStreamData RasterizationName ExprPos PrimitiveStreamData FBool

newtype FragmentStream a = FragmentStream [(a, FragmentStreamData)] deriving Monoid

instance Functor FragmentStream where
        fmap f (FragmentStream xs) = FragmentStream $ map (first f) xs
 
newtype ToFragment a b = ToFragment (Kleisli (State Int) a b) deriving (Category, Arrow)

class FragmentInput a where
    type FragmentFormat a
    toFragment :: ToFragment a (FragmentFormat a)  
    
rasterize:: forall p a s os f. (PrimitiveTopology p, FragmentInput a)
          => (s -> (Side, ViewPort, DepthRange))
          -> PrimitiveStream p (VPos, a)
          -> Shader os f s (FragmentStream (FragmentFormat a)) 
rasterize sf (PrimitiveStream xs) = Shader $ do
        n <- getName
        modifyRenderIO (\s -> s { rasterizationNameToRenderIO = insert n io (rasterizationNameToRenderIO s) } )
        return (FragmentStream $ map (f n) xs) 
    where        
        ToFragment (Kleisli m) = toFragment :: ToFragment a (FragmentFormat a)
        f n ((p, x),s) = (evalState (m x) 0, FragmentStreamData n (makePos p) s true)
        makePos (S x, S y, S z, S w) = do
                                       x' <- x
                                       y' <- y
                                       z' <- z
                                       w' <- w
                                       tellAssignment' "gl_Position" $ "vec4("++x'++',':y'++',':z'++',':w'++")"
        io s = let (side, ViewPort (x,y) (w,h), DepthRange dmin dmax) = sf s in do setGlCullFace side
                                                                                   glScissor (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 
                                                                                   glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 
                                                                                   glDepthRange (realToFrac dmin) (realToFrac dmax)

        setGlCullFace Front = glEnable gl_CULL_FACE >> glCullFace gl_BACK -- Back is culled when front is rasterized
        setGlCullFace Back = glEnable gl_CULL_FACE >> glCullFace gl_FRONT
        setGlCullFace _ = glDisable gl_CULL_FACE

data ViewPort = ViewPort { viewPortLowerLeft :: (Int,Int), viewPortSize :: (Int,Int) }
data DepthRange = DepthRange { minDepth :: Float, maxDepth :: Float }
 
filterFragments :: (a -> FBool) -> FragmentStream a -> FragmentStream a 
filterFragments f (FragmentStream xs) = FragmentStream $ map g xs
    where g (a,FragmentStreamData x y z w) = (a,FragmentStreamData x y z (w &&* f a))  

data RasterizedInfo = RasterizedInfo {
        rasterizedFragCoord :: (FFloat, FFloat, FFloat, FFloat),
        rasterizedFrontFacing :: FBool,
        rasterizedPointCoord :: (FFloat, FFloat)
    }       

withRasterizedInfo :: FragmentStream a -> FragmentStream (a, RasterizedInfo)
withRasterizedInfo = fmap (\a -> (a, RasterizedInfo (vec4S' "gl_FragCoord") (scalarS' "gl_FrontFacing") (vec2S' "gl_PointCoord")) )

data Flat a = Flat a
data NoPerspective a = NoPerspective a

makeFragment :: String -> SType -> (a -> ExprM String) -> ToFragment a (S c a1)
makeFragment qual styp f = ToFragment $ Kleisli $ \ x -> do n <- get
                                                            put (n+1)
                                                            return $ S $ useFInput qual "vf" styp n $ f x
unFlat :: Flat t -> t
unFlat (Flat s) = s
unNPersp :: NoPerspective t -> t
unNPersp (NoPerspective s) = s

instance FragmentInput VFloat where
        type FragmentFormat VFloat = FFloat
        toFragment = makeFragment "" STypeFloat unS
         
instance FragmentInput (Flat VFloat) where
        type FragmentFormat (Flat VFloat) = FFloat
        toFragment = makeFragment "flat" STypeFloat (unS . unFlat)

instance FragmentInput (NoPerspective VFloat) where
        type FragmentFormat (NoPerspective VFloat) = FFloat
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
        
instance (FragmentInput a, FragmentInput b) => FragmentInput (a,b) where
    type FragmentFormat (a,b) = (FragmentFormat a, FragmentFormat b)
    toFragment = proc (a,b) -> do a' <- toFragment -< a
                                  b' <- toFragment -< b
                                  returnA -< (a', b')

instance (FragmentInput a, FragmentInput b, FragmentInput c) => FragmentInput (a,b,c) where
    type FragmentFormat (a,b,c) = (FragmentFormat a, FragmentFormat b, FragmentFormat c)
    toFragment = proc (a,b,c) -> do a' <- toFragment -< a
                                    b' <- toFragment -< b
                                    c' <- toFragment -< c
                                    returnA -< (a', b', c')

instance (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d) => FragmentInput (a,b,c,d) where
    type FragmentFormat (a,b,c,d) = (FragmentFormat a, FragmentFormat b, FragmentFormat c, FragmentFormat d)
    toFragment = proc (a,b,c,d) -> do a' <- toFragment -< a
                                      b' <- toFragment -< b
                                      c' <- toFragment -< c
                                      d' <- toFragment -< d
                                      returnA -< (a', b', c', d')

