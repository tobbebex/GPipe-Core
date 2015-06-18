{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving  #-}
module Graphics.GPipe.FragmentStream where

import Control.Category hiding ((.))
import Control.Arrow 
import Graphics.GPipe.Shader
import Graphics.GPipe.VertexStream
import Control.Monad.Trans.State.Lazy
import Data.Monoid (Monoid)
import Data.Boolean

type VPos = (VFloat,VFloat,VFloat,VFloat)

data Side = Front | Back | FrontAndBack
type ShaderPos = ShaderM ()
data FragmentStreamData = FragmentStreamData Side ShaderPos PrimitiveStreamData FBool

newtype FragmentStream a = FragmentStream [(a, FragmentStreamData)] deriving Monoid

instance Functor FragmentStream where
        fmap f (FragmentStream xs) = FragmentStream $ map (first f) xs
 

newtype ToFragment a b = ToFragment (Kleisli (State Int) a b) deriving (Category, Arrow)

class FragmentInput a where
    type FragmentFormat a
    toFragment :: ToFragment a (FragmentFormat a)  
    
instance FragmentInput VFloat where
        type FragmentFormat VFloat = FFloat
        toFragment = ToFragment $ Kleisli $ \ (S s) -> do n <- get
                                                          put (n+1)
                                                          return $ S $ useFInput "vf" STypeFloat n s 
               

rasterize:: forall p a. (PrimitiveTopology p, FragmentInput a)
          => Side 
          -> PrimitiveStream p (VPos, a)
          -> FragmentStream (FragmentFormat a) -- FBool {-- TODO make struct of all sorts of stuff --}, 
rasterize side (PrimitiveStream xs) = 
    let ToFragment (Kleisli m) = toFragment :: ToFragment a (FragmentFormat a)
        f ((p, x),s) = (evalState (m x) 0, FragmentStreamData side (makePos p) s true)
        makePos (S x,S y,S z,S w) = do x' <- x
                                       y' <- y
                                       z' <- z
                                       w' <- w
                                       tellAssignment' "gl_Position" $ "vec4("++x'++',':y'++',':z'++',':w'++")"
    in FragmentStream $ map f xs

-- TODO: Add scissor and viewport
 
filterFragments :: (a -> FBool) -> FragmentStream a -> FragmentStream a 
filterFragments f (FragmentStream xs) = FragmentStream $ map g xs
    where g (a,FragmentStreamData x y z w) = (a,FragmentStreamData x y z (w &&* f a))  































