{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving  #-}
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

type VPos = (VFloat,VFloat,VFloat,VFloat)

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
    
instance FragmentInput VFloat where
        type FragmentFormat VFloat = FFloat
        toFragment = ToFragment $ Kleisli $ \ (S s) -> do n <- get
                                                          put (n+1)
                                                          return $ S $ useFInput "vf" STypeFloat n s 
               

rasterize:: forall p a s os f. (PrimitiveTopology p, FragmentInput a)
          => (s -> (Side, ViewPort))
          -> PrimitiveStream p (VPos, a)
          -> Shader os f s (FragmentStream (FragmentFormat a)) 
rasterize sf (PrimitiveStream xs) = Shader $ do
        n <- getName
        modifyRenderIO (\s -> s { rasterizationNameToRenderIO = insert n io (rasterizationNameToRenderIO s) } )
        return (FragmentStream $ map (f n) xs) 
    where        
        ToFragment (Kleisli m) = toFragment :: ToFragment a (FragmentFormat a)
        f n ((p, x),s) = (evalState (m x) 0, FragmentStreamData n (makePos p) s true)
        makePos (S x,S y,S z,S w) = do x' <- x
                                       y' <- y
                                       z' <- z
                                       w' <- w
                                       tellAssignment' "gl_Position" $ "vec4("++x'++',':y'++',':z'++',':w'++")"
        io s = let (side, vp) = sf s in putStrLn "glSetSideAndViewport"

data ViewPort = ViewPort (Int,Int) (Int,Int)

-- TODO: Add scissor and viewport
 
filterFragments :: (a -> FBool) -> FragmentStream a -> FragmentStream a 
filterFragments f (FragmentStream xs) = FragmentStream $ map g xs
    where g (a,FragmentStreamData x y z w) = (a,FragmentStreamData x y z (w &&* f a))  


-- TODO: Add withRasterizedInfo to add depth, side, etc
 -- FBool {-- TODO make struct of all sorts of stuff --},

























