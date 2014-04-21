{-# LANGUAGE FlexibleInstances, TypeFamilies, FlexibleContexts #-}

module Graphics.GPipe.Shader.Operations where

import Graphics.GPipe.Shader
import Data.Boolean

--noFun :: String -> a
--noFun = error . (++ ": No overloading for shader type S")

bin :: SType -> String -> S c x -> S c y -> S c z 
bin typ o (S a) (S b) = S $ do a' <- a
                               b' <- b
                               tellAssignment typ $ '(' : a' ++ o ++ b' ++ ")"

fun1 :: SType -> String -> S c x -> S c y
fun1 typ f (S a) = S $ do a' <- a
                          tellAssignment typ $ f ++ '(' : a' ++ ")"

fun2 :: SType -> String -> S c x -> S c y -> S c z
fun2 typ f (S a) (S b) = S $ do a' <- a
                                b' <- b
                                tellAssignment typ $ f ++ '(' : a' ++ ',' : b' ++ ")"

fun3 :: SType -> String -> S c x -> S c y -> S c z -> S c w
fun3 typ f (S a) (S b) (S c) = S $ do a' <- a
                                      b' <- b
                                      c' <- c
                                      tellAssignment typ $ f ++ '(' : a' ++ ',' : b' ++ ',' : c' ++")"

postop :: SType -> String -> S c x -> S c y
postop typ f (S a) = S $ do a' <- a
                            tellAssignment typ $ '(' : f ++ a' ++ ")"
                          
preop :: SType -> String -> S c x -> S c y
preop typ f (S a) = S $ do a' <- a
                           tellAssignment typ $ '(' : a' ++ f ++ ")"

binf :: String -> S c x -> S c y -> S c Float
binf = bin STypeFloat
fun1f :: String -> S c x -> S c Float
fun1f = fun1 STypeFloat
fun2f :: String -> S c x -> S c y -> S c Float
fun2f = fun2 STypeFloat
fun3f :: String -> S c x -> S c y -> S c z -> S c Float
fun3f = fun3 STypeFloat
preopf :: String -> S c x -> S c Float
preopf = preop STypeFloat
postopf :: String -> S c x -> S c Float
postopf = postop STypeFloat

instance Num (S a Float) where
    (+) = binf "+"
    (-) = binf "-"
    abs = fun1f "abs"
    signum = fun1f "sign"
    (*) = binf "*"
    fromInteger = S . return . show
    negate = preopf "-"

instance Fractional (S a Float) where
  (/)          = binf "/"
  fromRational = S . return . show

instance Floating (S a Float) where
  pi    = S $ return $ show (pi :: Float)
  sqrt  = fun1f "sqrt"
  exp   = fun1f "exp"
  log   = fun1f "log"
  (**)  = fun2f "pow"
  sin   = fun1f "sin"
  cos   = fun1f "cos"
  tan   = fun1f "tan"
  asin  = fun1f "asin"
  acos  = fun1f "acos"
  atan  = fun1f "atan"
  sinh  = fun1f "sinh"
  cosh  = fun1f "cosh"
  asinh = fun1f "asinh"
  atanh = fun1f "atanh"
  acosh = fun1f "acosh"

-- | This class provides the GPU functions either not found in Prelude's numerical classes, or that has wrong types.
--   Instances are also provided for normal 'Float's and 'Double's.
--   Minimal complete definition: 'floor'' and 'ceiling''.
class (IfB (RealBool a), IfB a, OrdB (RealBool a), OrdB a, Floating a) => Real' a where
  type RealBool a
  rsqrt :: a -> a
  exp2 :: a -> a
  log2 :: a -> a
  floor' :: a -> a
  ceiling' :: a -> a
  fract' :: a -> a
  mod' :: a -> a -> a
  clamp :: a -> a -> a -> a
  saturate :: a -> a
  mix :: a -> a -> a-> a
  step :: a -> a -> a
  smoothstep :: a -> a -> a -> a

  rsqrt = (1/) . sqrt
  exp2 = (2**)
  log2 = logBase 2
  clamp x a = minB (maxB x a)
  saturate x = clamp x 0 1
  mix x y a = x*(1-a)+y*a
  step a x = ifB (x <* a) 0 1
  smoothstep a b x = let t = saturate ((x-a) / (b-a))
                     in t*t*(3-2*t)
  fract' x = x - floor' x
  mod' x y = x - y* floor' (x/y)



while :: (IfB (S c Bool), IfB a) => (a -> S c Bool) -> (a -> a) -> a -> a
while = undefined
