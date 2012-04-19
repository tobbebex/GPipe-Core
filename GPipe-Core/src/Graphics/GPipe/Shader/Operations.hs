{-# LANGUAGE FlexibleInstances, TypeFamilies, FlexibleContexts #-}

module Graphics.GPipe.Shader.Operations where

import Graphics.GPipe.Shader
import Data.Int
import Data.Word
import Data.Boolean

-- TODO: Delete those when ghc 7.4 is in haskell platform:
instance Eq   (S a Float)
instance Show (S a Float)
instance Eq   (S a Int32)
instance Show (S a Int32)
instance Eq   (S a Int16)
instance Show (S a Int16)
instance Eq   (S a Int8)
instance Show (S a Int8)
instance Eq   (S a Word32)
instance Show (S a Word32)
instance Eq   (S a Word16)
instance Show (S a Word16)
instance Eq   (S a Word8)
instance Show (S a Word8)

bin :: String -> S c a -> S c a -> S c a
bin o (S a) (S b) = S $ '(' : a ++ o ++ b ++ ")"

fun1 :: String -> S c a -> S c a
fun1 f (S a) = S $ f ++ '(' : a ++ ")"
fun2 :: String -> S c a -> S c a -> S c a
fun2 f (S a) (S b) = S $ f ++ '(' : a ++ ',' : b ++ ")"
fun3 :: String -> S c a -> S c a -> S c a -> S c a
fun3 f (S a) (S b) (S c) = S $ f ++ '(' : a ++ ',' : b ++ ',' : c ++")"

postop :: String -> S c a -> S c a
postop f (S a) = S $ '(' : f ++ a ++ ")"

preop :: String -> S c a -> S c a
preop f (S a) = S $ '(' : a ++ f ++ ")"

instance Num (S a Float) where
    (+) = bin "+"
    (-) = bin "-"
    abs = fun1 "abs"
    signum = fun1 "sign"
    (*) = bin "*"
    fromInteger = S . show
    negate = preop "-"

instance Fractional (S a Float) where
  (/)          = bin "/"
  fromRational = S . show

instance Floating (S a Float) where
  pi    = S $ show (pi :: Float)
  sqrt  = fun1 "sqrt"
  exp   = fun1 "exp"
  log   = fun1 "log"
  (**)  = fun2 "pow"
  sin   = fun1 "sin"
  cos   = fun1 "cos"
  tan   = fun1 "tan"
  asin  = fun1 "asin"
  acos  = fun1 "acos"
  atan  = fun1 "atan"
  sinh  = fun1 "sinh"
  cosh  = fun1 "cosh"
  asinh = fun1 "asinh"
  atanh = fun1 "atanh"
  acosh = fun1 "acosh"

-- | This class provides the GPU functions either not found in Prelude's numerical classes, or that has wrong types.
--   Instances are also provided for normal 'Float's and 'Double's.
--   Minimal complete definition: 'floor'' and 'ceiling''.
class (IfB (RealBool a) a, OrdB (RealBool a) a, Floating a) => Real' a where
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


  

noFun :: String -> a
noFun = error . (++ ": No overloading for shader type S")

    