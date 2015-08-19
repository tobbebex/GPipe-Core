-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Expr
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- This module provides the DSL for shaders in GPipe. The type 'S x a' is an opaque type that represents a value of type 'a' in a shader stage 'x', eg 'S F Float' means a
-- floating point value in a fragment stream.
--
-- Instances for various type classes such as 'Num' are provided. Note that GPipe only defines the most low level linear algebra functionality, and using a separate package
-- for this is advised.
-----------------------------------------------------------------------------

module Graphics.GPipe.Expr (
    S(),   
    V, F,
    VFloat, VInt, VWord, VBool,
    FFloat, FInt, FWord, FBool,

    Convert(..),
    
    ShaderBase(),
    ShaderType(..),
    
    while,
    ifThen,
    ifThenElse,
    ifThenElse',
    dFdx,
    dFdy,
    fwidth,
    length4,
    length3,
    length2,
    normalize4,
    normalize3,
    normalize2,
    dot4,
    dot3,
    dot2,
    cross,    
    Real'(..),
    
    module Data.Boolean
)
where

import Data.Boolean
import Graphics.GPipe.Internal.Expr