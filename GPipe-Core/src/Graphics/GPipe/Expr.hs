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
    Integral'(..),
    Real'(..),
    
    module Data.Boolean
)
where

import Data.Boolean
import Graphics.GPipe.Internal.Expr