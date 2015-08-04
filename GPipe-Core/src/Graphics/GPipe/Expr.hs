module Graphics.GPipe.Expr (
    ShaderBase(..),
    ShaderType(..),
    S(),   
    V, F,
    VFloat, VInt, VWord, VBool,
    FFloat, FInt, FWord, FBool,
    
    ifThenElse,
    ifThenElse',
    ifThen,
    while,
    Real'(..),
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
    cross    
)
where

import Graphics.GPipe.Internal.Expr