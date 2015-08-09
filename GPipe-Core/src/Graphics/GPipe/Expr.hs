module Graphics.GPipe.Expr (
    S(),   
    V, F,
    VFloat, VInt, VWord, VBool,
    FFloat, FInt, FWord, FBool,

    Convert(..),
    
    ShaderBase(),
    ShaderType(..),
    
    ifThenElse,
    ifThenElse',
    ifThen,
    while,
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
    Real'(..)
)
where

import Graphics.GPipe.Internal.Expr