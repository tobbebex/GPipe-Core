module Graphics.GPipe.Expr (
    ShaderBase(),
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
)
where

import Graphics.GPipe.Internal.Expr