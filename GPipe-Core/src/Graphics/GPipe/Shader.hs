module Graphics.GPipe.Shader (
    Shader(),
    compileShader,
    CompiledShader,
    guard',
    mapShader,
    maybeShader,
    chooseShader,
    silenceShader
)
where

import Graphics.GPipe.Internal.Shader 