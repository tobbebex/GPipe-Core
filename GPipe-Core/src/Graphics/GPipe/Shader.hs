module Graphics.GPipe.Shader (
    Shader(),
    CompiledShader,
    guard',
    compileShader,
    mapShader,
    maybeShader,
    chooseShader,
    silenceShader
)
where

import Graphics.GPipe.Internal.Shader 