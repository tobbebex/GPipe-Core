module Graphics.GPipe.Shader (
    Shader(),
    CompiledShader,
    Render(),
    guard',
    render,
    compileShader,
    mapShader,
    maybeShader,
    chooseShader,
    silenceShader
)
where

import Graphics.GPipe.Internal.Shader 