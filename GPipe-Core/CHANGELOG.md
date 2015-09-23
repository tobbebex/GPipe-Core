### 2.1

- Making dangling finalizers work with shared and unshared contexts (#10) 
- Moved orphan instances to separate module (#11)
- Fixing a bug introduced in 2.0.2 when using multiple uniforms 
- Fixing exception when using conditionals in the shader (#12)

### 2.0.2

- Linear bumped to 1.20
- Running contextSwap and contextFrameBufferSize on righjt thread (#7)
- render now throws if rendering to an image from a texture thats used for sampling (#8)
- Added GPipe class instances for all linear types (#9)

### 2.0.1

- Fixed runtime error in simple fragment program when not all uniforms where used (#5)

### 2.0

- Initial release of GPipe 2