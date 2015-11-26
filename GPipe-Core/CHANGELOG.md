### 2.1.5

- Fixed bug in clear where masks weren't set
- Added up to 7-tuple instances

### 2.1.4

- Fixed bug in dropVertices and dropIndices (#16)
- Added withPointSize (#15)

### 2.1.3

- Fixed bug in clearImage

### 2.1.2

- Fixed bug when nesting while, ifThen, ifThenElse or ifThenElse'. 

### 2.1.1

- Made ifB use ifThenElse' instead to avoid unwanted strictness 
- Fixed bug where ShaderBaseType for () wasn't lazy enough, causing error in ifThenElse'
- Added missing () instances

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