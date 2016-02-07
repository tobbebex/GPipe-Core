# GPipe 2

This is the main repository for GPipe 2, a library which encapsulates OpenGL
and provides a minimal type safe interface.
Check out the release announcement, [GPipe is dead, long live GPipe!](http://tobbebex.blogspot.se/2015/09/gpipe-is-dead-long-live-gpipe.html)
or read on for the tutorials.

## Examples and tutorials

There is a series of five tutorials for learning the features of GPipe 2
hosted at <http://tobbebex.blogspot.se> (see below for a table of contents).
You'll start with a simple triangle example,
and advance through all of the encapsulated features of OpenGL that GPipe exposes.

### Example code

Clone [GPipe-Test](https://github.com/plredmond/GPipe-Test)
and build it with [Haskell Stack](https://github.com/commercialhaskell/stack) `$ stack build`.
This is the example from *Part 1*, below.

### GPU programming in Haskell using GPipe

* [Part 1](http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe.html)
    * Hello triangle!
    * The context
    * Rendering - This is what it's all about
    * Shader - A primer
    * Rasterization
    * Drawing and swapping
* [Part 2](http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe_11.html)
    * Buffers
    * Vertex arrays
    * Primitive arrays
    * Index arrays
    * Instanced primitive arrays
* [Part 3](http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe.html)
    * Welcome to the Shader!
    * Primitive streams
    * Error handling in Gpipe
    * Enough of that, back to the shader
    * Linear algebra in GPipe
    * Uniforms
    * Working with lifted S-values
    * Combining Shader monads
* [Part 4](http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe_21.html)
    * FragmentStreams
    * Textures
    * Samplers
    * Colors
    * Sampling
* [Part 5](http://tobbebex.blogspot.se/2015/11/gpu-programming-in-haskell-using-gpipe.html)
    * Window formats
    * Window drawing actions
    * Drawing colors
    * Depth test
    * Stencil test
    * Custom filtering of fragments
    * Drawing to texture images
    * Demo time!

### Feedback

Please submit bugs, questions, or suggestions for improvements to our documentation
to our github repository, https://github.com/tobbebex/GPipe-Core .
