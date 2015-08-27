-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Uniform
-- Copyright   :  Tobias Bexelius
-- License     :  MIT
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :  Portable
--
-- |
-- Uniform values are constants that you can combine with all vertices or fragments of a 'PrimitiveStream' or 'FragmentStream'.
-- They are loaded from a 'Buffer' and OpenGl uniform blocks are used under the hood.  
-----------------------------------------------------------------------------

module Graphics.GPipe.Uniform (
    UniformInput(..),
    ToUniform(),
    getUniform,   
    Uniform(..), 
)
where

import Graphics.GPipe.Internal.Uniform
import Graphics.GPipe.Internal.Buffer  