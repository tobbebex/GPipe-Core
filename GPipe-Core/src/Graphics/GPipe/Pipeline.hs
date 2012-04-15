{-# LANGUAGE GADTs #-}

module Graphics.GPipe.Pipeline where

import Graphics.GPipe.Format

data Pipeline os f where
     Pipeline :: FramebufferFormat f => Pipeline os f
     
     
runPipeLineGl p = undefined        