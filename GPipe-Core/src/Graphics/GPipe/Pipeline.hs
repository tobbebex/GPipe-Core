{-# LANGUAGE GADTs #-}

module Graphics.GPipe.Pipeline where


data Pipeline os c d s where
     Pipeline :: Pipeline os c d s
     
     
runPipeLineGl p = undefined        