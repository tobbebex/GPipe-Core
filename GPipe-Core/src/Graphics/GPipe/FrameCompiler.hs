module Graphics.GPipe.FrameCompiler (compile) where

import Graphics.GPipe.ContextState
import Graphics.GPipe.Context
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Exception (MonadException)

                        

compile :: (Monad m, MonadIO m, MonadException m) => [DrawCall] -> ContextT os f m CompiledFrame 
compile dcs = do
    return $ (\ _ -> Right $ return ())
      


{-
            in do (fsource, funis, fsamps, finps, prevDecls, prevS) <- runShaderM (return ()) m
                  (vsource, vunis, vsamps, vinps, _, _) <- runShaderM prevDecls prevS                 
                  let unis = orderedUnion funis vunis
                      samps = orderedUnion fsamps vsamps
                      dcname = 
                      showBiggerThen x m = '(' : show x ++ '>': show m ++ ")\n"
                      testLimit x m s = when (x > m) $ tellError ("Too many " ++ s ++ " in " ++ dcname ++ showBiggerThen x m)
                  
                  mvu <- getGlMAX_VERTEX_UNIFORMS
                  mfu <- getGlMAX_FRAGMENT_UNIFORMS
                  mu <- getGlMAX_UNIFORM_BINDINGS
                  mvs <- getGlMAX_VERTEX_SAMPLERS
                  mfs <- getGlMAX_FRAGMENT_SAMPLERS
                  ms <- getGlMAX_SAMPLER_BINDINGS                 
                  mvi <- getGlMAX_VERTEX_ATTRIBS                 
                  mfi <- getGlMAX_VARYING_FLOATS                                  
                  testLimit (length vunis) mvu "vertex uniform blocks"
                  testLimit (length funis) mfu "fragment uniform blocks"
                  testLimit (length unis) mu "total uniform blocks"
                  testLimit (length vsamps) mvs "vertex samplers"
                  testLimit (length fsamps) mfs "fragment samplers"
                  testLimit (length samps) ms "total samplers"
                  testLimit (length vinps) mvi "vertex inputs"
                  testLimit (length finps) mfi "fragment inputs"                 
                  
                  --generate program, bind inputs, uniforms and samplers
                  

                  glCompile                                     
                  -- compile shader and program
                  return () -- TODO: Make the shader and write the drawcall


getGlMAX_VERTEX_ATTRIBS = return 5
getGlMAX_VARYING_FLOATS = return 5     
getGlMAX_VERTEX_UNIFORMS = return 5                                                
getGlMAX_FRAGMENT_UNIFORMS = return 5                                                
getGlMAX_UNIFORM_BINDINGS = return 5                                                
getGlMAX_VERTEX_SAMPLERS = return 5                                                
getGlMAX_FRAGMENT_SAMPLERS = return 5
getGlMAX_SAMPLER_BINDINGS = return 5

-}

orderedUnion xxs@(x:xs) yys@(y:ys) | x == y    = x : orderedUnion xs ys 
                                   | x < y     = x : orderedUnion xs yys
                                   | otherwise = y : orderedUnion xxs ys
orderedUnion xs [] = xs
orderedUnion [] ys = ys
orderedUnion _ _   = []
      