{-# LANGUAGE PatternGuards #-}
module Graphics.GPipe.FrameCompiler where

import Graphics.GPipe.Context
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception (MonadException)
import Graphics.GPipe.Shader
import qualified Data.IntMap as Map
import Data.IntMap ((!))
import Prelude hiding (putStr, (!))
import Data.Text.Lazy.IO (putStr)
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text)
import Control.Monad (forM, forM_, liftM)
import Data.List (transpose, zip4)
import Data.Maybe (isJust)

data Drawcall = Drawcall {
                    drawcallErrorStr :: String,
                    frameBufferName :: Int,
                    drawcallName :: Int,
                    vertexsSource :: Text,
                    fragmentSource :: Text,
                    usedInputs :: [Int],
                    usedUniforms :: [Int],
                    usedSamplers :: [Int]
                    }                                      

type Binding = Int
-- index/binding refers to what is used in the final shader. Index space is limited, usually 16
-- attribname is what was declared, but all might not be used. Attribname share namespace with uniforms and textures and is unlimited(TM)

data RenderIOState s = RenderIOState 
    {
        uniformNameToRenderIO :: Map.IntMap (s -> Binding -> IO ()),
        samplerNameToRenderIO :: Map.IntMap (s -> Binding -> IO ()),
        inputArrayToRenderIOs :: Map.IntMap (s -> [[Binding] -> IO ()]),
        drawToRenderIOs :: Map.IntMap (s -> IO ())
    }  

newRenderIOState :: RenderIOState s
newRenderIOState = RenderIOState Map.empty Map.empty Map.empty Map.empty

mapRenderIOState :: (s -> s') -> RenderIOState s' -> RenderIOState s -> RenderIOState s
mapRenderIOState f (RenderIOState a b c d) (RenderIOState i j k l) = let g x = x . f in RenderIOState (Map.union i $ Map.map g a) (Map.union j $ Map.map g b) (Map.union k $ Map.map g c) (Map.union l $ Map.map g d)

data CompiledFrame os f s = CompiledFrame (s -> IO ())

compile :: (Monad m, MonadIO m, MonadException m) => [IO Drawcall] -> RenderIOState s -> ContextT os f m (CompiledFrame os f s) 
compile dcs s = do
    drawcalls <- liftIO $ sequence dcs
    let allocatedUniforms = allocate glMAXUniforms (map usedUniforms drawcalls)      
    let allocatedSamplers = allocate glMAXSamplers (map usedSamplers drawcalls)      
    (pnames, fs) <- liftM unzip $ mapM comp $ zip3 drawcalls allocatedUniforms allocatedSamplers
    let fr = CompiledFrame $ \x -> foldl (\io f -> io >> f x) (return ()) fs
    addContextFinalizer fr $ do mapM_ glDelProg pnames    
    return fr
 where
    comp (Drawcall err fbN dcN vsource fsource inps unis samps, ubinds, sbinds) =
           liftContextIO $ do
                              putStrLn "-------------------------------------------------------------------------------------------"
                              putStrLn "-------------------------------------------------------------------------------------------"
                              putStrLn "Compiling program"
                              putStrLn "-------------"
                              putStrLn "VERTEXSHADER:"
                              putStr vsource
                              putStrLn "-------------"   
                              putStrLn "FRAGMENTSHADER:"
                              putStr fsource
                              putStrLn "-------------"
                              
                              mapM_ (\(name, ix) -> putStrLn $ "INPUT BindNameToIndex in" ++ show name ++ " " ++ show ix) $ zip inps [0..]
                              mapM_ (\(name, ix) -> putStrLn $ "UNI BindNameToIndex uBlock" ++ show name ++ " " ++ show ix) $ zip unis [0..]
                              mapM_ (\(name, ix) -> putStrLn $ "SAMP BindNameToIndex s" ++ show name ++ " " ++ show ix) $ zip samps [0..]
                              
                              putStrLn "---- LINK ---"
                              putStrLn $ "If errors, blame it on " ++ err
                              pName <- glGenProg
                              
                              mapM_ (\(bind, ix) -> putStrLn $ "glUniformBlockBinding p ix bind " ++ show pName ++ " " ++ show ix ++ " " ++ show bind) $ zip ubinds [0..]
                              mapM_ (\(bind, ix) -> putStrLn $ "samplerBinds " ++ show pName ++ " " ++ show ix ++ " " ++ show bind) $ zip sbinds [0..]
                              putStrLn "-------------------------------------------------------------------------------------------"
                              putStrLn "-------------------------------------------------------------------------------------------"
                              
                              return (pName, \x -> do 
                                           putStrLn "-----------------------------------------"
                                           putStrLn $ "UseProgram " ++ show pName 
                                           -- Bind uniforms
                                           mapM_ (\(n,b) -> (uniformNameToRenderIO s ! n) x b) $ zip unis ubinds -- TODO: Dont bind already bound
                                           mapM_ (\(n,b) -> (samplerNameToRenderIO s ! n) x b) $ zip samps sbinds -- TODO: Dont bind already bound                                          
                                           (drawToRenderIOs s ! fbN) x  -- TODO: Dont bind already bound
                                           mapM_ ($ inps) ((inputArrayToRenderIOs s ! dcN) x)
                                           putStrLn "-----------------------------------------"
                                           )

glGenProg = return 0
glDelProg n = putStrLn $ "gldelprog " ++ show n

allocate :: Int -> [[Int]] -> [[Int]]
allocate mx = allocate' Map.empty []
    where allocate' m ys ((x:xs):xss) | Just a <- Map.lookup x m  = allocate' m (a:ys) (xs:xss) 
                                      | ms <- Map.size m, ms < mx = allocate' (Map.insert x ms m) (ms:ys) (xs:xss)
                                      | otherwise                 = let (ek,ev) = findLastUsed m mx (ys ++ xs ++ concat xss) in allocate' (Map.insert x ev (Map.delete ek m)) (ev:ys) (xs:xss)
          allocate' m ys (_:xss) = reverse ys : allocate' m [] xss 
          allocate' _ _ [] = []
          
          findLastUsed m n (x:xs) | n > 1 = let (a, m') = Map.updateLookupWithKey (const $ const Nothing) x m 
                                                n' = if isJust a then n-1 else n
                                            in findLastUsed m' n' xs
          findLastUsed m _ _ = head $ Map.toList m                                    
          
                                        

glMAXUniforms = 3 :: Int
glMAXSamplers = 3:: Int
      


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


      