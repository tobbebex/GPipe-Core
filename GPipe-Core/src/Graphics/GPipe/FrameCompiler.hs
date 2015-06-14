module Graphics.GPipe.FrameCompiler where

import Graphics.GPipe.Context
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception (MonadException)
import Graphics.GPipe.Shader
import Data.IntMap as Map hiding (foldl, map)
import Prelude hiding (putStr, (!))
import Data.Text.Lazy.IO (putStr)
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text)
import Control.Monad (forM, forM_)
import Data.List (transpose, zip4)

data Drawcall = Drawcall {
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

data RenderIOState = RenderIOState
    {
        uniformNameToRenderIO :: Map.IntMap (Binding -> IO ()),
        samplerNameToRenderIO :: Map.IntMap (Binding -> IO ()),
        inputArrayToRenderIOs :: Map.IntMap [[Int] -> IO ()],
        drawToRenderIOs :: Map.IntMap (IO ())
    }  

newRenderIOState :: RenderIOState
newRenderIOState = RenderIOState Map.empty Map.empty Map.empty Map.empty

compile :: (Monad m, MonadIO m, MonadException m) => [IO Drawcall] -> ContextT os f m (RenderIOState -> IO ()) 
compile dcs = do
    drawcalls <- liftIO $ sequence dcs
    let allocatedUniforms = allocate glMAXUniforms (map (usedUniforms) drawcalls)      
    let allocatedSamplers = allocate glMAXSamplers (map (usedSamplers) drawcalls)      
    fs <- mapM comp $ zip4 drawcalls allocatedUniforms allocatedSamplers [111..] -- just to debug program names, make zip3 later      
    return $ \x -> foldl (\io f -> io >> f x) (return ()) fs 
 where
    comp (Drawcall dcN vsource fsource inps unis samps, ubinds, sbinds, pN) =
           liftContextIO $ do let inpsi = zip inps [0..] :: [(Int,Int)]
                                  unisi = zip unis [0..] :: [(Int,Int)]
                                  sampsi = zip samps [0..] :: [(Int,Int)]
                                  ubindsi = zip ubinds [0..] :: [(Int,Int)]
                                  sbindsi = zip sbinds [0..] :: [(Int,Int)]
                                  unisbinds = zip unis ubinds :: [(Int,Int)]
                                  sampsbinds = zip samps sbinds :: [(Int,Int)]
                              
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
                              
                              mapM_ (\(name, ix) -> putStrLn $ "INPUT BindNameToIndex in" ++ show name ++ " " ++ show ix) inpsi
                              mapM_ (\(name, ix) -> putStrLn $ "UNI BindNameToIndex uBlock" ++ show name ++ " " ++ show ix) unisi
                              mapM_ (\(name, ix) -> putStrLn $ "SAMP BindNameToIndex s" ++ show name ++ " " ++ show ix) sampsi
                              
                              putStrLn "---- LINK ---"
                              pName <- return pN -- glGenProg
                              putStrLn $ "pName = " ++ show pName
                              
                              mapM_ (\(bind, ix) -> putStrLn $ "glUniformBlockBinding p ix bind " ++ show pName ++ " " ++ show ix ++ " " ++ show bind) ubindsi
                              mapM_ (\(bind, ix) -> putStrLn $ "samplerBinds " ++ show pName ++ " " ++ show ix ++ " " ++ show bind) sbindsi
                              putStrLn "-------------------------------------------------------------------------------------------"
                              putStrLn "-------------------------------------------------------------------------------------------"
                              
                              return (\s -> do 
                                           putStrLn "-----------------------------------------"
                                           putStrLn $ "UseProgram " ++ show pName 
                                           -- Bind uniforms
                                           mapM_ (\(n, b) -> (uniformNameToRenderIO s ! n) b) unisbinds
                                           mapM_ (\(n, b) -> (samplerNameToRenderIO s ! n) b) sampsbinds
                                           mapM_ ($ inps) (inputArrayToRenderIOs s ! dcN)
                                           putStrLn "-----------------------------------------"
                                           )

allocate mx xss = xss -- TODO, find bindings 

glMAXUniforms = 3 :: Int
glMAXSamplers = 3:: Int
      
orderedUnion :: Ord a => [a] -> [a] -> [a]
orderedUnion xxs@(x:xs) yys@(y:ys) | x == y    = x : orderedUnion xs ys 
                                   | x < y     = x : orderedUnion xs yys
                                   | otherwise = y : orderedUnion xxs ys
orderedUnion xs [] = xs
orderedUnion [] ys = ys

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


      