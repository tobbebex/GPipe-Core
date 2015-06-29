{-# LANGUAGE PatternGuards #-}
module Graphics.GPipe.Compiler where

import Graphics.GPipe.Context
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception (MonadException)
import qualified Data.IntMap as Map
import Data.IntMap ((!))
import Prelude hiding (putStr)
import Data.Text.Lazy.IO (putStr)
import Data.Text.Lazy (Text)
import Data.Maybe (isJust)
import Control.Monad (mapAndUnzipM)
import Control.Monad.Trans.State.Lazy (evalStateT, get, put)
import Control.Monad.Trans.Class (lift)

data Drawcall s = Drawcall {
                    runDrawcall :: s -> IO (),
                    drawcallName :: Int,
                    rasterizationName :: Int,
                    vertexsSource :: Text,
                    fragmentSource :: Text,
                    usedInputs :: [Int],
                    usedUniforms :: [Int],
                    usedSamplers :: [Int]
                    }                                      

type Binding = Int
-- index/binding refers to what is used in the final shader. Index space is limited, usually 16
-- attribname is what was declared, but all might not be used. Attribname share namespace with uniforms and textures and is unlimited(TM)

-- TODO: Add usedBuffers to RenderIOState, ie Map.IntMap (s -> (Binding -> IO (), Int)) and the like
--       then create a function that checks that none of the input buffers are used as output, and throws if it is

data RenderIOState s = RenderIOState 
    {
        uniformNameToRenderIO :: Map.IntMap (s -> Binding -> IO ()),
        samplerNameToRenderIO :: Map.IntMap (s -> Binding -> IO ()),
        rasterizationNameToRenderIO :: Map.IntMap (s -> IO ()),
        inputArrayToRenderIOs :: Map.IntMap (s -> [[Binding] -> IO ()])
    }  

newRenderIOState :: RenderIOState s
newRenderIOState = RenderIOState Map.empty Map.empty Map.empty Map.empty

mapRenderIOState :: (s -> s') -> RenderIOState s' -> RenderIOState s -> RenderIOState s
mapRenderIOState f (RenderIOState a b c d) (RenderIOState i j k l) = let g x = x . f in RenderIOState (Map.union i $ Map.map g a) (Map.union j $ Map.map g b) (Map.union k $ Map.map g c) (Map.union l $ Map.map g d)

data BoundState = BoundState { 
                        boundUniforms :: Map.IntMap Int,  
                        boundSamplers :: Map.IntMap Int
                        }  

compile :: (Monad m, MonadIO m, MonadException m) => [IO (Drawcall s)] -> RenderIOState s -> ContextT os f m (s -> IO ())
compile dcs s = do
    drawcalls <- liftIO $ sequence dcs -- IO only for SNMap
    let allocatedUniforms = allocate glMAXUniforms (map usedUniforms drawcalls)      
    let allocatedSamplers = allocate glMAXSamplers (map usedSamplers drawcalls)      
    (pnames, fs) <- evalStateT (mapAndUnzipM comp  (zip3 drawcalls allocatedUniforms allocatedSamplers)) (BoundState Map.empty Map.empty)
    let fr x = foldl (\ io f -> io >> f x) (return ()) fs
    addContextFinalizer fr $ mapM_ glDelProg pnames    
    return fr
 where   
    comp (Drawcall runner primN rastN vsource fsource inps unis samps, ubinds, sbinds) = do
           BoundState uniState sampState <- get
           let (bindUni, uniState') = makeBind uniState (uniformNameToRenderIO s) (zip unis ubinds)
           let (bindSamp, sampState') = makeBind sampState (samplerNameToRenderIO s) $ zip samps sbinds
           put $ BoundState uniState' sampState'
           
           lift $ liftContextIO $ do
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
                              pName <- glGenProg
                              
                              mapM_ (\(bind, ix) -> putStrLn $ "glUniformBlockBinding p ix bind " ++ show pName ++ " " ++ show ix ++ " " ++ show bind) $ zip ubinds [0..]
                              mapM_ (\(bind, ix) -> putStrLn $ "samplerBinds " ++ show pName ++ " " ++ show ix ++ " " ++ show bind) $ zip sbinds [0..]
                              putStrLn "-------------------------------------------------------------------------------------------"
                              putStrLn "-------------------------------------------------------------------------------------------"
                              
                              return (pName, \x -> do
                                           putStrLn "-----------------------------------------"
                                           putStrLn $ "UseProgram " ++ show pName 
                                           bindUni x 
                                           bindSamp x                                           
                                           runner x
                                           (rasterizationNameToRenderIO s ! rastN) x -- TODO: Dont bind already bound?
                                           mapM_ ($ inps) ((inputArrayToRenderIOs s ! primN) x) -- TODO: Dont bind already bound?
                                           putStrLn "-----------------------------------------"
                                           )

-- Optimization, save gl calls to already bound buffers/samplers
makeBind :: Map.IntMap Int -> Map.IntMap (s -> Int -> IO ()) -> [(Int, Int)] -> (s -> IO (), Map.IntMap Int)
makeBind m iom ((n,b):xs) = (g, m'')
    where 
        (f, m') = makeBind m iom xs
        (io, m'') = case Map.lookup b m' of
                            Just x | x == n -> (const $ return (), m')
                            _               -> (\s -> (iom ! n) s b, Map.insert b n m')      
        g s = f s >> io s
makeBind m _ [] = (const $ return (), m)                          

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
      
      