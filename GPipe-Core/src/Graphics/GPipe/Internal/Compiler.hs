{-# LANGUAGE PatternGuards #-}
module Graphics.GPipe.Internal.Compiler where

import Graphics.GPipe.Internal.Context
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception (MonadException)
import qualified Data.IntMap as Map
import Data.IntMap ((!))
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State.Lazy (evalStateT, get, put)
import Control.Monad.Trans.Class (lift)

import Graphics.GL.Core33
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr (nullPtr)
import Data.Either
import Control.Exception (throwIO)
import Data.IORef

data Drawcall s = Drawcall {
                    drawcallFBO :: s -> (Maybe (IO FBOKeys, IO ()), IO ()),
                    drawcallName :: Int,
                    rasterizationName :: Int,
                    vertexsSource :: String,
                    fragmentSource :: String,
                    usedInputs :: [Int],
                    usedUniforms :: [Int],
                    usedSamplers :: [Int]
                    }                                      

-- index/binding refers to what is used in the final shader. Index space is limited, usually 16
-- attribname is what was declared, but all might not be used. Attribname share namespace with uniforms and textures and is unlimited(TM)
type Binding = Int

-- TODO: Add usedBuffers to RenderIOState, ie Map.IntMap (s -> (Binding -> IO (), Int)) and the like
--       then create a function that checks that none of the input buffers are used as output, and throws if it is

data RenderIOState s = RenderIOState 
    {
        uniformNameToRenderIO :: Map.IntMap (s -> Binding -> IO ()),
        samplerNameToRenderIO :: Map.IntMap (s -> Binding -> IO ()),
        rasterizationNameToRenderIO :: Map.IntMap (s -> IO ()),
        inputArrayToRenderIOs :: Map.IntMap (s -> [[Binding] -> ((IO [VAOKey], IO ()), IO ())])
    }  
  
newRenderIOState :: RenderIOState s
newRenderIOState = RenderIOState Map.empty Map.empty Map.empty Map.empty

mapRenderIOState :: (s -> s') -> RenderIOState s' -> RenderIOState s -> RenderIOState s
mapRenderIOState f (RenderIOState a b c d) (RenderIOState i j k l) = let g x = x . f in RenderIOState (Map.union i $ Map.map g a) (Map.union j $ Map.map g b) (Map.union k $ Map.map g c) (Map.union l $ Map.map g d)

data BoundState = BoundState { 
                        boundUniforms :: Map.IntMap Int, 
                        boundSamplers :: Map.IntMap Int,
                        boundRasterizerN :: Int
                        }  


-- | May throw a GPipeCompileException 
compile :: (Monad m, MonadIO m, MonadException m) => [IO (Drawcall s)] -> RenderIOState s -> ContextT w os f m (ContextData -> s -> IO ())
compile dcs s = do
    drawcalls <- liftIO $ sequence dcs -- IO only for SNMap
    (maxUnis, maxSamplers) <- liftContextIO $ do 
                       maxUnis <- alloca (\ptr -> glGetIntegerv GL_MAX_COMBINED_UNIFORM_BLOCKS ptr >> peek ptr)
                       maxSamplers <- alloca (\ptr -> glGetIntegerv GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS ptr >> peek ptr)
                       return (fromIntegral maxUnis, fromIntegral maxSamplers)
    let unisPerDc = map usedUniforms drawcalls
    let sampsPerDc = map usedSamplers drawcalls
    -- TODO: Check if this is really needed, if so make it more verbose:
    let errUni = ["Too many uniform blocks used in a single shader\n" | any (\ xs -> length xs >= maxUnis) unisPerDc]
    let errSamp = ["Too many textures used in a single shader\n" | any (\ xs -> length xs >= maxSamplers) sampsPerDc]
    let allocatedUniforms = allocate maxUnis unisPerDc      
    let allocatedSamplers = allocate maxSamplers sampsPerDc
    compRet <- evalStateT (mapM comp  (zip3 drawcalls allocatedUniforms allocatedSamplers)) (BoundState Map.empty Map.empty (-1))
    fAdd <- getContextFinalizerAdder
    let (errs, ret) = partitionEithers compRet      
        (pnames, fs) = unzip ret
        fr cd x = foldl (\ io f -> io >> f x cd fAdd) (return ()) fs
        allErrs = errUni ++ errSamp ++ errs
    if null allErrs 
        then do  
            forM_ pnames (\pNameRef -> do pName <- liftIO $ readIORef pNameRef 
                                          addContextFinalizer pNameRef (glDeleteProgram pName))
            return fr
        else do
            liftContextIOAsync $ mapM_ (readIORef >=> glDeleteProgram) pnames
            liftIO $ throwIO (GPipeException $ concat allErrs)   
 where   
    comp (Drawcall fboSetup primN rastN vsource fsource inps unis samps, ubinds, sbinds) = do
           BoundState uniState sampState boundRastN <- get
           let (bindUni, uniState') = makeBind uniState (uniformNameToRenderIO s) (zip unis ubinds)
           let (bindSamp, sampState') = makeBind sampState (samplerNameToRenderIO s) $ zip samps sbinds
           let bindRast = if rastN == boundRastN then const $ return () else rasterizationNameToRenderIO s ! rastN 
           put $ BoundState uniState' sampState' rastN
           
           lift $ do ePname <- liftContextIO $ do
                              vShader <- glCreateShader GL_VERTEX_SHADER
                              mErrV <- compileShader vShader vsource
                              fShader <- glCreateShader GL_FRAGMENT_SHADER                             
                              mErrF <- compileShader fShader fsource
                              if isNothing mErrV && isNothing mErrV 
                                then do pName <- glCreateProgram
                                        glAttachShader pName vShader
                                        glAttachShader pName fShader
                                        mapM_ (\(name, ix) -> withCString ("in"++ show name) $ glBindAttribLocation pName ix) $ zip inps [0..]
                                        mPErr <- linkProgram pName
                                        glDetachShader pName vShader
                                        glDetachShader pName fShader
                                        glDeleteShader vShader
                                        glDeleteShader fShader
                                        case mPErr of
                                            Just errP -> do glDeleteProgram pName 
                                                            return $ Left $ "Linking a GPU progam failed:\n" ++ errP ++ "\nVertex source:\n" ++ vsource ++ "\nFragment source:\n" ++ fsource
                                            Nothing -> return $ Right pName
                                else do glDeleteShader vShader
                                        glDeleteShader fShader
                                        let err = maybe "" (\e -> "A vertex shader compilation failed:\n" ++ e ++ "\nSource:\n" ++ vsource) mErrV
                                              ++ maybe "" (\e -> "A fragment shader compilation failed:\n" ++ e ++ "\nSource:\n" ++ fsource) mErrF
                                        return $ Left err
                     case ePname of                   
                          Left err -> return $ Left err
                          Right pName -> liftContextIO $ do                                                          
                              forM_ (zip unis ubinds) $ \(name, bind) -> do
                                    uix <- withCString ("uBlock" ++ show name) $ glGetUniformBlockIndex pName
                                    glUniformBlockBinding pName uix (fromIntegral bind)
                              
                              glUseProgram pName -- For setting texture uniforms
                              forM_ (zip samps sbinds) $ \(name, bind) -> do
                                    six <- withCString ("s" ++ show name) $ glGetUniformLocation pName
                                    glUniform1i six (fromIntegral bind)
                              pNameRef <- newIORef pName
                                                                 
                              return $ Right (pNameRef, \x cd fAdd -> do
                                           -- Drawing with program --
                                           pName' <- readIORef pNameRef -- Cant use pName, need to touch pNameRef
                                           glUseProgram pName'
                                           bindUni x 
                                           bindSamp x
                                           bindRast x
                                           let (mfbokeyio, blendio) = fboSetup x
                                           blendio
                                           case mfbokeyio of
                                                Nothing -> glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                                                Just (fbokeyio, fboio) -> do
                                                   fbokey <- fbokeyio
                                                   mfbo <- getFBO cd fbokey
                                                   case mfbo of
                                                        Just fbo -> do fbo' <- readIORef fbo
                                                                       glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                        Nothing -> do fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
                                                                      fbo <- newIORef fbo'
                                                                      void $ fAdd fbo $ with fbo' (glDeleteFramebuffers 1)
                                                                      setFBO cd fbokey fbo
                                                                      glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                                      glEnable GL_FRAMEBUFFER_SRGB
                                                                      fboio
                                                                      let numColors = length $ fboColors fbokey
                                                                      withArray [GL_COLOR_ATTACHMENT0 .. (GL_COLOR_ATTACHMENT0 + fromIntegral numColors - 1)] $ glDrawBuffers (fromIntegral numColors)
                                           -- Draw each Vertex Array --
                                           forM_ (map ($ inps) ((inputArrayToRenderIOs s ! primN) x)) $ \ ((keyio, vaoio), drawio) -> do
                                                key <- keyio
                                                mvao <- getVAO cd key
                                                case mvao of
                                                    Just vao -> do vao' <- readIORef vao
                                                                   glBindVertexArray vao'
                                                    Nothing -> do vao' <- alloca (\ptr -> glGenVertexArrays 1 ptr >> peek ptr)
                                                                  vao <- newIORef vao'
                                                                  void $ fAdd vao $ with vao' (glDeleteVertexArrays 1)
                                                                  setVAO cd key vao
                                                                  glBindVertexArray vao'
                                                                  vaoio
                                                drawio
                                           )

    compileShader name source = do 
        withCStringLen source $ \ (ptr, len) ->
                                    with ptr $ \ pptr ->
                                        with (fromIntegral len) $ \ plen ->
                                            glShaderSource name 1 pptr plen
        glCompileShader name
        compStatus <- alloca $ \ ptr -> glGetShaderiv name GL_COMPILE_STATUS ptr >> peek ptr
        if compStatus /= GL_FALSE
            then return Nothing
            else do logLen <- alloca $ \ ptr -> glGetShaderiv name GL_INFO_LOG_LENGTH ptr >> peek ptr
                    let logLen' = fromIntegral logLen
                    liftM Just $ allocaArray logLen' $ \ ptr -> do
                                    glGetShaderInfoLog name logLen nullPtr ptr
                                    peekCString ptr 
    linkProgram name = do glLinkProgram name
                          linkStatus <- alloca $ \ ptr -> glGetProgramiv name GL_LINK_STATUS ptr >> peek ptr
                          if linkStatus /= GL_FALSE
                            then return Nothing
                            else do logLen <- alloca $ \ ptr -> glGetProgramiv name GL_INFO_LOG_LENGTH ptr >> peek ptr
                                    let logLen' = fromIntegral logLen
                                    liftM Just $ allocaArray logLen' $ \ ptr -> do
                                                    glGetProgramInfoLog name logLen nullPtr ptr
                                                    peekCString ptr 
            

-- Optimization, save gl calls to already bound buffers/samplers
makeBind :: Map.IntMap Int -> Map.IntMap (s -> Binding -> IO ()) -> [(Int, Int)] -> (s -> IO (), Map.IntMap Int)
makeBind m iom ((n,b):xs) = (g, m'')
    where 
        (f, m') = makeBind m iom xs
        (io, m'') = case Map.lookup b m' of
                            Just x | x == n -> (const $ return (), m')
                            _               -> (\s -> (iom ! n) s b, Map.insert b n m')      
        g s = f s >> io s
makeBind m _ [] = (const $ return (), m)                          

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
          
                             
      
