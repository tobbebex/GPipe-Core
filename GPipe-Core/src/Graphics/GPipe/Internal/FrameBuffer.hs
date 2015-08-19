{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
module Graphics.GPipe.Internal.FrameBuffer where

import           Control.Applicative
import           Control.Monad                           (when, void)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans.Writer.Lazy
import           Data.List                               (intercalate)
import           Graphics.GPipe.Internal.Compiler
import           Graphics.GPipe.Internal.Context
import           Graphics.GPipe.Internal.Expr
import           Graphics.GPipe.Internal.Format
import           Graphics.GPipe.Internal.FragmentStream
import           Graphics.GPipe.Internal.PrimitiveStream
import           Graphics.GPipe.Internal.Shader
import           Graphics.GPipe.Internal.Texture

import           Data.Word                               (Word)
import           Foreign.Marshal.Utils
import Graphics.GL.Core33
import Graphics.GL.Types
import Data.IORef
import Foreign.Marshal.Alloc
import Foreign.Storable (peek)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)

newtype DrawColors os s a = DrawColors (StateT Int (Writer [Int -> (ExprM (), GlobDeclM (), s -> (IO FBOKey, IO (), IO ()))]) a) deriving (Functor, Applicative, Monad)

runDrawColors :: DrawColors os s a -> (ExprM (), GlobDeclM (), s -> (IO [FBOKey], IO (), IO ()))
runDrawColors (DrawColors m) = foldl sf (return (), return (), const (return [], return (), return ())) $ zip [0..] $ execWriter (runStateT m 0)
    where sf (ms, mg, mio) (n, f) = let (sh, g, io) = f n in (ms >> sh, mg >> g, sf' mio io )
          sf' mio io s = let (a,b,c) = mio s
                             (x,y,z) = io s
                         in (do ns <- a
                                n <- x
                                return $ ns ++ [n]
                            , b >> y, c >> z)
drawColor :: forall c s os. ColorRenderable c => FragColor c -> (s -> (Image (Format c), ColorMask c, UseBlending)) -> DrawColors os s ()
drawColor c sf = DrawColors $ do n <- get
                                 put $ n+1
                                 lift $ tell [\ix -> make3  (setColor cf ix c) $ \s -> let (i, mask, o) = sf s
                                                                                           n' = fromIntegral n
                                                                                           useblend = if o then glEnablei GL_BLEND n' else glDisablei GL_BLEND n'
                                                                                       in (getImageFBOKey i,
                                                                                           getImageBinding i (GL_COLOR_ATTACHMENT0 + n'),
                                                                                            do
                                                                                             useblend
                                                                                             setGlColorMask cf n' mask)
                                                                                       ]
    where cf = undefined :: c

draw :: forall a os f s. FragmentStream a -> (s -> Blending) -> (a -> DrawColors os s ()) -> Shader os f s ()
drawDepth :: forall a os f s d. DepthRenderable d => FragmentStream (a, FragDepth) -> (s -> (Blending, Image (Format d), DepthOption)) -> (a -> DrawColors os s ()) -> Shader os f s ()
drawStencil :: forall a os f s st. StencilRenderable st => FragmentStream a -> (s -> (Blending, Image (Format st), StencilOptions)) -> (a -> DrawColors os s ()) -> Shader os f s ()
drawDepthStencil :: forall a os f s d st. (DepthRenderable d, StencilRenderable st) => FragmentStream (a, FragDepth) -> (s -> (Blending, Image (Format d), Image (Format st), DepthStencilOption)) -> (a -> DrawColors os s ()) -> Shader os f s ()

makeFBOKeys :: IO [FBOKey] -> IO (Maybe FBOKey) -> IO (Maybe FBOKey) -> IO FBOKeys
makeFBOKeys c d s = do c' <- c
                       d' <- d
                       s' <- s
                       return $ FBOKeys c' d' s'

draw fs sf m = Shader $ tellDrawcalls fs $ \c -> let (sh,g,ioc) = runDrawColors (m c) in (sh, g, f ioc)
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        b = sf s
                    in (Just (makeFBOKeys fbokeyio (return Nothing) (return Nothing), fboio)
                       , io >> glDisable GL_DEPTH_TEST >> glDisable GL_STENCIL_TEST >> setGlBlend b)

drawDepth fs sf m = Shader $ tellDrawcalls fs $ \(c,d) -> let (sh,g,ioc) = runDrawColors (m c) in (sh >> setDepth d, g, f ioc)
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        (b, di, o) = sf s
                    in (Just (makeFBOKeys fbokeyio (Just <$> getImageFBOKey di) (return Nothing)
                             , fboio >> getImageBinding di GL_DEPTH_ATTACHMENT)
                       , io >> glDisable GL_STENCIL_TEST >> setGlBlend b >> setGlDepthOptions o)

drawStencil fs sf m = Shader $ tellDrawcalls fs $ \c -> let (sh,g,ioc) = runDrawColors (m c) in (sh, g, f ioc )
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        (b, si, o) = sf s
                    in (Just (makeFBOKeys fbokeyio (return Nothing) (Just <$> getImageFBOKey si)
                             , fboio >> getImageBinding si GL_STENCIL_ATTACHMENT)
                       , io >> glDisable GL_DEPTH_TEST >> setGlBlend b >> setGlStencilOptions o OpZero OpZero)

drawDepthStencil fs sf m = Shader $ tellDrawcalls fs $ \(c,d) -> let (sh,g,ioc) = runDrawColors (m c) in (sh >> setDepth d, g, f ioc )
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        (b, di, si, o) = sf s
                    in (Just (makeFBOKeys fbokeyio (Just <$> getImageFBOKey di) (Just <$> getImageFBOKey si)
                             , fboio >> getCombinedBinding di si)
                       , io >> setGlBlend b >> setGlDepthStencilOptions o)
          getCombinedBinding di si | imageEquals di si = getImageBinding di GL_DEPTH_STENCIL_ATTACHMENT
                                   | otherwise = getImageBinding di GL_DEPTH_ATTACHMENT >> getImageBinding si GL_STENCIL_ATTACHMENT


drawContextColor :: forall os s c ds. ContextColorFormat c => FragmentStream (FragColor c) -> (s -> ContextColorOption c) -> Shader os (ContextFormat c ds) s ()
drawContextDepth :: forall os s c ds. DepthRenderable ds => FragmentStream FragDepth -> (s -> DepthOption) -> Shader os (ContextFormat c ds) s ()
drawContextColorDepth :: forall os s c ds. (ContextColorFormat c, DepthRenderable ds) => FragmentStream (FragColor c, FragDepth) -> (s -> (ContextColorOption c, DepthOption)) -> Shader os (ContextFormat c ds) s ()
drawContextStencil :: forall os s c ds. StencilRenderable ds => FragmentStream () -> (s -> StencilOptions) -> Shader os (ContextFormat c ds) s ()
drawContextColorStencil :: forall os s c ds. (ContextColorFormat c, StencilRenderable ds) => FragmentStream (FragColor c) -> (s -> (ContextColorOption c, StencilOptions)) -> Shader os (ContextFormat c ds) s ()
drawContextDepthStencil :: forall os s c ds. (DepthRenderable ds, StencilRenderable ds) => FragmentStream FragDepth -> (s -> DepthStencilOption) -> Shader os (ContextFormat c ds) s ()
drawContextColorDepthStencil :: forall os s c ds. (ContextColorFormat c, DepthRenderable ds, StencilRenderable ds) => FragmentStream (FragColor c, FragDepth) -> (s -> (ContextColorOption c, DepthStencilOption)) -> Shader os (ContextFormat c ds) s ()

drawContextColor fs sf = Shader $ tellDrawcalls fs $ \ a -> make3 (setColor cf 0 a) io
                            where io s = (Nothing, glDisable GL_DEPTH_TEST >> glDisable GL_STENCIL_TEST >> setGlContextColorOptions cf (sf s))
                                  cf = undefined :: c

drawContextDepth fs sf = Shader $ tellDrawcalls fs $ \ a-> (setDepth a, return(), io)
                            where io s = (Nothing, glDisable GL_STENCIL_TEST >> setGlDepthOptions (sf s))

drawContextColorDepth fs sf = Shader $ tellDrawcalls fs $ \(c,d) -> let (s, g) = setColor cf 0 c in (s >> setDepth d, g, io)
                                where io s = let (cop, dop) = sf s in (Nothing, glDisable GL_STENCIL_TEST >> setGlContextColorOptions cf cop >> setGlDepthOptions dop)
                                      cf = undefined :: c

drawContextStencil fs sf = Shader $ tellDrawcalls fs $ const (return (), return (), io)
                                where io s = (Nothing, glDisable GL_DEPTH_TEST >> setGlStencilOptions (sf s) OpZero OpZero)

drawContextColorStencil fs sf = Shader $ tellDrawcalls fs $ \ a -> make3 (setColor cf 0 a) io
                                    where io s = let (cop, dop) = sf s in (Nothing, glDisable GL_DEPTH_TEST >> setGlContextColorOptions cf cop >> setGlStencilOptions dop OpZero OpZero)
                                          cf = undefined :: c

drawContextDepthStencil fs sf = Shader $ tellDrawcalls fs $ \ a-> (setDepth a, return(), io)
                                    where io s = (Nothing, setGlDepthStencilOptions (sf s))

drawContextColorDepthStencil fs sf = Shader $ tellDrawcalls fs $ \ (c,d) -> let (s, g) = setColor cf 0 c in (s >> setDepth d, g, io)
                                        where io s = let (cop, dop) = sf s in (Nothing, setGlContextColorOptions cf cop >> setGlDepthStencilOptions dop)
                                              cf = undefined :: c

tellDrawcalls :: FragmentStream a -> (a -> (ExprM (), GlobDeclM (), s -> (Maybe (IO FBOKeys, IO ()), IO ()))) -> ShaderM s ()
tellDrawcalls (FragmentStream xs) f = do
                               let g (x, fd) = tellDrawcall $ makeDrawcall (f x) fd
                               mapM_ g xs

makeDrawcall :: (ExprM (), GlobDeclM (), s -> (Maybe (IO FBOKeys, IO ()), IO ())) -> FragmentStreamData -> IO (Drawcall s)
makeDrawcall (sh, shd, io) (FragmentStreamData rastN shaderpos (PrimitiveStreamData primN) keep) =
       do (fsource, funis, fsamps, _, prevDecls, prevS) <- runExprM shd (discard keep >> sh)
          (vsource, vunis, vsamps, vinps, _, _) <- runExprM prevDecls (prevS >> shaderpos)
          let unis = orderedUnion funis vunis
              samps = orderedUnion fsamps vsamps
          return $ Drawcall io primN rastN vsource fsource vinps unis samps

orderedUnion :: Ord a => [a] -> [a] -> [a]
orderedUnion xxs@(x:xs) yys@(y:ys) | x == y    = x : orderedUnion xs ys
                                   | x < y     = x : orderedUnion xs yys
                                   | otherwise = y : orderedUnion xxs ys
orderedUnion xs [] = xs
orderedUnion [] ys = ys


setColor :: forall c. ColorSampleable c => c -> Int -> FragColor c -> (ExprM (), GlobDeclM ())
setColor ct n c = let    name = "out" ++ show n
                         typeS = typeStr ct
                  in (do xs <- mapM unS (fromColor ct c :: [S F (ColorElement c)])
                         tellAssignment' name (typeS ++ "(" ++ intercalate "," xs ++ ")")
                         ,
                      do tellGlobal "layout(location = "
                         tellGlobal $ show n
                         tellGlobal ") out "
                         tellGlobal typeS
                         tellGlobal " "
                         tellGlobalLn name)

setDepth :: FFloat -> ExprM ()
setDepth (S d) = do d' <- d
                    when (d' /= "gl_FragDepth") $
                        tellAssignment' "gl_FragDepth" d'

make3 :: (t, t1) -> t2 -> (t, t1, t2)
make3 (a,b) c = (a,b,c)

type FragColor c = Color c (S F (ColorElement c))
type FragDepth = FFloat

setGlColorMask :: ColorSampleable f => f -> GLuint -> Color f Bool -> IO ()
setGlColorMask c i mask = glColorMaski i x y z w
    where [x,y,z,w] = map fromBool $ take 4 $ fromColor c mask ++ [False, False, False]

setGlContextColorOptions :: ColorSampleable f => f -> ContextColorOption f -> IO ()
setGlContextColorOptions c (ContextColorOption blend mask) = do
        setGlColorMask c 0 mask
        setGlBlend blend
        case blend of
            NoBlending -> glDisable GL_BLEND
            LogicOp _ -> glDisable GL_BLEND
            _ -> glEnable GL_BLEND

setGlBlend :: Blending -> IO ()
setGlBlend NoBlending = return ()
setGlBlend (BlendRgbAlpha (e, ea) (BlendingFactors sf df, BlendingFactors sfa dfa) (r, g, b, a)) = do
                            glBlendEquationSeparate (getGlBlendEquation e) (getGlBlendEquation ea)
                            glBlendFuncSeparate (getGlBlendFunc sf) (getGlBlendFunc df) (getGlBlendFunc sfa) (getGlBlendFunc dfa)
                            glBlendColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
setGlBlend (LogicOp op) = glEnable GL_COLOR_LOGIC_OP >> glLogicOp (getGlLogicOp op)


setGlDepthOptions :: DepthOption -> IO ()
setGlDepthOptions (DepthOption df dm) = do glEnable GL_DEPTH_TEST
                                           glDepthFunc (getGlCompFunc df)
                                           glDepthMask $ fromBool dm

setGlStencilOptions :: FrontBack StencilOption -> StencilOp -> StencilOp -> IO ()
setGlStencilOptions (FrontBack (StencilOption ft fr ff fp frm fwm) (StencilOption bt br bf bp brm bwm)) fdf bdf = do
    glEnable GL_STENCIL_TEST
    glStencilFuncSeparate GL_FRONT (getGlCompFunc ft) (fromIntegral fr) (fromIntegral frm)
    glStencilOpSeparate GL_FRONT (getGlStencilOp ff) (getGlStencilOp fdf) (getGlStencilOp fp)
    glStencilMaskSeparate GL_FRONT (fromIntegral fwm)
    glStencilFuncSeparate GL_BACK (getGlCompFunc bt) (fromIntegral br) (fromIntegral brm)
    glStencilOpSeparate GL_BACK (getGlStencilOp bf) (getGlStencilOp bdf) (getGlStencilOp bp)
    glStencilMaskSeparate GL_BACK (fromIntegral bwm)

setGlDepthStencilOptions :: DepthStencilOption -> IO ()
setGlDepthStencilOptions (DepthStencilOption sop dop (FrontBack fdf bdf)) = do
    setGlDepthOptions dop
    setGlStencilOptions sop fdf bdf

data ContextColorOption f = ContextColorOption Blending (ColorMask f)

data DepthOption = DepthOption DepthFunction DepthMask
type StencilOptions = FrontBack StencilOption
data StencilOption = StencilOption {
                            stencilTest         :: ComparisonFunction,
                            stencilReference    :: Int,
                            opWhenStencilFail   :: StencilOp,
                            opWhenStencilPass   :: StencilOp,
                            stencilReadBitMask  :: Word,
                            stencilWriteBitMask :: Word
                            }
data DepthStencilOption = DepthStencilOption {
                            dsStencilOptions              :: StencilOptions,
                            dsDepthOption                 :: DepthOption ,
                            opWhenStencilPassButDepthFail :: FrontBack StencilOp
                            }

data FrontBack a = FrontBack { front :: a, back :: a }

-- | 'True' for each color component that should be written to the 'ShaderBuffer'.
type ColorMask f = Color f Bool
-- | 'True' if the depth component should be written to the 'ShaderBuffer'.
type DepthMask = Bool
-- | The function used to compare the fragment's depth and the depth buffers depth with.
type DepthFunction = ComparisonFunction

type UseBlending = Bool

-- | Sets how the painted colors are blended with the 'ShaderBuffer's previous value.
data Blending =
      NoBlending
    | BlendRgbAlpha (BlendEquation, BlendEquation) (BlendingFactors, BlendingFactors) ConstantColor
    | LogicOp LogicOp

type ConstantColor = (Float, Float, Float, Float)

data BlendingFactors = BlendingFactors { blendFactorSrc :: BlendingFactor, blendFactorDst :: BlendingFactor }

data BlendEquation =
     FuncAdd
   | FuncSubtract
   | FuncReverseSubtract
   | Min
   | Max

data BlendingFactor =
     Zero
   | One
   | SrcColor
   | OneMinusSrcColor
   | DstColor
   | OneMinusDstColor
   | SrcAlpha
   | OneMinusSrcAlpha
   | DstAlpha
   | OneMinusDstAlpha
   | ConstantColor
   | OneMinusConstantColor
   | ConstantAlpha
   | OneMinusConstantAlpha
   | SrcAlphaSaturate

data LogicOp =
     Clear
   | And
   | AndReverse
   | Copy
   | AndInverted
   | Noop
   | Xor
   | Or
   | Nor
   | Equiv
   | Invert
   | OrReverse
   | CopyInverted
   | OrInverted
   | Nand
   | Set

-- | Sets the operations that should be performed on the 'ShaderBuffer's stencil value
data StencilOp =
     OpZero
   | OpKeep
   | OpReplace
   | OpIncr
   | OpIncrWrap
   | OpDecr
   | OpDecrWrap
   | OpInvert

-----------

clearColorImage :: forall c os f. ColorRenderable c => Image c -> Color c (ColorElement c) -> Render os f ()
clearColorImage i c = do cd <- getRenderContextData
                         key <- Render $ lift $ getImageFBOKey i
                         let fbokey = FBOKeys [key] Nothing Nothing
                         mfbo <- Render $ lift $ getFBO cd fbokey
                         case mfbo of
                                Just fbo -> Render $ lift $ do fbo' <- readIORef fbo
                                                               glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                Nothing -> do fAdd <- getRenderContextFinalizerAdder
                                              Render $ lift $ do 
                                                  fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
                                                  fbo <- newIORef fbo'
                                                  void $ fAdd fbo $ with fbo' (glDeleteFramebuffers 1)
                                                  setFBO cd fbokey fbo
                                                  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                  glEnable GL_FRAMEBUFFER_SRGB
                                                  getImageBinding i GL_COLOR_ATTACHMENT0
                                                  withArray [GL_COLOR_ATTACHMENT0] $ glDrawBuffers 1
                         Render $ lift $ do glDisable GL_SCISSOR_TEST
                                            clearColor (undefined :: c) c
                                            glEnable GL_SCISSOR_TEST

clearDepthImage :: DepthRenderable d => Image d -> Float -> Render os f ()
clearDepthImage i d = do cd <- getRenderContextData
                         key <- Render $ lift $ getImageFBOKey i
                         let fbokey = FBOKeys [] (Just key) Nothing
                         mfbo <- Render $ lift $ getFBO cd fbokey
                         case mfbo of
                                Just fbo -> Render $ lift $ do fbo' <- readIORef fbo
                                                               glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                Nothing -> do fAdd <- getRenderContextFinalizerAdder
                                              Render $ lift $ do 
                                                  fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
                                                  fbo <- newIORef fbo'
                                                  void $ fAdd fbo $ with fbo' (glDeleteFramebuffers 1)
                                                  setFBO cd fbokey fbo
                                                  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                  glEnable GL_FRAMEBUFFER_SRGB
                                                  getImageBinding i GL_DEPTH_ATTACHMENT
                                                  glDrawBuffers 0 nullPtr
                         Render $ lift $ do glDisable GL_SCISSOR_TEST
                                            with (realToFrac d) $ glClearBufferfv GL_DEPTH 0
                                            glEnable GL_SCISSOR_TEST

clearStencilImage :: StencilRenderable s => Image s -> Int -> Render os f ()
clearStencilImage i s = do cd <- getRenderContextData
                           key <- Render $ lift $ getImageFBOKey i
                           let fbokey = FBOKeys [] Nothing (Just key)
                           mfbo <- Render $ lift $ getFBO cd fbokey
                           case mfbo of
                                Just fbo -> Render $ lift $ do fbo' <- readIORef fbo
                                                               glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                Nothing -> do fAdd <- getRenderContextFinalizerAdder
                                              Render $ lift $ do 
                                                  fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
                                                  fbo <- newIORef fbo'
                                                  void $ fAdd fbo $ with fbo' (glDeleteFramebuffers 1)
                                                  setFBO cd fbokey fbo
                                                  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                  glEnable GL_FRAMEBUFFER_SRGB
                                                  getImageBinding i GL_STENCIL_ATTACHMENT
                                                  glDrawBuffers 0 nullPtr
                           Render $ lift $ do glDisable GL_SCISSOR_TEST 
                                              with (fromIntegral s) $ glClearBufferiv GL_STENCIL 0
                                              glEnable GL_SCISSOR_TEST

clearDepthStencilImage :: Image DepthStencil -> Float -> Int -> Render os f ()
clearDepthStencilImage i d s = do
                           cd <- getRenderContextData
                           key <- Render $ lift $ getImageFBOKey i
                           let fbokey = FBOKeys [] Nothing (Just key)
                           mfbo <- Render $ lift $ getFBO cd fbokey
                           case mfbo of
                                Just fbo -> Render $ lift $ do fbo' <- readIORef fbo
                                                               glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                Nothing -> do fAdd <- getRenderContextFinalizerAdder
                                              Render $ lift $ do 
                                                  fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
                                                  fbo <- newIORef fbo'
                                                  void $ fAdd fbo $ with fbo' (glDeleteFramebuffers 1)
                                                  setFBO cd fbokey fbo
                                                  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                  glEnable GL_FRAMEBUFFER_SRGB
                                                  getImageBinding i GL_DEPTH_STENCIL_ATTACHMENT
                                                  glDrawBuffers 0 nullPtr
                           Render $ lift $ do glDisable GL_SCISSOR_TEST 
                                              glClearBufferfi GL_DEPTH_STENCIL 0 (realToFrac d) (fromIntegral s)
                                              glEnable GL_SCISSOR_TEST 

clearContextColor :: forall os c ds. ContextColorFormat c => Color c Float -> Render os (ContextFormat c ds) ()
clearContextColor c = Render $ lift $ do glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                                         glDisable GL_SCISSOR_TEST 
                                         withArray (map realToFrac (fromColor (undefined :: c) c ++ replicate 3 0 :: [Float])) $ glClearBufferfv GL_COLOR 0
                                         glEnable GL_SCISSOR_TEST 

clearContextDepth :: DepthRenderable ds => Float -> Render os (ContextFormat c ds) ()
clearContextDepth d = Render $ lift $ do glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                                         glDisable GL_SCISSOR_TEST 
                                         with (realToFrac d) $ glClearBufferfv GL_DEPTH 0
                                         glEnable GL_SCISSOR_TEST 

clearContextStencil :: StencilRenderable ds => Int -> Render os (ContextFormat c ds) ()
clearContextStencil s = Render $ lift $ do glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                                           glDisable GL_SCISSOR_TEST 
                                           with (fromIntegral s) $ glClearBufferiv GL_STENCIL 0
                                           glEnable GL_SCISSOR_TEST

clearContextDepthStencil :: Float -> Int -> Render os (ContextFormat c DepthStencil) ()
clearContextDepthStencil d s = Render $ lift $ do glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                                                  glDisable GL_SCISSOR_TEST 
                                                  glClearBufferfi GL_DEPTH_STENCIL 0 (realToFrac d) (fromIntegral s)
                                                  glEnable GL_SCISSOR_TEST

---------------


getGlBlendEquation :: BlendEquation -> GLenum
getGlBlendEquation FuncAdd = GL_FUNC_ADD
getGlBlendEquation FuncSubtract = GL_FUNC_SUBTRACT
getGlBlendEquation FuncReverseSubtract = GL_FUNC_REVERSE_SUBTRACT
getGlBlendEquation Min = GL_MIN
getGlBlendEquation Max = GL_MAX

getGlBlendFunc :: BlendingFactor -> GLenum
getGlBlendFunc Zero = GL_ZERO
getGlBlendFunc One = GL_ONE
getGlBlendFunc SrcColor = GL_SRC_COLOR
getGlBlendFunc OneMinusSrcColor = GL_ONE_MINUS_SRC_COLOR
getGlBlendFunc DstColor = GL_DST_COLOR
getGlBlendFunc OneMinusDstColor = GL_ONE_MINUS_DST_COLOR
getGlBlendFunc SrcAlpha = GL_SRC_ALPHA
getGlBlendFunc OneMinusSrcAlpha = GL_ONE_MINUS_SRC_ALPHA
getGlBlendFunc DstAlpha = GL_DST_ALPHA
getGlBlendFunc OneMinusDstAlpha = GL_ONE_MINUS_DST_ALPHA
getGlBlendFunc ConstantColor = GL_CONSTANT_COLOR
getGlBlendFunc OneMinusConstantColor = GL_ONE_MINUS_CONSTANT_COLOR
getGlBlendFunc ConstantAlpha = GL_CONSTANT_ALPHA
getGlBlendFunc OneMinusConstantAlpha = GL_ONE_MINUS_CONSTANT_ALPHA
getGlBlendFunc SrcAlphaSaturate = GL_SRC_ALPHA_SATURATE

getGlLogicOp :: LogicOp -> GLenum
getGlLogicOp Clear = GL_CLEAR
getGlLogicOp And = GL_AND
getGlLogicOp AndReverse = GL_AND_REVERSE
getGlLogicOp Copy = GL_COPY
getGlLogicOp AndInverted = GL_AND_INVERTED
getGlLogicOp Noop = GL_NOOP
getGlLogicOp Xor = GL_XOR
getGlLogicOp Or = GL_OR
getGlLogicOp Nor = GL_NOR
getGlLogicOp Equiv = GL_EQUIV
getGlLogicOp Invert = GL_INVERT
getGlLogicOp OrReverse = GL_OR_REVERSE
getGlLogicOp CopyInverted = GL_COPY_INVERTED
getGlLogicOp OrInverted = GL_OR_INVERTED
getGlLogicOp Nand = GL_NAND
getGlLogicOp Set = GL_SET

getGlStencilOp :: StencilOp -> GLenum
getGlStencilOp OpZero = GL_ZERO
getGlStencilOp OpKeep = GL_KEEP
getGlStencilOp OpReplace = GL_REPLACE
getGlStencilOp OpIncr = GL_INCR
getGlStencilOp OpIncrWrap = GL_INCR_WRAP
getGlStencilOp OpDecr = GL_DECR
getGlStencilOp OpDecrWrap = GL_DECR_WRAP
getGlStencilOp OpInvert = GL_INVERT

   --------------------
