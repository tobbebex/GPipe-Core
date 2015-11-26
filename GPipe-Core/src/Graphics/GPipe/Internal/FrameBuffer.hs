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
import Linear.V4

-- | A monad in which individual color images can be drawn.
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

-- | Draw color values into a color renderable texture image.
drawColor :: forall c s os. ColorRenderable c => (s -> (Image (Format c), ColorMask c, UseBlending)) -> FragColor c -> DrawColors os s ()
drawColor sf c = DrawColors $ do n <- get
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

-- | Draw all fragments in a 'FragmentStream' using the provided function that passes each fragment value into a 'DrawColors' monad. The first argument is a function
--   that retrieves a 'Blending' setting from the shader environment, which will be used for all 'drawColor' actions in the 'DrawColors' monad where 'UseBlending' is 'True'.
--   (OpenGl 3.3 unfortunately doesn't support having different blending settings for different color targets.)
draw :: forall a os f s. (s -> Blending) -> FragmentStream a -> (a -> DrawColors os s ()) -> Shader os f s ()
-- | Like 'draw', but performs a depth test on each fragment first. The 'DrawColors' monad is then only run for fragments where the depth test passes.
drawDepth :: forall a os f s d. DepthRenderable d => (s -> (Blending, Image (Format d), DepthOption)) -> FragmentStream (a, FragDepth) -> (a -> DrawColors os s ()) -> Shader os f s ()
-- | Like 'draw', but performs a stencil test on each fragment first. The 'DrawColors' monad is then only run for fragments where the stencil test passes.
drawStencil :: forall a os f s st. StencilRenderable st => (s -> (Blending, Image (Format st), StencilOptions)) -> FragmentStream a -> (a -> DrawColors os s ()) -> Shader os f s ()
-- | Like 'draw', but performs a stencil test and a depth test (in that order) on each fragment first. The 'DrawColors' monad is then only run for fragments where the stencil and depth test passes.
drawDepthStencil :: forall a os f s d st. (DepthRenderable d, StencilRenderable st) => (s -> (Blending, Image (Format d), Image (Format st), DepthStencilOption)) -> FragmentStream (a, FragDepth) -> (a -> DrawColors os s ()) -> Shader os f s ()

makeFBOKeys :: IO [FBOKey] -> IO (Maybe FBOKey) -> IO (Maybe FBOKey) -> IO FBOKeys
makeFBOKeys c d s = do c' <- c
                       d' <- d
                       s' <- s
                       return $ FBOKeys c' d' s'

draw sf fs m = Shader $ tellDrawcalls fs $ \c -> let (sh,g,ioc) = runDrawColors (m c) in (sh, g, f ioc)
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        b = sf s
                    in (Just (makeFBOKeys fbokeyio (return Nothing) (return Nothing), fboio)
                       , io >> glDisable GL_DEPTH_TEST >> glDisable GL_STENCIL_TEST >> setGlBlend b)

drawDepth sf fs m = Shader $ tellDrawcalls fs $ \(c,d) -> let (sh,g,ioc) = runDrawColors (m c) in (sh >> setDepth d, g, f ioc)
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        (b, di, o) = sf s
                    in (Just (makeFBOKeys fbokeyio (Just <$> getImageFBOKey di) (return Nothing)
                             , fboio >> getImageBinding di GL_DEPTH_ATTACHMENT)
                       , io >> glDisable GL_STENCIL_TEST >> setGlBlend b >> setGlDepthOptions o)

drawStencil sf fs m = Shader $ tellDrawcalls fs $ \c -> let (sh,g,ioc) = runDrawColors (m c) in (sh, g, f ioc )
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        (b, si, o) = sf s
                    in (Just (makeFBOKeys fbokeyio (return Nothing) (Just <$> getImageFBOKey si)
                             , fboio >> getImageBinding si GL_STENCIL_ATTACHMENT)
                       , io >> glDisable GL_DEPTH_TEST >> setGlBlend b >> setGlStencilOptions o OpZero OpZero)

drawDepthStencil sf fs m = Shader $ tellDrawcalls fs $ \(c,d) -> let (sh,g,ioc) = runDrawColors (m c) in (sh >> setDepth d, g, f ioc )
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        (b, di, si, o) = sf s
                    in (Just (makeFBOKeys fbokeyio (Just <$> getImageFBOKey di) (Just <$> getImageFBOKey si)
                             , fboio >> getCombinedBinding di si)
                       , io >> setGlBlend b >> setGlDepthStencilOptions o)
          getCombinedBinding di si | imageEquals di si = getImageBinding di GL_DEPTH_STENCIL_ATTACHMENT
                                   | otherwise = getImageBinding di GL_DEPTH_ATTACHMENT >> getImageBinding si GL_STENCIL_ATTACHMENT

-- | Draw color values from a 'FragmentStream' into the context window.
drawContextColor :: forall os s c ds. ContextColorFormat c => (s -> ContextColorOption c) -> FragmentStream (FragColor c) -> Shader os (ContextFormat c ds) s ()
-- | Perform a depth test for each fragment from a 'FragmentStream' in the context window. This doesn't draw any color values and only affects the depth buffer.
drawContextDepth :: forall os s c ds. DepthRenderable ds => (s -> DepthOption) -> FragmentStream FragDepth -> Shader os (ContextFormat c ds) s ()
-- | Perform a depth test for each fragment from a 'FragmentStream' and write a color value from each fragment that passes the test into the context window.
drawContextColorDepth :: forall os s c ds. (ContextColorFormat c, DepthRenderable ds) => (s -> (ContextColorOption c, DepthOption)) -> FragmentStream (FragColor c, FragDepth) -> Shader os (ContextFormat c ds) s ()
-- | Perform a stencil test for each fragment from a 'FragmentStream' in the context window. This doesn't draw any color values and only affects the stencil buffer.
drawContextStencil :: forall os s c ds. StencilRenderable ds => (s -> StencilOptions) -> FragmentStream () -> Shader os (ContextFormat c ds) s ()
-- | Perform a stencil test for each fragment from a 'FragmentStream' and write a color value from each fragment that passes the test into the context window.
drawContextColorStencil :: forall os s c ds. (ContextColorFormat c, StencilRenderable ds) => (s -> (ContextColorOption c, StencilOptions)) -> FragmentStream (FragColor c) -> Shader os (ContextFormat c ds) s ()
-- | Perform a stencil test and depth test (in that order) for each fragment from a 'FragmentStream' in the context window. This doesnt draw any color values and only affects the depth and stencil buffer.
drawContextDepthStencil :: forall os s c ds. (DepthRenderable ds, StencilRenderable ds) => (s -> DepthStencilOption) -> FragmentStream FragDepth -> Shader os (ContextFormat c ds) s ()
-- | Perform a stencil test and depth test (in that order) for each fragment from a 'FragmentStream' and write a color value from each fragment that passes the tests into the context window.
drawContextColorDepthStencil :: forall os s c ds. (ContextColorFormat c, DepthRenderable ds, StencilRenderable ds) => (s -> (ContextColorOption c, DepthStencilOption)) -> FragmentStream (FragColor c, FragDepth) -> Shader os (ContextFormat c ds) s ()

drawContextColor sf fs = Shader $ tellDrawcalls fs $ \ a -> make3 (setColor cf 0 a) io
                            where io s = (Nothing, glDisable GL_DEPTH_TEST >> glDisable GL_STENCIL_TEST >> setGlContextColorOptions cf (sf s))
                                  cf = undefined :: c

drawContextDepth sf fs = Shader $ tellDrawcalls fs $ \ a-> (setDepth a, return(), io)
                            where io s = (Nothing, glDisable GL_STENCIL_TEST >> setGlDepthOptions (sf s))

drawContextColorDepth sf fs = Shader $ tellDrawcalls fs $ \(c,d) -> let (s, g) = setColor cf 0 c in (s >> setDepth d, g, io)
                                where io s = let (cop, dop) = sf s in (Nothing, glDisable GL_STENCIL_TEST >> setGlContextColorOptions cf cop >> setGlDepthOptions dop)
                                      cf = undefined :: c

drawContextStencil sf fs = Shader $ tellDrawcalls fs $ const (return (), return (), io)
                                where io s = (Nothing, glDisable GL_DEPTH_TEST >> setGlStencilOptions (sf s) OpZero OpZero)

drawContextColorStencil sf fs = Shader $ tellDrawcalls fs $ \ a -> make3 (setColor cf 0 a) io
                                    where io s = let (cop, dop) = sf s in (Nothing, glDisable GL_DEPTH_TEST >> setGlContextColorOptions cf cop >> setGlStencilOptions dop OpZero OpZero)
                                          cf = undefined :: c

drawContextDepthStencil sf fs = Shader $ tellDrawcalls fs $ \ a-> (setDepth a, return(), io)
                                    where io s = (Nothing, setGlDepthStencilOptions (sf s))

drawContextColorDepthStencil sf fs = Shader $ tellDrawcalls fs $ \ (c,d) -> let (s, g) = setColor cf 0 c in (s >> setDepth d, g, io)
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
          return $ Drawcall io primN rastN vsource fsource vinps vunis vsamps funis fsamps

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
setGlBlend (BlendRgbAlpha (e, ea) (BlendingFactors sf df, BlendingFactors sfa dfa) (V4 r g b a)) = do
                            glBlendEquationSeparate (getGlBlendEquation e) (getGlBlendEquation ea)
                            glBlendFuncSeparate (getGlBlendFunc sf) (getGlBlendFunc df) (getGlBlendFunc sfa) (getGlBlendFunc dfa)
                            when (usesConstantColor sf || usesConstantColor df || usesConstantColor sfa || usesConstantColor dfa) $
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

-- | 'True' for each color component that should be written to the target.
type ColorMask f = Color f Bool
-- | 'True' if the depth component should be written to the target.
type DepthMask = Bool
-- | The function used to compare the fragment's depth and the depth buffers depth with. E.g. 'Less' means "where fragment's depth is less than the buffers current depth".
type DepthFunction = ComparisonFunction

-- | Indicates whether this color draw should use the 'Blending' setting given to the draw action. If this is 'False', the fragment's color value will simply replace the
--   target value.
type UseBlending = Bool

-- | Denotes how each fragment's color value should be blended with the target value.
data Blending =
      -- | The fragment's color will simply replace the target value.
      NoBlending
      -- | The fragment's color will be blended using an equation and a set of factors for the RGB components, and a separate equation and set of factors for the Alpha component (if present), and a 'ConstantColor' that may be referenced from the 'BlendingFactors'. The 'ConstantColor' may be 'undefined' if none of the 'BlendingFactors' needs it.
      --   This kind of blending will only be made on colors with a 'Float' representation (e.g. 'RGB8' or 'RGB32F', but not 'RGB8I'), integer colors will simply replace the target value.
    | BlendRgbAlpha (BlendEquation, BlendEquation) (BlendingFactors, BlendingFactors) ConstantColor
      -- | A logical operation that will be done on the bits of the fragment color and the target color. This kind of blending is only done on colors that has a
      --   integral /internal/ representation (e.g. 'RGB8' or 'RGB8I', but not 'RGB32F'; note the difference with @BlendRgbAlpha@ restriction). For targets with an internal floating point representation, the fragment value will simply replace the target value.
    | LogicOp LogicOp

type ConstantColor = V4 Float

-- | A set of blending factors used for the source (fragment) and the destination (target).
data BlendingFactors = BlendingFactors { blendFactorSrc :: BlendingFactor, blendFactorDst :: BlendingFactor }

-- | The equation used to combine the source (fragment) and the destination (target) after they have been multiplied with their respective 'BlendingFactor's.
data BlendEquation =
     FuncAdd
   | FuncSubtract
   | FuncReverseSubtract
   | Min
   | Max

-- | A factor that the source (fragment) or the destination (target) will be multiplied with before combined with the other in the 'BlendEquation'.
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

usesConstantColor ConstantColor = True
usesConstantColor OneMinusConstantColor = True
usesConstantColor ConstantAlpha = True
usesConstantColor OneMinusConstantAlpha = True
usesConstantColor _ = False

-- | A bitwise logical operation that will be used to combine colors that has an integral internal representation.
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

-- | Denotes the operation that will be performed on the target's stencil value
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

-- | Fill a color image with a constant color value
clearColorImage :: forall c os f. ColorRenderable c => Image (Format c) -> Color c (ColorElement c) -> Render os f ()
clearColorImage i c = do cd <- getRenderContextData
                         key <- Render $ lift $ lift $ lift $ getImageFBOKey i
                         let fbokey = FBOKeys [key] Nothing Nothing
                         mfbo <- Render $ lift $ lift $ lift $ getFBO cd fbokey
                         case mfbo of
                                Just fbo -> Render $ lift $ lift $ lift $ do
                                                                      fbo' <- readIORef fbo
                                                                      glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                Nothing -> do fAdd <- getRenderContextFinalizerAdder
                                              Render $ throwFromMaybe $ lift $ lift $ lift $ do
                                                  fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
                                                  fbo <- newIORef fbo'
                                                  void $ fAdd fbo $ with fbo' (glDeleteFramebuffers 1)
                                                  setFBO cd fbokey fbo
                                                  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                  glEnable GL_FRAMEBUFFER_SRGB
                                                  getImageBinding i GL_COLOR_ATTACHMENT0
                                                  withArray [GL_COLOR_ATTACHMENT0] $ glDrawBuffers 1
                                                  getFBOerror

                         Render $ lift $ lift $ lift $ do
                                                   glDisable GL_SCISSOR_TEST
                                                   glColorMask glTrue glTrue glTrue glTrue
                                                   clearColor (undefined :: c) c
                                                   glEnable GL_SCISSOR_TEST

-- | Fill a depth image with a constant depth value (in the range [0,1])
clearDepthImage :: DepthRenderable d => Image (Format d) -> Float -> Render os f ()
clearDepthImage i d = do cd <- getRenderContextData
                         key <- Render $ lift $ lift $ lift $ getImageFBOKey i
                         let fbokey = FBOKeys [] (Just key) Nothing
                         mfbo <- Render $ lift $ lift $ lift $ getFBO cd fbokey
                         case mfbo of
                                Just fbo -> Render $ lift $ lift $ lift $ do
                                                                      fbo' <- readIORef fbo
                                                                      glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                Nothing -> do fAdd <- getRenderContextFinalizerAdder
                                              Render $ throwFromMaybe $ lift $ lift $ lift $ do
                                                  fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
                                                  fbo <- newIORef fbo'
                                                  void $ fAdd fbo $ with fbo' (glDeleteFramebuffers 1)
                                                  setFBO cd fbokey fbo
                                                  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                  glEnable GL_FRAMEBUFFER_SRGB
                                                  getImageBinding i GL_DEPTH_ATTACHMENT
                                                  glDrawBuffers 0 nullPtr
                                                  getFBOerror
                         Render $ lift $ lift $ lift $ do
                                                   glDisable GL_SCISSOR_TEST
                                                   glDepthMask glTrue
                                                   with (realToFrac d) $ glClearBufferfv GL_DEPTH 0
                                                   glEnable GL_SCISSOR_TEST

-- | Fill a depth image with a constant stencil value
clearStencilImage :: StencilRenderable s => Image (Format s) -> Int -> Render os f ()
clearStencilImage i s = do cd <- getRenderContextData
                           key <- Render $ lift $ lift $ lift $ getImageFBOKey i
                           let fbokey = FBOKeys [] Nothing (Just key)
                           mfbo <- Render $ lift $ lift $ lift $ getFBO cd fbokey
                           case mfbo of
                                Just fbo -> Render $ lift $ lift $ lift $ do
                                                                      fbo' <- readIORef fbo
                                                                      glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                Nothing -> do fAdd <- getRenderContextFinalizerAdder
                                              Render $ throwFromMaybe $ lift $ lift $ lift $ do
                                                  fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
                                                  fbo <- newIORef fbo'
                                                  void $ fAdd fbo $ with fbo' (glDeleteFramebuffers 1)
                                                  setFBO cd fbokey fbo
                                                  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                  glEnable GL_FRAMEBUFFER_SRGB
                                                  getImageBinding i GL_STENCIL_ATTACHMENT
                                                  glDrawBuffers 0 nullPtr
                                                  getFBOerror
                           Render $ lift $ lift $ lift $ do
                                                     glDisable GL_SCISSOR_TEST
                                                     glStencilMask maxBound
                                                     with (fromIntegral s) $ glClearBufferiv GL_STENCIL 0
                                                     glEnable GL_SCISSOR_TEST

-- | Fill a combined depth stencil image with a constant depth value (in the range [0,1]) and a constant stencil value
clearDepthStencilImage :: Image (Format DepthStencil) -> Float -> Int -> Render os f ()
clearDepthStencilImage i d s = do
                           cd <- getRenderContextData
                           key <- Render $ lift $ lift $ lift $ getImageFBOKey i
                           let fbokey = FBOKeys [] Nothing (Just key)
                           mfbo <- Render $ lift $ lift $ lift $ getFBO cd fbokey
                           case mfbo of
                                Just fbo -> Render $ lift $ lift $ lift $ do
                                                                      fbo' <- readIORef fbo
                                                                      glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                Nothing -> do fAdd <- getRenderContextFinalizerAdder
                                              Render $ throwFromMaybe $ lift $ lift $ lift $ do
                                                  fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
                                                  fbo <- newIORef fbo'
                                                  void $ fAdd fbo $ with fbo' (glDeleteFramebuffers 1)
                                                  setFBO cd fbokey fbo
                                                  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo'
                                                  glEnable GL_FRAMEBUFFER_SRGB
                                                  getImageBinding i GL_DEPTH_STENCIL_ATTACHMENT
                                                  glDrawBuffers 0 nullPtr
                                                  getFBOerror
                           Render $ lift $ lift $ lift $ do
                                                     glDisable GL_SCISSOR_TEST
                                                     glDepthMask glTrue
                                                     glStencilMask maxBound
                                                     glClearBufferfi GL_DEPTH_STENCIL 0 (realToFrac d) (fromIntegral s)
                                                     glEnable GL_SCISSOR_TEST

-- | Fill the context window's back buffer with a constant color value
clearContextColor :: forall os c ds. ContextColorFormat c => Color c Float -> Render os (ContextFormat c ds) ()
clearContextColor c = Render $ lift $ lift $ lift $ do
                                                glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                                                glDisable GL_SCISSOR_TEST
                                                glColorMask glTrue glTrue glTrue glTrue
                                                withArray (map realToFrac (fromColor (undefined :: c) c ++ replicate 3 0 :: [Float])) $ glClearBufferfv GL_COLOR 0
                                                glEnable GL_SCISSOR_TEST

-- | Fill the context window's back depth buffer with a constant depth value (in the range [0,1])
clearContextDepth :: DepthRenderable ds => Float -> Render os (ContextFormat c ds) ()
clearContextDepth d = Render $ lift $ lift $ lift $ do
                                                glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                                                glDisable GL_SCISSOR_TEST
                                                glDepthMask glTrue
                                                with (realToFrac d) $ glClearBufferfv GL_DEPTH 0
                                                glEnable GL_SCISSOR_TEST

-- | Fill the context window's back stencil buffer with a constant stencil value
clearContextStencil :: StencilRenderable ds => Int -> Render os (ContextFormat c ds) ()
clearContextStencil s = Render $ lift $ lift $ lift $ do
                                                  glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                                                  glDisable GL_SCISSOR_TEST
                                                  glStencilMask maxBound
                                                  with (fromIntegral s) $ glClearBufferiv GL_STENCIL 0
                                                  glEnable GL_SCISSOR_TEST

-- | Fill the context window's back depth and stencil buffers with a constant depth value (in the range [0,1]) and a constant stencil value
clearContextDepthStencil :: Float -> Int -> Render os (ContextFormat c DepthStencil) ()
clearContextDepthStencil d s = Render $ lift $ lift $ lift $ do
                                                         glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
                                                         glDisable GL_SCISSOR_TEST
                                                         glDepthMask glTrue
                                                         glStencilMask maxBound
                                                         glClearBufferfi GL_DEPTH_STENCIL 0 (realToFrac d) (fromIntegral s)
                                                         glEnable GL_SCISSOR_TEST

---------------
glTrue :: Num n => n
glTrue = fromBool True


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
