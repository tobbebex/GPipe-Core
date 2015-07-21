{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, TupleSections #-}
module Graphics.GPipe.FrameBuffer where

import Graphics.GPipe.Expr
import Graphics.GPipe.FragmentStream
import Graphics.GPipe.Format
import Graphics.GPipe.Shader
import Graphics.GPipe.PrimitiveStream
import Graphics.GPipe.Compiler
import Graphics.GPipe.Texture
import Graphics.GPipe.Context
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.Writer.Lazy
import Data.List (intercalate)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Data.Vec

import Graphics.Rendering.OpenGL.Raw.Core33
import Foreign.Marshal.Utils
import Data.Word (Word)

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
drawColor :: forall c s os. ColorRenderable c => FragColor c -> (s -> (Image c, ColorMask c, UseBlending)) -> DrawColors os s ()
drawColor c sf = DrawColors $ do n <- get
                                 put $ n+1
                                 lift $ tell [\ix -> make3  (setColor cf ix c) $ \s -> let (i, mask, o) = sf s
                                                                                           n' = fromIntegral n 
                                                                                           useblend = if o then glEnablei gl_BLEND n' else glDisablei gl_BLEND n'
                                                                                       in (getImageFBOKey i, 
                                                                                           getImageBinding i (gl_COLOR_ATTACHMENT0 + n'), 
                                                                                            do 
                                                                                             useblend
                                                                                             setGlColorMask cf n' mask) 
                                                                                       ]
    where cf = undefined :: c

draw :: forall a os f s. FragmentStream a -> (s -> Blending) -> (a -> DrawColors os s ()) -> Shader os f s ()
drawDepth :: forall a os f s d. DepthRenderable d => FragmentStream (a, FragDepth) -> (s -> (Blending, Image d, DepthOption)) -> (a -> DrawColors os s ()) -> Shader os f s ()
drawStencil :: forall a os f s st. StencilRenderable st => FragmentStream a -> (s -> (Blending, Image st, StencilOptions)) -> (a -> DrawColors os s ()) -> Shader os f s ()
drawDepthStencil :: forall a os f s d st. (DepthRenderable d, StencilRenderable st) => FragmentStream (a, FragDepth) -> (s -> (Blending, Image d, Image st, DepthStencilOption)) -> (a -> DrawColors os s ()) -> Shader os f s ()

makeFBOKeys :: IO [FBOKey] -> IO (Maybe FBOKey) -> IO (Maybe FBOKey) -> IO FBOKeys
makeFBOKeys c d s = do c' <- c
                       d' <- d
                       s' <- s
                       return $ FBOKeys c' d' s'

draw fs sf m = Shader $ tellDrawcalls fs $ \c -> let (sh,g,ioc) = runDrawColors (m c) in (sh, g, f ioc)
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        b = sf s  
                    in (Just (makeFBOKeys fbokeyio (return Nothing) (return Nothing), fboio)
                       , io >> glDisable gl_DEPTH_TEST >> glDisable gl_STENCIL_TEST >> setGlBlend b)

drawDepth fs sf m = Shader $ tellDrawcalls fs $ \(c,d) -> let (sh,g,ioc) = runDrawColors (m c) in (sh >> setDepth d, g, f ioc)
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        (b, di, o) = sf s  
                    in (Just (makeFBOKeys fbokeyio (Just <$> getImageFBOKey di) (return Nothing)
                             , fboio >> getImageBinding di gl_DEPTH_ATTACHMENT)
                       , io >> glDisable gl_STENCIL_TEST >> setGlBlend b >> setGlDepthOptions o)

drawStencil fs sf m = Shader $ tellDrawcalls fs $ \c -> let (sh,g,ioc) = runDrawColors (m c) in (sh, g, f ioc )
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        (b, si, o) = sf s  
                    in (Just (makeFBOKeys fbokeyio (return Nothing) (Just <$> getImageFBOKey si)
                             , fboio >> getImageBinding si gl_STENCIL_ATTACHMENT)
                       , io >> glDisable gl_DEPTH_TEST >> setGlBlend b >> setGlStencilOptions o OpZero OpZero)

drawDepthStencil fs sf m = Shader $ tellDrawcalls fs $ \(c,d) -> let (sh,g,ioc) = runDrawColors (m c) in (sh >> setDepth d, g, f ioc )
    where f ioc s = let (fbokeyio, fboio, io) = ioc s
                        (b, di, si, o) = sf s  
                    in (Just (makeFBOKeys fbokeyio (Just <$> getImageFBOKey di) (Just <$> getImageFBOKey si)
                             , fboio >> getCombinedBinding di si)
                       , io >> setGlBlend b >> setGlDepthStencilOptions o)
          getCombinedBinding di si | imageEquals di si = getImageBinding di gl_DEPTH_STENCIL_ATTACHMENT
                                   | otherwise = getImageBinding di gl_DEPTH_ATTACHMENT >> getImageBinding si gl_STENCIL_ATTACHMENT


drawContextColor :: forall os s c ds. ContextColorFormat c => FragmentStream (FragColor c) -> (s -> ContextColorOption c) -> Shader os (ContextFormat c ds) s ()
drawContextDepth :: forall os s c ds. DepthRenderable ds => FragmentStream FragDepth -> (s -> DepthOption) -> Shader os (ContextFormat c ds) s ()
drawContextColorDepth :: forall os s c ds. (ContextColorFormat c, DepthRenderable ds) => FragmentStream (FragColor c, FragDepth) -> (s -> (ContextColorOption c, DepthOption)) -> Shader os (ContextFormat c ds) s ()
drawContextStencil :: forall os s c ds. StencilRenderable ds => FragmentStream () -> (s -> StencilOptions) -> Shader os (ContextFormat c ds) s ()
drawContextColorStencil :: forall os s c ds. (ContextColorFormat c, StencilRenderable ds) => FragmentStream (FragColor c) -> (s -> (ContextColorOption c, StencilOptions)) -> Shader os (ContextFormat c ds) s ()
drawContextDepthStencil :: forall os s c ds. (DepthRenderable ds, StencilRenderable ds) => FragmentStream FragDepth -> (s -> DepthStencilOption) -> Shader os (ContextFormat c ds) s ()
drawContextColorDepthStencil :: forall os s c ds. (ContextColorFormat c, DepthRenderable ds, StencilRenderable ds) => FragmentStream (FragColor c, FragDepth) -> (s -> (ContextColorOption c, DepthStencilOption)) -> Shader os (ContextFormat c ds) s ()

drawContextColor fs sf = Shader $ tellDrawcalls fs $ \ a -> make3 (setColor cf 0 a) io 
                            where io s = (Nothing, glDisable gl_DEPTH_TEST >> glDisable gl_STENCIL_TEST >> setGlContextColorOptions cf (sf s))
                                  cf = undefined :: c

drawContextDepth fs sf = Shader $ tellDrawcalls fs $ \ a-> (setDepth a, return(), io)
                            where io s = (Nothing, glDisable gl_STENCIL_TEST >> setGlDepthOptions (sf s))

drawContextColorDepth fs sf = Shader $ tellDrawcalls fs $ \(c,d) -> let (s, g) = setColor cf 0 c in (s >> setDepth d, g, io)
                                where io s = let (cop, dop) = sf s in (Nothing, glDisable gl_STENCIL_TEST >> setGlContextColorOptions cf cop >> setGlDepthOptions dop)
                                      cf = undefined :: c
    
drawContextStencil fs sf = Shader $ tellDrawcalls fs $ const (return (), return (), io)
                                where io s = (Nothing, glDisable gl_DEPTH_TEST >> setGlStencilOptions (sf s) OpZero OpZero)
    
drawContextColorStencil fs sf = Shader $ tellDrawcalls fs $ \ a -> make3 (setColor cf 0 a) io
                                    where io s = let (cop, dop) = sf s in (Nothing, glDisable gl_DEPTH_TEST >> setGlContextColorOptions cf cop >> setGlStencilOptions dop OpZero OpZero)
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
                  in (do xs <- mapM unS $ fromColor ct c
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
            NoBlending -> glDisable gl_BLEND
            LogicOp _ -> glDisable gl_BLEND
            _ -> glEnable gl_BLEND

setGlBlend :: Blending -> IO ()
setGlBlend NoBlending = return ()
setGlBlend (BlendRgbAlpha (e, ea) (BlendingFactors sf df, BlendingFactors sfa dfa) (V4 r g b a)) = do
                            glBlendEquationSeparate (getGlBlendEquation e) (getGlBlendEquation ea) 
                            glBlendFuncSeparate (getGlBlendFunc sf) (getGlBlendFunc df) (getGlBlendFunc sfa) (getGlBlendFunc dfa)  
                            glBlendColor (toGlFloat r) (toGlFloat g) (toGlFloat b) (toGlFloat a)
setGlBlend (LogicOp op) = glEnable gl_COLOR_LOGIC_OP >> glLogicOp (getGlLogicOp op) 

toGlFloat :: Float -> GLfloat
toGlFloat = fromRational . toRational
       
setGlDepthOptions :: DepthOption -> IO ()
setGlDepthOptions (DepthOption df dm) = do glEnable gl_DEPTH_TEST
                                           glDepthFunc (getGlCompFunc df)
                                           glDepthMask $ fromBool dm 

setGlStencilOptions :: FrontBack StencilOption -> StencilOp -> StencilOp -> IO ()
setGlStencilOptions (FrontBack (StencilOption ft fr ff fp frm fwm) (StencilOption bt br bf bp brm bwm)) fdf bdf = do
    glEnable gl_STENCIL_TEST
    glStencilFuncSeparate gl_FRONT (getGlCompFunc ft) (fromIntegral fr) (fromIntegral frm) 
    glStencilOpSeparate gl_FRONT (getGlStencilOp ff) (getGlStencilOp fdf) (getGlStencilOp fp) 
    glStencilMaskSeparate gl_FRONT (fromIntegral fwm)
    glStencilFuncSeparate gl_BACK (getGlCompFunc bt) (fromIntegral br) (fromIntegral brm) 
    glStencilOpSeparate gl_BACK (getGlStencilOp bf) (getGlStencilOp bdf) (getGlStencilOp bp) 
    glStencilMaskSeparate gl_BACK (fromIntegral bwm)
    
setGlDepthStencilOptions :: DepthStencilOption -> IO ()
setGlDepthStencilOptions (DepthStencilOption sop dop (FrontBack fdf bdf)) = do
    setGlDepthOptions dop
    setGlStencilOptions sop fdf bdf

data ContextColorOption f = ContextColorOption Blending (ColorMask f)

data DepthOption = DepthOption DepthFunction DepthMask
type StencilOptions = FrontBack StencilOption 
data StencilOption = StencilOption {
                            stencilTest :: ComparisonFunction,
                            stencilReference :: Int,
                            opWhenStencilFail :: StencilOp,
                            opWhenStencilPass :: StencilOp,
                            stencilReadBitMask :: Word,
                            stencilWriteBitMask :: Word
                            }
data DepthStencilOption = DepthStencilOption {
                            dsStencilOptions :: StencilOptions, 
                            dsDepthOption :: DepthOption ,
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
                      
type ConstantColor = V4 Float

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

getGlBlendEquation :: BlendEquation -> GLenum
getGlBlendEquation FuncAdd = gl_FUNC_ADD
getGlBlendEquation FuncSubtract = gl_FUNC_SUBTRACT
getGlBlendEquation FuncReverseSubtract = gl_FUNC_REVERSE_SUBTRACT
getGlBlendEquation Min = gl_MIN
getGlBlendEquation Max = gl_MAX

getGlBlendFunc :: BlendingFactor -> GLenum
getGlBlendFunc Zero = gl_ZERO
getGlBlendFunc One = gl_ONE
getGlBlendFunc SrcColor = gl_SRC_COLOR
getGlBlendFunc OneMinusSrcColor = gl_ONE_MINUS_SRC_COLOR
getGlBlendFunc DstColor = gl_DST_COLOR
getGlBlendFunc OneMinusDstColor = gl_ONE_MINUS_DST_COLOR
getGlBlendFunc SrcAlpha = gl_SRC_ALPHA
getGlBlendFunc OneMinusSrcAlpha = gl_ONE_MINUS_SRC_ALPHA
getGlBlendFunc DstAlpha = gl_DST_ALPHA
getGlBlendFunc OneMinusDstAlpha = gl_ONE_MINUS_DST_ALPHA
getGlBlendFunc ConstantColor = gl_CONSTANT_COLOR
getGlBlendFunc OneMinusConstantColor = gl_ONE_MINUS_CONSTANT_COLOR
getGlBlendFunc ConstantAlpha = gl_CONSTANT_ALPHA
getGlBlendFunc OneMinusConstantAlpha = gl_ONE_MINUS_CONSTANT_ALPHA
getGlBlendFunc SrcAlphaSaturate = gl_SRC_ALPHA_SATURATE

getGlLogicOp :: LogicOp -> GLenum
getGlLogicOp Clear = gl_CLEAR
getGlLogicOp And = gl_AND
getGlLogicOp AndReverse = gl_AND_REVERSE
getGlLogicOp Copy = gl_COPY
getGlLogicOp AndInverted = gl_AND_INVERTED
getGlLogicOp Noop = gl_NOOP
getGlLogicOp Xor = gl_XOR
getGlLogicOp Or = gl_OR
getGlLogicOp Nor = gl_NOR
getGlLogicOp Equiv = gl_EQUIV
getGlLogicOp Invert = gl_INVERT
getGlLogicOp OrReverse = gl_OR_REVERSE
getGlLogicOp CopyInverted = gl_COPY_INVERTED
getGlLogicOp OrInverted = gl_OR_INVERTED
getGlLogicOp Nand = gl_NAND
getGlLogicOp Set = gl_SET

getGlStencilOp :: StencilOp -> GLenum
getGlStencilOp OpZero = gl_ZERO
getGlStencilOp OpKeep = gl_KEEP
getGlStencilOp OpReplace = gl_REPLACE
getGlStencilOp OpIncr = gl_INCR
getGlStencilOp OpIncrWrap = gl_INCR_WRAP
getGlStencilOp OpDecr = gl_DECR
getGlStencilOp OpDecrWrap = gl_DECR_WRAP
getGlStencilOp OpInvert = gl_INVERT
   
   --------------------