{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, TupleSections #-}
module Graphics.GPipe.FrameBuffer where

import Graphics.GPipe.Expr
import Graphics.GPipe.FragmentStream
import Graphics.GPipe.Format
import Graphics.GPipe.Shader
import Graphics.GPipe.PrimitiveStream
import Graphics.GPipe.Compiler
import Graphics.GPipe.Texture
import Control.Applicative (Applicative)
import Control.Monad (when)
import Control.Monad.Trans.Writer.Lazy
import Data.List (intercalate)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

newtype DrawColors os s a = DrawColors (StateT Int (Writer [Int -> (ExprM (), GlobDeclM (), s -> IO ())]) a) deriving (Functor, Applicative, Monad)

runDrawColors :: DrawColors os s a -> (ExprM (), GlobDeclM (), s -> IO ())
runDrawColors (DrawColors m) = foldl sf (return (), return (), const $ return ()) $ zip ws [0..]
    where sf (ms, mg, mio) (f, n) = let (sh, g, io) = f n in (ms >> sh, mg >> g, \s -> mio s >> io s)
          ((_,numColors),ws) = runWriter (runStateT m 0)
drawColor :: forall c s os. ColorRenderable c => FragColor c -> (s -> (Image c, ColorOption c)) -> DrawColors os s ()
drawColor c sf = DrawColors $ do n <- get
                                 put $ n+1
                                 lift $ tell [\ix -> make3  (setColor (undefined :: c) ix c) $ \s -> let (i, o) = sf s in getImageBinding i (glColorAttachment + n) >> glSetOptions o]

glColorAttachment = 0
glDepthAttachment = 1 
glStencilAttachment = 2
glDepthStencilAttachment = 3 

draw :: forall a os f s. FragmentStream a -> (a -> DrawColors os s ()) -> Shader os f s ()
drawDepth :: forall a os f s d. DepthRenderable d => FragmentStream (a, FragDepth) -> (s -> (Image d, DepthOption)) -> (a -> DrawColors os s ()) -> Shader os f s ()
drawStencil :: forall a os f s st. StencilRenderable st => FragmentStream a -> (s -> (Image st, StencilOption)) -> (a -> DrawColors os s ()) -> Shader os f s ()
drawDepthStencil :: forall a os f s d st. (DepthRenderable d, StencilRenderable st) => FragmentStream (a, FragDepth) -> (s -> (Image d, Image st, DepthStencilOption)) -> (a -> DrawColors os s ()) -> Shader os f s ()

draw fs m = Shader $ tellDrawcalls fs (runDrawColors . m)   

drawDepth fs sf m = Shader $ tellDrawcalls fs $ \(c,d) -> let (s,g,ioc) = runDrawColors (m c) in (s >> setDepth d, g, \s -> ioc s >> iod s)
    where iod s = let (i, o) = sf s in getImageBinding i glDepthAttachment >> glSetOptions o

drawStencil fs sf m = Shader $ tellDrawcalls fs $ \c -> let (s,g,ioc) = runDrawColors (m c) in (s, g, \s -> ioc s >> ios s)
    where ios s = let (i, o) = sf s in getImageBinding i glStencilAttachment >> glSetOptions o

drawDepthStencil fs sf m = Shader $ tellDrawcalls fs $ \(c,d) -> let (s,g,ioc) = runDrawColors (m c) in (s >> setDepth d, g, \s -> ioc s >> iods s)
    where iods s = let (di, si, o) = sf s in getCombinedBinding di si >> glSetOptions o
          getCombinedBinding di si | getImageName di == getImageName si = getImageBinding di glDepthStencilAttachment
                                   | otherwise = getImageBinding di glDepthAttachment >> getImageBinding si glStencilAttachment

drawContextColor :: forall os s c ds. ContextColorFormat c => FragmentStream (FragColor c) -> (s -> ColorOption c) -> Shader os (ContextFormat c ds) s ()
drawContextDepth :: forall os s c ds. DepthRenderable ds => FragmentStream FragDepth -> (s -> DepthOption) -> Shader os (ContextFormat c ds) s ()
drawContextColorDepth :: forall os s c ds. (ContextColorFormat c, DepthRenderable ds) => FragmentStream (FragColor c, FragDepth) -> (s -> (ColorOption c, DepthOption)) -> Shader os (ContextFormat c ds) s ()
drawContextStencil :: forall os s c ds. StencilRenderable ds => FragmentStream () -> (s -> StencilOption) -> Shader os (ContextFormat c ds) s ()
drawContextColorStencil :: forall os s c ds. (ContextColorFormat c, StencilRenderable ds) => FragmentStream (FragColor c) -> (s -> (ColorOption c, StencilOption)) -> Shader os (ContextFormat c ds) s ()
drawContextDepthStencil :: forall os s c ds. (DepthRenderable ds, StencilRenderable ds) => FragmentStream FragDepth -> (s -> DepthStencilOption) -> Shader os (ContextFormat c ds) s ()
drawContextColorDepthStencil :: forall os s c ds. (ContextColorFormat c, DepthRenderable ds, StencilRenderable ds) => FragmentStream (FragColor c, FragDepth) -> (s -> (ColorOption c, DepthStencilOption)) -> Shader os (ContextFormat c ds) s ()

drawContextColor fs sf = Shader $ tellDrawcalls fs $ \a-> make3 (setColor (undefined :: c) 0 a) io 
                            where io s = glSetOptions (sf s)

drawContextDepth fs sf = Shader $ tellDrawcalls fs $ \a-> (setDepth a, return(), io)
                            where io s = glSetOptions (sf s)

drawContextColorDepth fs sf = Shader $ tellDrawcalls fs $ \(c,d) -> let (s, g) = setColor (undefined :: c) 0 c in (s >> setDepth d, g, io)
                                where io s = glSetOptions (sf s)
    
drawContextStencil fs sf = Shader $ tellDrawcalls fs $ const (return (), return (), io)
                                where io s = glSetOptions (sf s)
    
drawContextColorStencil fs sf = Shader $ tellDrawcalls fs $ \a-> make3 (setColor (undefined :: c) 0 a) io
                                    where io s = glSetOptions (sf s)
    
drawContextDepthStencil fs sf = Shader $ tellDrawcalls fs $ \a-> (setDepth a, return(), io)
                                    where io s = glSetOptions (sf s)
        
drawContextColorDepthStencil fs sf = Shader $ tellDrawcalls fs $ \(c,d) -> let (s, g) = setColor (undefined :: c) 0 c in (s >> setDepth d, g, io)
                                        where io s = glSetOptions (sf s)
            
tellDrawcalls :: FragmentStream a -> (a -> (ExprM (), GlobDeclM (), s -> IO ())) -> ShaderM s ()
tellDrawcalls (FragmentStream xs) f = do  
                               let g (x, fd) = tellDrawcall $ makeDrawcall (f x) fd
                               mapM_ g xs

makeDrawcall :: (ExprM (), GlobDeclM (), s -> IO ()) -> FragmentStreamData -> IO (Drawcall s)
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
                      do tellGlobal typeS
                         tellGlobal " "
                         tellGlobalLn name)

setDepth :: FFloat -> ExprM ()
setDepth (S d) = do d' <- d
                    when (d' /= "gl_FragDepth") $
                        tellAssignment' "gl_FragDepth" d'  

make3 :: (t, t1) -> t2 -> (t, t1, t2)
make3 (a,b) c = (a,b,c)

glSetOptions _ = putStrLn "glSetOptions o"

type FragColor c = Color c (S F (ColorElement c))
type FragDepth = FFloat


data ColorOption f = ColorOption (Blending f) (ColorMask f)

data DepthOption = DepthOption DepthFunction DepthMask
type StencilOpWhenStencilFail = FrontBack StencilOp
type StencilOpWhenDepthFail = FrontBack StencilOp
type StencilOpWhenPass = FrontBack StencilOp
data StencilOption = StencilOption (FrontBack StencilTest) StencilOpWhenStencilFail StencilOpWhenPass
data DepthStencilOption = DepthStencilOption StencilOption DepthOption StencilOpWhenDepthFail

data FrontBack a = FrontBack { front :: a, back :: a }

-- | 'True' for each color component that should be written to the 'ShaderBuffer'.
type ColorMask f = Color f Bool
-- | 'True' if the depth component should be written to the 'ShaderBuffer'.
type DepthMask = Bool
-- | The function used to compare the fragment's depth and the depth buffers depth with.
type DepthFunction = ComparisonFunction

-- | Sets how the painted colors are blended with the 'ShaderBuffer's previous value.
data Blending f = NoBlending -- ^ The painted fragment completely overwrites the previous value.
              | Blend (FrontBack BlendEquation)
                      (FrontBack (BlendingFactor, BlendingFactor))
                      (Color f Float) -- ^ Use blending equations to combine the fragment with the previous value.
              | BlendLogicOp LogicOp -- ^ Use a 'LogicOp' to combine the fragment with the previous value.

-- | Sets a test that should be performed on the stencil value.
data StencilTest = StencilTest {
                       stencilComparision :: ComparisonFunction, -- ^ The function used to compare the @stencilReference@ and the stencil buffers value with.
                       stencilReference :: Int,--Int32, -- ^ The value to compare with the stencil buffer's value. 
                       stencilMask :: Int --Word32 -- ^ A bit mask with ones in each position that should be compared and written to the stencil buffer.
                   } 

data BlendEquation =
     FuncAdd
   | FuncSubtract
   | FuncReverseSubtract
   | Min
   | Max
   | LogicOp

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

data ComparisonFunction =
     Never
   | Less
   | Equal
   | Lequal
   | Greater
   | Notequal
   | Gequal
   | Always
   deriving ( Eq, Ord, Show )
   
   --------------------