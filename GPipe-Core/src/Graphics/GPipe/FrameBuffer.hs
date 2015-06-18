{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Graphics.GPipe.FrameBuffer where

import Graphics.GPipe.Shader
import Graphics.GPipe.FragmentStream
import Graphics.GPipe.Format
import Graphics.GPipe.Frame
import Graphics.GPipe.VertexStream
import Graphics.GPipe.FrameCompiler
import Data.IntMap.Lazy (insert)
import Control.Applicative (Applicative)

newtype DrawColors os s a = DrawColors (FrameM s a) deriving (Functor, Applicative, Monad)

drawColor :: forall c s os. ColorRenderable c => FragColor c -> (s -> (Image c, ColorOption c)) -> DrawColors os s ()
drawColor = undefined

draw :: forall a os f s. FragmentStream a -> (a -> DrawColors os s ()) -> Frame os f s ()
drawDepth :: forall a os f s d. DepthRenderable d => FragmentStream (a, FragDepth) -> (s -> (Image d, DepthOption)) -> (a -> DrawColors os s ()) -> Frame os f s ()
drawStencil :: forall a os f s st. StencilRenderable st => FragmentStream a -> (s -> (Image st, StencilOption)) -> (a -> DrawColors os s ()) -> Frame os f s ()
drawDepthStencil :: forall a os f s d st. (DepthRenderable d, StencilRenderable st) => FragmentStream (a, FragDepth) -> (s -> (Image d, Image st, DepthStencilOption)) -> (a -> DrawColors os s ()) -> Frame os f s ()

draw = undefined
drawDepth = undefined
drawStencil = undefined
drawDepthStencil = undefined

drawContextDepth :: forall os s c ds. DepthRenderable ds => FragmentStream FragDepth -> (s -> DepthOption) -> Frame os (ContextFormat c ds) s ()
drawContextColorDepth :: forall os s c ds. (ColorRenderable c, DepthRenderable ds) => FragmentStream (FragColor c, FragDepth) -> (s -> (ColorOption c, DepthOption)) -> Frame os (ContextFormat c ds) s ()
drawContextStencil :: forall os s c ds. StencilRenderable ds => FragmentStream () -> (s -> StencilOption) -> Frame os (ContextFormat c ds) s ()
drawContextDepthStencil :: forall os s c ds. (DepthRenderable ds, StencilRenderable ds) => FragmentStream FragDepth -> (s -> DepthStencilOption) -> Frame os (ContextFormat c ds) s ()
drawContextColorDepthStencil :: forall os s c ds. (ColorRenderable c, DepthRenderable ds, StencilRenderable ds) => FragmentStream (FragColor c, FragDepth) -> (s -> (ColorOption c, DepthStencilOption)) -> Frame os (ContextFormat c ds) s ()

drawContextDepth = undefined
drawContextColorDepth = undefined
drawContextStencil = undefined 
drawContextDepthStencil = undefined
drawContextColorDepthStencil = undefined 

drawContextColor :: forall os s c ds. ColorRenderable c => FragmentStream (FragColor c) -> (s -> ColorOption c) -> Frame os (ContextFormat c ds) s ()
drawContextColor (FragmentStream xs) sf = Frame $ do 
                               n <- getName
                               dc <- getDrawcall
                               let f (c, fd) = tellDrawcall $ makeDrawcall n (orderth dc ++ " drawcall") (setColor (undefined :: c) "gl_FragColor" c) fd
                               mapM_ f xs
                               doForDraw n $ \s -> glBindOutputAndSetColorOptions (sf s)

doForDraw :: Int -> (s -> IO ()) -> FrameM s ()
doForDraw n io = modifyRenderIO (\s -> s { drawToRenderIOs = insert n io (drawToRenderIOs s) } )

makeDrawcall :: Int -> String -> ShaderM () -> FragmentStreamData -> IO Drawcall
makeDrawcall n err sh (FragmentStreamData side shaderpos (PrimitiveStreamData dcN) keep) =
       do (fsource, funis, fsamps, _, prevDecls, prevS) <- runShaderM (return ()) (discard keep >> sh)
          (vsource, vunis, vsamps, vinps, _, _) <- runShaderM prevDecls (prevS >> shaderpos)
          let unis = orderedUnion funis vunis
              samps = orderedUnion fsamps vsamps
          return $ Drawcall err n dcN vsource fsource vinps unis samps

orderedUnion :: Ord a => [a] -> [a] -> [a]
orderedUnion xxs@(x:xs) yys@(y:ys) | x == y    = x : orderedUnion xs ys 
                                   | x < y     = x : orderedUnion xs yys
                                   | otherwise = y : orderedUnion xxs ys
orderedUnion xs [] = xs
orderedUnion [] ys = ys


setColor :: forall c. ColorFormat c => c -> String -> FragColor c -> ShaderM ()
setColor ct name c = let (S x, S y, S z, S w) = fromColor ct c (S $ return "1")
                  in do 
                    x' <- x
                    y' <- y
                    z' <- z
                    w' <- w
                    tellAssignment' name ("vec4(" ++ x' ++ ',' : y' ++ ',' : z' ++ ',' : w' ++")")  

orderth :: Int -> String
orderth x = let s = show x 
            in s ++ case init s of
                        xs@(_:_) | last xs == '1' -> "th" -- 11th through 19th"
                        _ -> case last s of
                                '1' -> "st"  
                                '2' -> "nd"  
                                '3' -> "rd"
                                _ -> "th"  

glBindOutputAndSetColorOptions _ = putStrLn "glBindOutputAndSetColorOptions x"

type FragColor c = Color c (S F (ColorElement c))
type FragDepth = FFloat


data ColorOption f = ColorOption (Blending f) (ColorMask f)

data Image f = Image

data DepthOption = DepthOption DepthFunction DepthMask
type StencilOpWhenStencilFail = FrontBack StencilOp
type StencilOpWhenDepthFail = FrontBack StencilOp
type StencilOpWhenPass = FrontBack StencilOp
data StencilOption = StencilOption (FrontBack StencilTest) StencilOpWhenStencilFail StencilOpWhenPass
data DepthStencilOption = DepthStencilOption StencilOption DepthOption StencilOpWhenDepthFail

data FrontBack a = FrontBack { front :: a, back :: a }

-- | 'True' for each color component that should be written to the 'FrameBuffer'.
type ColorMask f = Color f Bool
-- | 'True' if the depth component should be written to the 'FrameBuffer'.
type DepthMask = Bool
-- | The function used to compare the fragment's depth and the depth buffers depth with.
type DepthFunction = ComparisonFunction

-- | Sets how the painted colors are blended with the 'FrameBuffer's previous value.
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

-- | Sets the operations that should be performed on the 'FrameBuffer's stencil value
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