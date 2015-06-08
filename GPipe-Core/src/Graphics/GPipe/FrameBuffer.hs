{-# LANGUAGE Arrows, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Graphics.GPipe.FrameBuffer where

import Graphics.GPipe.Shader
import Graphics.GPipe.FragmentStream
import Graphics.GPipe.Format
import Graphics.GPipe.Frame
import Graphics.GPipe.FrameCompiler
import Control.Monad.Trans.Writer.Lazy (tell)
import Control.Category (Category)
import Control.Arrow (Arrow)

newtype DrawColors os a b = DrawColors (IntFrame a b) deriving (Category, Arrow)



drawColor :: forall c os. ColorRenderable c => DrawColors os (Image c, ColorOption c, FragColor c) ()
drawColor = undefined

drawColorsDepth :: forall a os f d. DepthRenderable d => DrawColors os a () -> Frame os f (Image d, DepthOption, FragmentStream (a, FragDepth)) ()
drawColorsStencil :: forall a os f s. StencilRenderable s => DrawColors os a () -> Frame os f (Image s, StencilOption, FragmentStream a) ()
drawColorsDepthStencil :: forall a os f d s. (DepthRenderable d, StencilRenderable s) => DrawColors os a () -> Frame os f (Image d, Image s, DepthStencilOption, FragmentStream (a, FragDepth)) ()

drawColorsDepth = undefined
drawColorsStencil = undefined
drawColorsDepthStencil = undefined

drawContextDepth :: forall c os ds. DepthRenderable ds => Frame os (ContextFormat c ds) (DepthOption, FragmentStream FragDepth) ()
drawContextColorDepth :: forall c os ds. (ColorRenderable c, DepthRenderable ds) => Frame os (ContextFormat c ds) (ColorOption c, DepthOption, FragmentStream (FragColor c, FragDepth)) ()
drawContextStencil :: forall c os ds. StencilRenderable ds => Frame os (ContextFormat c ds) (StencilOption, FragmentStream ()) ()
drawContextDepthStencil :: forall c os ds. (DepthRenderable ds, StencilRenderable ds) => Frame os (ContextFormat c ds) (DepthStencilOption, FragmentStream FragDepth) ()
drawContextColorDepthStencil :: forall c os ds. (ColorRenderable c, DepthRenderable ds, StencilRenderable ds) => Frame os (ContextFormat c ds) (ColorOption c, DepthStencilOption, FragmentStream (FragColor c, FragDepth)) ()

drawContextDepth = undefined
drawContextColorDepth = undefined
drawContextStencil = undefined 
drawContextDepthStencil = undefined
drawContextColorDepthStencil = undefined 

drawContextColor :: forall c os ds. ColorRenderable c => Frame os (ContextFormat c ds) (ColorOption c, FragmentStream (FragColor c)) ()
drawContextColor = dynStatIn md f 
    where
        md = dynInStatOut $ do n <- getName
                               dc <- getDrawcall 
                               return ((n, dc) ,\co -> doForName n $ \ _ _ _ -> glBindOutputAndSetColorOptions co)
        f ((n,dc),FragmentStream xs) = mapM_ (g n dc) xs
        g n dc (c, fd) = tell [DrawCall n (orderth dc ++ " drawcall") (setColor (undefined :: c) "gl_FragColor" c) fd]                                       


dynStatIn :: IntFrame a t -> ((t, t1) -> StaticFrame ()) -> Frame os f (a, t1) ()
dynStatIn  md f = proc (co, fs) -> do ndc <- IntFrame md -< co  
                                      IntFrame (statIn f) -< (ndc, fs)


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

glBindOutputAndSetColorOptions = undefined

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