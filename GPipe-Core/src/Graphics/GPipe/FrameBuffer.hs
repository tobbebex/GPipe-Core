{-# LANGUAGE Arrows, ScopedTypeVariables #-}
module Graphics.GPipe.FrameBuffer where

import Graphics.GPipe.ContextState
import Graphics.GPipe.Shader
import Graphics.GPipe.FragmentStream
import Graphics.GPipe.Format
import Graphics.GPipe.Frame
import Control.Monad.Trans.Writer.Lazy (tell)

drawContextColor :: forall c os ds. ColorRenderable c => Frame os (ContextFormat c ds) (ColorOption c, FragmentStream (FragColor c)) ()
drawContextColor = proc (co, fs) -> do ndc <- IntFrame md -< co  
                                       IntFrame (statIn f) -< (ndc, fs)
    where
        md = dynInStatOut $ do n <- getName
                               dc <- getDrawcall 
                               return ((n, dc) ,\co -> doForName n $ \ _ _ _ -> glBindOutputAndSetColorOptions co)
        f ((n,dc),FragmentStream xs) = mapM_ (g n dc) xs
        g n dc (c, fd) = 
            let (S x, S y, S z, S w) = fromColor (undefined :: c) c (S $ return "1")
                m =  do x' <- x
                        y' <- y
                        z' <- z
                        w' <- w                       
                        return ("vec4(" ++ x' ++ ',' : y' ++ ',' : z' ++ ',' : w' ++")")  
            in tell [DrawCall n (orderth dc ++ " drawcall") m fd]                                       

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

data DepthOption = DepthOption DepthFunction DepthMask
data StencilOption = StencilOption (FrontBack StencilTest) (FrontBack StencilOp) (FrontBack StencilOp)
data DepthStencilOption = DepthStencilOption DepthOption StencilOption (FrontBack StencilOp)    

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