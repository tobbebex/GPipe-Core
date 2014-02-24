{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving, FlexibleInstances, ImpredicativeTypes, GADTs, EmptyDataDecls #-}

module Graphics.GPipe.Frame (
    Frame(),
)where

import Graphics.GPipe.Format

import Graphics.GPipe.Texture
import Graphics.GPipe.Stream
import Graphics.GPipe.Shader
import Graphics.GPipe.Context
import Control.Applicative (Applicative)
import Control.Monad.Trans.State 


data FrameState = FrameState


newtype Frame fr os f a = Frame (State FrameState a) deriving (Functor, Applicative, Monad)

runFrame :: (forall fr. Frame fr os f a) -> ContextT os f m a
runFrame = undefined 

type FragColor c = Color c (S F (ColorElement c))
type FragDepth = FFloat



drawContextColor :: ColorRenderable c => ColorOption c -> Stream Fragments fr (FragColor c) -> Frame fr os (ContextFormat c ds) ()
drawContextColor = undefined 
drawContextColorDepth :: (ColorRenderable c, DepthRenderable ds) => ColorOption c -> DepthOption -> Stream Fragments fr (FragColor c, FragDepth) -> Frame fr os (ContextFormat c ds) ()
drawContextColorDepth = undefined 
drawContextColorStencil :: (ColorRenderable c, StencilRenderable ds) => ColorOption c -> StencilOption -> Stream Fragments fr (FragColor c) -> Frame fr os (ContextFormat c ds) ()
drawContextColorStencil = undefined
drawContextColorDepthStencil :: (ColorRenderable c, DepthRenderable ds, StencilRenderable ds) => ColorOption c -> DepthStencilOption -> Stream Fragments fr (FragColor c, FragDepth) -> Frame fr os (ContextFormat c ds) ()
drawContextColorDepthStencil = undefined
drawContextDepth :: DepthRenderable ds => DepthOption -> Stream Fragments fr FragDepth -> Frame fr os (ContextFormat c ds) ()
drawContextDepth = undefined 
drawContextStencil :: StencilRenderable ds => StencilOption -> Stream Fragments fr () -> Frame fr os (ContextFormat c ds) ()
drawContextStencil = undefined
drawContextDepthStencil :: (DepthRenderable ds, StencilRenderable ds) => DepthStencilOption -> Stream Fragments fr FragDepth -> Frame fr os (ContextFormat c ds) ()
drawContextDepthStencil = undefined

drawColors :: [forall c. ColorRenderable c => (a -> (Color c FFloat), RenderTarget os c, ColorOption c)] -> Stream Fragments fr a -> Frame fr os f ()
drawColors xs = undefined
drawColorsDepth  :: DepthRenderable ds => [forall c. ColorRenderable c => (a -> (Color c FFloat), RenderTarget os c, ColorOption c)] -> DepthStencil os ds -> DepthOption -> Stream Fragments fr (a, FragDepth) -> Frame fr os f ()
drawColorsDepth xs = undefined
drawColorsStencil  :: StencilRenderable ds => [forall c. ColorRenderable c => (a -> (Color c FFloat), RenderTarget os c, ColorOption c)] -> DepthStencil os ds -> StencilOption -> Stream Fragments fr a -> Frame fr os f ()
drawColorsStencil xs = undefined
drawColorsDepthStencil  :: (DepthRenderable ds, StencilRenderable ds) => [forall c. ColorRenderable c => (a -> (Color c FFloat), RenderTarget os c, ColorOption c)] -> DepthStencil os ds -> DepthOption -> StencilOption -> Stream Fragments fr (a, FragDepth) -> Frame fr os f ()
drawColorsDepthStencil xs = undefined
drawDepth  :: DepthRenderable ds => DepthStencil os ds -> DepthOption -> Stream Fragments fr FragDepth -> Frame fr os f ()
drawDepth = undefined
drawStencil  :: StencilRenderable ds => DepthStencil os ds -> StencilOption -> Stream Fragments fr () -> Frame fr os f ()
drawStencil = undefined
drawDepthStencil  :: (DepthRenderable ds, StencilRenderable ds) => DepthStencil os ds -> DepthOption -> StencilOption -> Stream Fragments fr FragDepth -> Frame fr os f ()
drawDepthStencil = undefined

-- --.......................--

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
-- Private 

setupFboGl f = undefined
teardownFboGl f = undefined
clearFrameBufferGl c d s = undefined
 
    
     