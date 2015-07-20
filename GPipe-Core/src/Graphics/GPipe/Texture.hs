{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, AllowAmbiguousTypes, EmptyDataDecls #-}
module Graphics.GPipe.Texture where

import Graphics.GPipe.Format
import Graphics.GPipe.Expr
import Graphics.GPipe.Context
import Graphics.GPipe.Shader
import Graphics.GPipe.Compiler
import Graphics.GPipe.Buffer
import Control.Monad.IO.Class (MonadIO)
import Data.IntMap.Lazy (insert)
import Data.Functor ((<$>))
import Data.Vec

import Graphics.Rendering.OpenGL.Raw.Core33
import Graphics.Rendering.OpenGL.Raw.EXT.TextureFilterAnisotropic

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Control.Monad
import Foreign.C.Types

data Texture1D os a = Texture1D TexName (V1 Int)
data Texture1DArray os a = Texture1DArray TexName (V2 Int) 
data Texture2D os a = Texture2D TexName (V2 Int) | RenderBuffer2D TexName (V2 Int)
data Texture2DArray os a = Texture2DArray TexName (V1 Int)
data Texture3D os a = Texture3D TexName (Int, Int, Int)
data TextureCube os a = TextureCube TexName Int

newTexture1D :: (ColorSampleable c, Monad m) => c -> V1 Int -> ContextT os f m (Texture1D os c)
newTexture1DArray :: (ColorSampleable c, Monad m) => c -> V2 Int -> ContextT os f m (Texture1DArray os c)
newTexture2D :: (TextureFormat c, MonadIO m) => c -> V2 Int-> ContextT os f m (Texture2D os c)
newTexture2DArray :: (ColorSampleable c, Monad m) => c -> V3 Int -> ContextT os f m (Texture2DArray os c)
newTexture3D :: (ColorRenderable c, Monad m) => c -> V3 Int -> ContextT os f m (Texture3D os c)
newTextureCube :: (ColorSampleable c, Monad m) => c -> Int -> ContextT os f m (TextureCube os c)

setTexture1DLevels :: Texture1D os c -> Int -> ContextT os f m ()
setTexture1DArrayLevels :: Texture1DArray os c -> Int -> ContextT os f m ()
setTexture2DLevels :: Texture2D os c -> Int -> ContextT os f m ()
setTexture2DArrayLevels :: Texture2DArray os c -> Int -> ContextT os f m ()
setTexture3DLevels :: Texture3D os c -> Int -> ContextT os f m ()
setTextureCubeLevels :: TextureCube os c -> Int -> ContextT os f m ()

getTexture1DLevels :: Texture1D os c -> ContextT os f m Int 
getTexture1DArrayLevels :: Texture1DArray os c -> ContextT os f m Int 
getTexture2DLevels :: Texture2D os c -> ContextT os f m Int 
getTexture2DArrayLevels :: Texture2DArray os c -> ContextT os f m Int 
getTexture3DLevels :: Texture3D os c -> ContextT os f m Int 
getTextureCubeLevels :: TextureCube os c -> ContextT os f m Int 

maxTexture1DLevels :: Texture1D os c -> Int 
maxTexture1DArrayLevels :: Texture1DArray os c -> Int 
maxTexture2DLevels :: Texture2D os c -> Int 
maxTexture2DArrayLevels :: Texture2DArray os c -> Int 
maxTexture3DLevels :: Texture3D os c -> Int 
maxTextureCubeLevels :: TextureCube os c -> Int 

getTexture1DLevels = undefined 
getTexture1DArrayLevels = undefined 
getTexture2DLevels = undefined 
getTexture2DArrayLevels = undefined 
getTexture3DLevels = undefined 
getTextureCubeLevels = undefined 

maxTexture1DLevels = undefined 
maxTexture1DArrayLevels = undefined 
maxTexture2DLevels (Texture2D _ (V2 w h)) = 1 + truncate (logBase 2.0 (fromIntegral (max w h) :: Double))
maxTexture2DLevels (RenderBuffer2D _ _) = 1
maxTexture2DArrayLevels = undefined 
maxTexture3DLevels = undefined 
maxTextureCubeLevels = undefined 


newTexture1D = undefined  
newTexture1DArray = undefined
newTexture2D f s@(V2 w h) | getGlFormat f == gl_STENCIL_INDEX = do 
                                t <- makeRenderBuff
                                liftContextIO $ do
                                   undefined
                                   return $ RenderBuffer2D t s   
                              | otherwise = do
                                t <- makeTex
                                liftContextIO $ do
                                    useTexSync t gl_TEXTURE_2D
                                    glTexImage2D gl_TEXTURE_2D 0 (fromIntegral $ getGlInternalFormat f) (fromIntegral w) (fromIntegral h) 0 (getGlFormat f) gl_BYTE nullPtr
                                    return $ Texture2D t s   

newTexture2DArray = undefined
newTexture3D  = undefined
newTextureCube  = undefined


texture1DSize :: Texture1D os c -> Level -> V1 Int 
texture1DArraySize :: Texture1DArray os c -> Level -> V2 Int 
texture2DSize :: Texture2D os c -> Level -> V2 Int 
texture2DArraySize :: Texture2DArray os c -> Level -> V3 Int 
texture3DSize :: Texture3D os c -> Level -> V3 Int 
textureCubeSize :: TextureCube os c -> Level -> Int 

texture1DSize = undefined 
texture1DArraySize = undefined 
texture2DSize t@(Texture2D _ (V2 w h)) l | l < maxTexture2DLevels t = V2 (calcLevelSize w l) (calcLevelSize h l)
                                         | otherwise = V2 0 0
texture2DSize (RenderBuffer2D _ (V2 w h)) 0 = V2 (calcLevelSize w 0) (calcLevelSize h 0)
texture2DSize (RenderBuffer2D _ _) _ = V2 0 0

texture2DArraySize = undefined 
texture3DSize = undefined 
textureCubeSize = undefined 

calcLevelSize :: Int -> Int -> Int
calcLevelSize size0 level = max 1 (size0 `div` (2 ^ level))

data TexName = TexName { getTexName :: CUInt } 

makeTex :: MonadIO m => ContextT os f m TexName 
makeTex = do
    name <- liftContextIO $ fromIntegral <$> alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
    let tex = TexName name 
    addContextFinalizer tex $ with (fromIntegral name) (glDeleteTextures 1)
    return tex 

makeRenderBuff :: MonadIO m => ContextT os f m TexName 
makeRenderBuff = do
    name <- liftContextIO $ fromIntegral <$> alloca (\ptr -> glGenRenderbuffers 1 ptr >> peek ptr)
    let tex = TexName name 
    addContextFinalizer tex $ with (fromIntegral name) (glDeleteRenderbuffers 1)
    return tex 
    
useTex (TexName n) t bind = do glActiveTexture (gl_TEXTURE0 + fromIntegral bind)
                               glBindTexture t n
                                             
useTexSync tn t = do maxUnits <- alloca (\ptr -> glGetIntegerv gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS ptr >> peek ptr)  -- Use last for all sync actions, keeping 0.. for async drawcalls
                     useTex tn t (maxUnits-1)
                                 
setTexture1DLevels  = undefined
setTexture1DArrayLevels  = undefined
setTexture2DLevels = undefined
setTexture2DArrayLevels  = undefined
setTexture3DLevels  = undefined
setTextureCubeLevels  = undefined


type Level = Int
type Slice = Int
data CubeSide = CubePosX | CubeNegX | CubePosY | CubeNegY | CubePosZ | CubeNegZ

data Proxy t = Proxy

writeTexture1D :: (ColorElement c ~ BaseShaderFormat b) => Texture1D os c -> Level -> (V1 Int, V1 Int) -> ([HostFormat b], Proxy b) -> ContextT os f m ()
writeTexture1DArray :: (ColorElement c ~ BaseShaderFormat b) => Texture1DArray os c -> Level -> Slice -> (V1 Int, V1 Int) -> ([HostFormat b], Proxy b) -> ContextT os f m ()
writeTexture2D :: (ColorElement c ~ BaseShaderFormat b) => Texture2D os c -> Level -> (V2 Int, V2 Int) -> ([HostFormat b], Proxy b) -> ContextT os f m ()
writeTexture2DArray :: (ColorElement c ~ BaseShaderFormat b) => Texture2DArray os c -> Level -> Slice -> (V2 Int, V2 Int) -> ([HostFormat b], Proxy b) -> ContextT os f m ()
writeTexture3D :: (ColorElement c ~ BaseShaderFormat b) => Texture3D os c -> Level -> (V3 Int, V3 Int) -> ([HostFormat b], Proxy b) -> ContextT os f m ()
writeTextureCube :: (ColorElement c ~ BaseShaderFormat b) => TextureCube os c -> Level -> CubeSide -> (V2 Int, V2 Int) -> ([HostFormat b], Proxy b) -> ContextT os f m ()

writeTexture1D' :: (ColorElement c ~ BaseShaderFormat b) => Texture1D os c -> Level -> (V1 Int, V1 Int) -> (Buffer os a, a -> b, Int) -> ContextT os f m ()
writeTexture1DArray' :: (ColorElement c ~ BaseShaderFormat b) => Texture1DArray os c -> Level -> Slice -> (V1 Int, V1 Int) -> (Buffer os a, a -> b, Int) -> ContextT os f m ()
writeTexture2D' :: (ColorElement c ~ BaseShaderFormat b) => Texture2D os c -> Level -> (V2 Int, V2 Int) -> (Buffer os a, a -> b, Int) -> ContextT os f m ()
writeTexture2DArray' :: (ColorElement c ~ BaseShaderFormat b) => Texture2DArray os c -> Level -> Slice -> (V2 Int, V2 Int) -> (Buffer os a, a -> b, Int) -> ContextT os f m ()
writeTexture3D' :: (ColorElement c ~ BaseShaderFormat b) => Texture3D os c -> Level -> (V3 Int, V3 Int) -> (Buffer os a, a -> b, Int) -> ContextT os f m ()
writeTextureCube' :: (ColorElement c ~ BaseShaderFormat b) => TextureCube os c -> Level -> CubeSide -> (V2 Int, V2 Int) -> (Buffer os a, a -> b, Int) -> ContextT os f m ()


writeTexture1D = undefined
writeTexture1DArray = undefined
writeTexture2D = undefined
writeTexture2DArray = undefined
writeTexture3D = undefined
writeTextureCube = undefined

writeTexture1D' = undefined
writeTexture1DArray' = undefined
writeTexture2D' = undefined
writeTexture2DArray' = undefined
writeTexture3D' = undefined
writeTextureCube' = undefined

----------------------------------------------------------------------

-- TODO : Texture -> user
-- TODO : Texture -> Texture
-- TODO : Default -> Texture
-- TODO : Texture -> Default
-- TODO : Buffer -> Default
-- TODO : Texture -> Buffer
-- TODO : Buffer -> user

----------------------------------------------------------------------
-- Samplers

data Filter = Nearest | Linear  deriving (Eq, Enum)
data EdgeMode = Repeat | Mirror | ClampToEdge | ClampToBorder deriving (Eq, Enum)
type BorderColor c = Color c Float 

type Anisotropy = Float
noAnisotropy = 1.0 
type MinFilter = Filter
type MagFilter = Filter
type LodFilter = Filter
data SamplerMode dim c = SamplerMode MagFilter MinFilter LodFilter Anisotropy (dim EdgeMode) (BorderColor c)

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

getGlCompFunc Never = gl_NEVER
getGlCompFunc Less = gl_LESS
getGlCompFunc Equal = gl_EQUAL
getGlCompFunc Lequal = gl_LEQUAL
getGlCompFunc Greater = gl_GREATER
getGlCompFunc Notequal = gl_NOTEQUAL
getGlCompFunc Gequal = gl_GEQUAL
getGlCompFunc Always = gl_ALWAYS
   
newSampler1D :: ColorSampleable c => (s -> (Texture1D os c, SamplerMode V1 c)) -> Shader os f s (Sampler1D c)
newSampler1DArray :: ColorSampleable c => (s -> (Texture1DArray os c, SamplerMode V1 c)) -> Shader os f s (Sampler1DArray c)
newSampler2D :: ColorSampleable c => (s -> (Texture2D os c, SamplerMode V2 c)) -> Shader os f s (Sampler2D c)
newSampler3D :: ColorRenderable c => (s -> (Texture3D os c, SamplerMode V3 c)) -> Shader os f s (Sampler3D c)
newSamplerCube :: ColorSampleable c => (s -> (TextureCube os c, SamplerMode V2 c)) -> Shader os f s (SamplerCube c)

newSampler1DShadow :: DepthRenderable d => (s -> (Texture1D os d, SamplerMode V1 c, ComparisonFunction)) -> Shader os f s (Sampler1D Shadow)
newSampler1DArrayShadow :: DepthRenderable d => (s -> (Texture1DArray os d, SamplerMode V1 c, ComparisonFunction)) -> Shader os f s (Sampler1DArray Shadow)
newSampler2DShadow :: DepthRenderable d => (s -> (Texture2D os d, SamplerMode V2 c, ComparisonFunction)) -> Shader os f s (Sampler2D Shadow)
newSampler2DArray :: ColorSampleable c => (s -> (Texture2DArray os c, SamplerMode V2 c)) -> Shader os f s (Sampler2DArray c)
newSampler2DArrayShadow :: DepthRenderable d => (s -> (Texture2DArray os d, SamplerMode V2 c, ComparisonFunction)) -> Shader os f s (Sampler2DArray Shadow)
newSamplerCubeShadow :: DepthRenderable d => (s -> (TextureCube os d, SamplerMode V2 c, ComparisonFunction)) -> Shader os f s (SamplerCube Shadow)

newSampler1D = undefined
newSampler1DShadow = undefined
newSampler1DArray = undefined
newSampler1DArrayShadow = undefined
newSampler2D sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture2D tn _, sm) = sf s 
                                                    in  do useTex tn gl_TEXTURE_2D bind
                                                           setSamplerMode gl_TEXTURE_2D sm (\(V2 x y) -> (Just x, Just y, Nothing)) (\x -> return ())
                   return $ Sampler2D sampId

setSamplerMode t (SamplerMode magf minf lodf a e bc) f g = do
                                                       glTexParameteri t gl_TEXTURE_MIN_FILTER (fromIntegral glmin)
                                                       glTexParameteri t gl_TEXTURE_MAG_FILTER (fromIntegral glmag)
                                                       glTexParameterf t gl_TEXTURE_MAX_ANISOTROPY_EXT (fromRational $ toRational a)
                                                       glwrap gl_TEXTURE_WRAP_S se
                                                       glwrap gl_TEXTURE_WRAP_T te
                                                       glwrap gl_TEXTURE_WRAP_R re
                                                       when (se == Just ClampToBorder ||
                                                           te == Just ClampToBorder ||
                                                           re == Just ClampToBorder)
                                                           $ g bc
    where glmin = case (minf, lodf) of
                    (Nearest, Nearest) -> gl_NEAREST_MIPMAP_NEAREST
                    (Linear, Nearest) -> gl_LINEAR_MIPMAP_NEAREST
                    (Nearest, Linear) -> gl_NEAREST_MIPMAP_LINEAR                                                        
                    (Linear, Linear) -> gl_LINEAR_MIPMAP_LINEAR
          glmag = case magf of
                    Nearest -> gl_NEAREST                                                                            
                    Linear -> gl_LINEAR
          (se,te,re) = f e
          glwrap x Nothing = return ()
          glwrap x (Just Repeat) = glTexParameteri t x (fromIntegral gl_REPEAT)
          glwrap x (Just Mirror) = glTexParameteri t x (fromIntegral gl_MIRRORED_REPEAT)
          glwrap x (Just ClampToEdge) = glTexParameteri t x (fromIntegral gl_CLAMP_TO_EDGE)
          glwrap x (Just ClampToBorder) = glTexParameteri t x (fromIntegral gl_CLAMP_TO_BORDER)
          

newSampler2DShadow = undefined
newSampler2DArray = undefined
newSampler2DArrayShadow = undefined
newSampler3D = undefined
newSamplerCube = undefined
newSamplerCubeShadow = undefined

doForSampler :: Int -> (s -> Binding -> IO()) -> ShaderM s ()
doForSampler n io = modifyRenderIO (\s -> s { samplerNameToRenderIO = insert n io (samplerNameToRenderIO s) } )

data Shadow
data Sampler1D c
data Sampler1DArray c
data Sampler2D c = Sampler2D Int
data Sampler2DArray c
data Sampler3D c
data SamplerCube c

data SampleLod v x where
    SampleAuto :: SampleLod v F
    SampleBias :: FFloat -> SampleLod v F   
    SampleLod :: S x Float -> SampleLod v x
    SampleGrad :: v (S x Float) -> SampleLod v x

data SampleLod' v x where
    SampleAuto' :: SampleLod' v F
    SampleBias' :: FFloat -> SampleLod' v F   
    SampleGrad' :: v (S x Float) -> SampleLod' v x

fromLod' SampleAuto' = SampleAuto
fromLod' (SampleBias' x) = SampleBias x
fromLod' (SampleGrad' x) = SampleGrad x

type SampleProj x = Maybe (S x Float)
type SampleOffset v x = Maybe (v Int) 

-- | The type of a color sample made by a texture t 
type ColorSample x f = Color f (S x (ColorElement f))

sample1D            :: forall c x. ColorSampleable c =>  Sampler1D c          -> SampleLod V1 x -> SampleProj x -> SampleOffset V1 x -> V1 (S x Float) -> ColorSample x c
sample2D            :: forall c x. ColorSampleable c => Sampler2D c          -> SampleLod V2 x -> SampleProj x -> SampleOffset V2 x -> V2 (S x Float) -> ColorSample x c
sample3D            :: forall c x. ColorSampleable c =>  Sampler3D c          -> SampleLod V3 x -> SampleProj x -> SampleOffset V3 x -> V3 (S x Float) -> ColorSample x c
sample1DArray       :: forall c x. ColorSampleable c =>  Sampler1DArray c     -> SampleLod V1 x -> SampleOffset V1 x -> V2 (S x Float) -> ColorSample x c
sample2DArray       :: forall c x. ColorSampleable c =>  Sampler2DArray c     -> SampleLod V2 x -> SampleOffset V2 x -> V3 (S x Float) -> ColorSample x c
sampleCube          :: forall c x. ColorSampleable c =>  SamplerCube c        -> SampleLod V2 x -> V2 (S x Float) -> ColorSample x c

sample1DShadow      :: forall x. Sampler1D Shadow     -> SampleLod V1 x -> SampleProj x -> SampleOffset V1 x -> S x Float -> V1 (S x Float) -> S x Float
sample2DShadow      :: forall x. Sampler2D Shadow     -> SampleLod V2 x -> SampleProj x -> SampleOffset V2 x -> S x Float -> V2 (S x Float) -> S x Float
sample1DArrayShadow :: forall x. Sampler1DArray Shadow-> SampleLod V1 x -> SampleOffset V1 x -> S x Float -> V2 (S x Float) -> S x Float
sample2DArrayShadow :: forall x. Sampler2DArray Shadow-> SampleLod' V2 x -> SampleOffset V2 x -> S x Float -> V3 (S x Float)-> S x Float
sampleCubeShadow    :: forall x. SamplerCube Shadow   -> SampleLod' V2 x -> S x Float -> V2 (S x Float) -> S x Float

texelFetch1D        :: forall c x. ColorSampleable c =>  Sampler1D c          -> SampleOffset V1 x -> S x Level -> V1 (S x Int) -> ColorSample x c
texelFetch2D        :: forall c x. ColorSampleable c =>  Sampler2D c          -> SampleOffset V2 x -> S x Level -> V2 (S x Int) -> ColorSample x c
texelFetch3D        :: forall c x. ColorSampleable c =>  Sampler3D c          -> SampleOffset V3 x -> S x Level -> V3 (S x Int) -> ColorSample x c
texelFetch1DArray   :: forall c x. ColorSampleable c =>  Sampler1DArray c     -> SampleOffset V1 x -> S x Level -> V2 (S x Int) -> ColorSample x c
texelFetch2DArray   :: forall c x. ColorSampleable c =>  Sampler2DArray c     -> SampleOffset V2 x -> S x Level -> V3 (S x Int) -> ColorSample x c

sampler1Dsize      :: forall c x. Sampler1D c -> S x Level -> V1 (S x Int)
sampler2Dsize      :: forall c x. Sampler2D c -> S x Level -> V2 (S x Int)
sampler3Dsize      :: forall c x. Sampler3D c -> S x Level -> V3 (S x Int)
sampler1DArraysize :: forall c x. Sampler1DArray c -> S x Level -> V2 (S x Int)
sampler2DArraysize :: forall c x. Sampler2DArray c -> S x Level -> V3 (S x Int)
samplerCubesize    :: forall c x. SamplerCube c -> S x Level -> S x Int

sampler1Dsize = undefined
sampler2Dsize = undefined
sampler3Dsize = undefined
sampler1DArraysize = undefined
sampler2DArraysize = undefined
samplerCubesize = undefined    

sample1D = undefined
sample2D (Sampler2D sampId) lod proj off coord = sample (undefined :: c) "2D" sampId lod proj off coord v2toF iv2toF pv2toF 
sample3D = undefined
sampleCube = undefined
sample1DShadow = undefined
sample2DShadow = undefined
sampleCubeShadow = undefined
sample1DArray = undefined
sample2DArray = undefined
sample1DArrayShadow = undefined
sample2DArrayShadow = undefined

texelFetch1D        = undefined
texelFetch2D        = undefined
texelFetch3D        = undefined
texelFetch1DArray   = undefined
texelFetch2DArray   = undefined

sample f sName sampId lod proj off coord vToS ivToS pvToS =
    toColor f $ vec4S (STypeVec 4) $ do s <- useSampler sName sampId
                                        sampleFunc s proj lod off coord vToS ivToS pvToS 

v2toF (V2 x y) = do x' <- unS x
                    y' <- unS y
                    return $ "vec2(" ++ x' ++ ',':y' ++ ")"   
iv2toF (V2 x y) = "ivec2(" ++ show x ++ ',':show y ++ ")"   
pv2toF (V2 x y) z = do x' <- unS x
                       y' <- unS y
                       z' <- unS z
                       return $ "vec3(" ++ x' ++ ',':y' ++ ',':z' ++ ")"

sampleFunc s proj lod off coord vToS ivToS pvToS = do
    pc <- projCoordParam proj coord vToS pvToS 
    l <- lodParam lod vToS
    let o = offParam off ivToS
    b <- biasParam lod
    return $ "texture" ++ projName proj ++ lodName lod ++ offName off ++ '(' : s ++ ',' : pc ++ l ++ o ++ b ++ ")"  

projName Nothing = ""
projName _ = "Proj"

projCoordParam Nothing coord vToS pvToS = vToS coord
projCoordParam (Just p) coord vToS pvToS = pvToS coord p

lodParam (SampleLod x) _ = fmap (',':) (unS x)
lodParam (SampleGrad x) vToS = fmap (',':) (vToS x)
lodParam _ _ = return ""

biasParam :: SampleLod v x -> ExprM String 
biasParam (SampleBias (S x)) = do x' <- x
                                  return $ ',':x'
biasParam _ = return ""

offParam Nothing _ = ""
offParam (Just x) ivToS = ',' : ivToS x
    
lodName (SampleLod _) = "Lod"
lodName (SampleGrad _) = "Grad"
lodName _ = ""

offName Nothing = ""
offName _ = "Offset"

----------------------------------------------------------------------------------

data Image f = Image TexName (V2 Int) (CUInt -> IO ()) -- Internal

getImageName (Image tn _ _) = getTexName tn

getImageBinding (Image _ _ io) = io

imageSize :: Image f -> V2 Int
imageSize (Image _ s _) = s

getTexture1DImage :: Texture1D os f -> Level -> Render os f (Image f) 
getTexture2DImage :: Texture2D os f -> Level -> Render os f (Image f) 
getTexture3DImage :: Texture3D os f -> Level -> Int -> Render os f (Image f) 
getTexture1DArray :: Texture1DArray os f -> Level -> Slice -> Render os f (Image f) 
getTexture2DArray :: Texture2DArray os f -> Level -> Slice -> Render os f (Image f) 
getTextureCube :: TextureCube os f -> Level -> CubeSide -> Render os f (Image f) 

getTexture1DImage t@(Texture1D tn _) l = return $ Image tn (V2 (fromV1 (texture1DSize t l)) 1) $ \ap -> glFramebufferTexture1D gl_DRAW_FRAMEBUFFER ap gl_TEXTURE_1D (getTexName tn) (fromIntegral l)
getTexture2DImage t@(Texture2D tn _) l = return $ Image tn (texture2DSize t l) $ \ap -> glFramebufferTexture2D gl_DRAW_FRAMEBUFFER ap gl_TEXTURE_2D (getTexName tn) (fromIntegral l)
getTexture3DImage t@(Texture3D tn _) l z' = let V3 x y z = texture3DSize t l in return $ Image tn (V2 x y) $ \ap -> glFramebufferTextureLayer gl_DRAW_FRAMEBUFFER ap (getTexName tn) (fromIntegral l) (fromIntegral $ min z' (z-1))
getTexture1DArray t@(Texture1DArray tn _) l y' = let V2 x y = texture1DArraySize t l in return $ Image tn (V2 x 1) $ \ap -> glFramebufferTextureLayer gl_DRAW_FRAMEBUFFER ap (getTexName tn) (fromIntegral l) (fromIntegral $ min y' (y-1)) 
getTexture2DArray t@(Texture2DArray tn _) l z' = let V3 x y z = texture2DArraySize t l in return $ Image tn (V2 x y) $ \ap -> glFramebufferTextureLayer gl_DRAW_FRAMEBUFFER ap (getTexName tn) (fromIntegral l) (fromIntegral $ min z' (z-1)) 
getTextureCube t@(TextureCube tn _) l s = let x = textureCubeSize t l in return $ Image tn (V2 x x) $ \ap -> glFramebufferTexture2D gl_DRAW_FRAMEBUFFER ap (getGlCubeSide s) (getTexName tn) (fromIntegral l)

getGlCubeSide CubePosX = gl_TEXTURE_CUBE_MAP_POSITIVE_X 
getGlCubeSide CubeNegX = gl_TEXTURE_CUBE_MAP_NEGATIVE_X 
getGlCubeSide CubePosY = gl_TEXTURE_CUBE_MAP_POSITIVE_Y
getGlCubeSide CubeNegY = gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
getGlCubeSide CubePosZ = gl_TEXTURE_CUBE_MAP_POSITIVE_Z
getGlCubeSide CubeNegZ = gl_TEXTURE_CUBE_MAP_NEGATIVE_Z




