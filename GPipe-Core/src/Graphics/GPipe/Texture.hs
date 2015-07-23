{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, AllowAmbiguousTypes, EmptyDataDecls #-}
module Graphics.GPipe.Texture where

import Graphics.GPipe.Format
import Graphics.GPipe.Expr
import Graphics.GPipe.Context
import Graphics.GPipe.Shader
import Graphics.GPipe.Compiler
import Graphics.GPipe.Buffer
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Data.IORef

data Texture1D os a = Texture1D TexName (V1 Int) MaxLevels
data Texture1DArray os a = Texture1DArray TexName (V2 Int)  MaxLevels
data Texture2D os a = Texture2D TexName (V2 Int) MaxLevels | RenderBuffer2D TexName (V2 Int)
data Texture2DArray os a = Texture2DArray TexName (V1 Int) MaxLevels
data Texture3D os a = Texture3D TexName (Int, Int, Int) MaxLevels
data TextureCube os a = TextureCube TexName Int MaxLevels

type MaxLevels = Int

newTexture1D :: (ColorSampleable c, Monad m) => c -> V1 Int -> MaxLevels -> ContextT os f m (Texture1D os c)
newTexture1DArray :: (ColorSampleable c, Monad m) => c -> V2 Int -> MaxLevels -> ContextT os f m (Texture1DArray os c)
newTexture2D :: (TextureFormat c, MonadIO m) => c -> V2 Int-> MaxLevels -> ContextT os f m (Texture2D os c)
newTexture2DArray :: (ColorSampleable c, Monad m) => c -> V3 Int -> MaxLevels -> ContextT os f m (Texture2DArray os c)
newTexture3D :: (ColorRenderable c, Monad m) => c -> V3 Int -> MaxLevels -> ContextT os f m (Texture3D os c)
newTextureCube :: (ColorSampleable c, Monad m) => c -> Int -> MaxLevels -> ContextT os f m (TextureCube os c)

texture1DLevels :: Texture1D os c -> ContextT os f m Int 
texture1DArrayLevels :: Texture1DArray os c -> ContextT os f m Int 
texture2DLevels :: Texture2D os c -> ContextT os f m Int 
texture2DArrayLevels :: Texture2DArray os c -> ContextT os f m Int 
texture3DLevels :: Texture3D os c -> ContextT os f m Int 
textureCubeLevels :: TextureCube os c -> ContextT os f m Int 

texture1DLevels = undefined 
texture1DArrayLevels = undefined 
texture2DLevels = undefined 
texture2DArrayLevels = undefined 
texture3DLevels = undefined 
textureCubeLevels = undefined 


newTexture1D = undefined  
newTexture1DArray = undefined
newTexture2D f s@(V2 w h) mx | getGlFormat f == gl_STENCIL_INDEX = do 
                                t <- makeRenderBuff
                                liftContextIO $ do
                                   undefined
                                   return $ RenderBuffer2D t s
                             | otherwise = do
                                t <- makeTex
                                liftContextIO $ do
                                    let glintf = fromIntegral $ getGlInternalFormat f
                                        glf = getGlFormat f
                                        ls = min mx (calcMaxLevels (max w h))
                                        tex = Texture2D t s ls
                                    useTexSync t gl_TEXTURE_2D
                                    forM_ (zip (texture2DSizes tex) [0..]) $ \(V2 lw lh, l) ->
                                        glTexImage2D gl_TEXTURE_2D l glintf (fromIntegral lw) (fromIntegral lh) 0 glf gl_BYTE nullPtr
                                    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_BASE_LEVEL 0
                                    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAX_LEVEL (fromIntegral (ls-1))
                                    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_NEAREST_MIPMAP_NEAREST)
                                    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_NEAREST)                                    
                                    return tex

newTexture2DArray = undefined
newTexture3D  = undefined
newTextureCube  = undefined


texture1DSizes :: Texture1D os c -> [V1 Int] 
texture1DArraySizes :: Texture1DArray os c -> [V2 Int] 
texture2DSizes :: Texture2D os c -> [V2 Int] 
texture2DArraySizes :: Texture2DArray os c -> [V3 Int] 
texture3DSizes :: Texture3D os c -> [V3 Int] 
textureCubeSizes :: TextureCube os c -> [Int] 

texture1DSizes = undefined 
texture1DArraySizes = undefined 
texture2DSizes t@(Texture2D _ (V2 w h) ls) = map (\l -> V2 (calcLevelSize w l) (calcLevelSize h l)) [0..(ls-1)]
texture2DSizes (RenderBuffer2D _ (V2 w h)) = [V2 w h]
texture2DArraySizes = undefined 
texture3DSizes = undefined 
textureCubeSizes = undefined 

calcLevelSize :: Int -> Int -> Int
calcLevelSize size0 level = max 1 (size0 `div` (2 ^ level))

calcMaxLevels :: Int -> Int
calcMaxLevels s = 1 + truncate (logBase 2.0 (fromIntegral s :: Double))

type TexName = IORef CUInt 

makeTex :: MonadIO m => ContextT os f m TexName 
makeTex = do
    name <- liftContextIO $ fromIntegral <$> alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
    tex <- liftIO $ newIORef name 
    addContextFinalizer tex $ with (fromIntegral name) (glDeleteTextures 1)
    addFBOTextureFinalizer False tex
    return tex 

makeRenderBuff :: MonadIO m => ContextT os f m TexName 
makeRenderBuff = do
    name <- liftContextIO $ fromIntegral <$> alloca (\ptr -> glGenRenderbuffers 1 ptr >> peek ptr)
    tex <- liftIO $ newIORef name 
    addContextFinalizer tex $ with (fromIntegral name) (glDeleteRenderbuffers 1)
    addFBOTextureFinalizer True tex
    return tex 
    
useTex :: Integral a => TexName -> GLenum -> a -> IO ()
useTex texNameRef t bind = do glActiveTexture (gl_TEXTURE0 + fromIntegral bind)
                              n <- readIORef texNameRef
                              glBindTexture t n
                                             
useTexSync :: TexName -> GLenum -> IO ()
useTexSync tn t = do maxUnits <- alloca (\ptr -> glGetIntegerv gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS ptr >> peek ptr)  -- Use last for all sync actions, keeping 0.. for async drawcalls
                     useTex tn t (maxUnits-1)
                                 

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

noAnisotropy :: Anisotropy 
noAnisotropy = 1.0 

type MinFilter = Filter
type MagFilter = Filter
type LodFilter = Filter

data SamplerFilter c where
    SamplerFilter :: (ColorElement c ~ Float) => MagFilter -> MinFilter -> LodFilter -> Maybe Anisotropy -> SamplerFilter c 
    SamplerNearest :: SamplerFilter c

type SamplerEdgeMode dim c = (dim EdgeMode, BorderColor c)

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

getGlCompFunc :: ComparisonFunction -> GLenum
getGlCompFunc Never = gl_NEVER
getGlCompFunc Less = gl_LESS
getGlCompFunc Equal = gl_EQUAL
getGlCompFunc Lequal = gl_LEQUAL
getGlCompFunc Greater = gl_GREATER
getGlCompFunc Notequal = gl_NOTEQUAL
getGlCompFunc Gequal = gl_GEQUAL
getGlCompFunc Always = gl_ALWAYS
   
newSampler1D :: ColorSampleable c => (s -> (Texture1D os c, SamplerFilter c, SamplerEdgeMode V1 c)) -> Shader os f s (Sampler1D c)
newSampler1DArray :: ColorSampleable c => (s -> (Texture1DArray os c, SamplerFilter c, SamplerEdgeMode V1 c)) -> Shader os f s (Sampler1DArray c)
newSampler2D :: ColorSampleable c => (s -> (Texture2D os c, SamplerFilter c, SamplerEdgeMode V2 c)) -> Shader os f s (Sampler2D c)
newSampler3D :: ColorRenderable c => (s -> (Texture3D os c, SamplerFilter c, SamplerEdgeMode V3 c)) -> Shader os f s (Sampler3D c)
newSamplerCube :: ColorSampleable c => (s -> (TextureCube os c, SamplerFilter c)) -> Shader os f s (SamplerCube c)

newSampler1DShadow :: DepthRenderable d => (s -> (Texture1D os d, SamplerFilter c, SamplerEdgeMode V1 c, ComparisonFunction)) -> Shader os f s (Sampler1D Shadow)
newSampler1DArrayShadow :: DepthRenderable d => (s -> (Texture1DArray os d, SamplerFilter c, SamplerEdgeMode V1 c, ComparisonFunction)) -> Shader os f s (Sampler1DArray Shadow)
newSampler2DShadow :: DepthRenderable d => (s -> (Texture2D os d, SamplerFilter c, SamplerEdgeMode V2 c, ComparisonFunction)) -> Shader os f s (Sampler2D Shadow)
newSampler2DArray :: ColorSampleable c => (s -> (Texture2DArray os c, SamplerFilter c, SamplerEdgeMode V2 c)) -> Shader os f s (Sampler2DArray c)
newSampler2DArrayShadow :: DepthRenderable d => (s -> (Texture2DArray os d, SamplerFilter c, SamplerEdgeMode V2 c, ComparisonFunction)) -> Shader os f s (Sampler2DArray Shadow)
newSamplerCubeShadow :: DepthRenderable d => (s -> (TextureCube os d, SamplerFilter c, ComparisonFunction)) -> Shader os f s (SamplerCube Shadow)

newSampler1D = undefined
newSampler1DShadow = undefined
newSampler1DArray = undefined
newSampler1DArrayShadow = undefined
newSampler2D sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture2D tn _ _, filter, (V2 ex ey, ec)) = sf s 
                                                    in  do useTex tn gl_TEXTURE_2D bind
                                                           setSamplerFilter gl_TEXTURE_2D filter
                                                           setEdgeMode gl_TEXTURE_2D (Just ex, Just ey, Nothing) (return ())
                   return $ Sampler2D sampId

setEdgeMode t (se,te,re) bcio = do glwrap gl_TEXTURE_WRAP_S se
                                   glwrap gl_TEXTURE_WRAP_T te
                                   glwrap gl_TEXTURE_WRAP_R re
                                   when (se == Just ClampToBorder || te == Just ClampToBorder || re == Just ClampToBorder) 
                                      bcio
    where glwrap _ Nothing = return ()
          glwrap x (Just Repeat) = glTexParameteri t x (fromIntegral gl_REPEAT)
          glwrap x (Just Mirror) = glTexParameteri t x (fromIntegral gl_MIRRORED_REPEAT)
          glwrap x (Just ClampToEdge) = glTexParameteri t x (fromIntegral gl_CLAMP_TO_EDGE)
          glwrap x (Just ClampToBorder) = glTexParameteri t x (fromIntegral gl_CLAMP_TO_BORDER)
          
setSamplerFilter t (SamplerFilter magf minf lodf a) = setSamplerFilter' t magf minf lodf a
setSamplerFilter t SamplerNearest = setSamplerFilter' t Nearest Nearest Nearest Nothing

setSamplerFilter' :: GLenum -> MagFilter -> MinFilter -> LodFilter -> Maybe Anisotropy -> IO () 
setSamplerFilter' t magf minf lodf a = do
                                           glTexParameteri t gl_TEXTURE_MIN_FILTER (fromIntegral glmin)
                                           glTexParameteri t gl_TEXTURE_MAG_FILTER (fromIntegral glmag)
                                           case a of
                                                Nothing -> return ()
                                                Just a' -> glTexParameterf t gl_TEXTURE_MAX_ANISOTROPY_EXT (fromRational $ toRational a')
    where glmin = case (minf, lodf) of
                    (Nearest, Nearest) -> gl_NEAREST_MIPMAP_NEAREST
                    (Linear, Nearest) -> gl_LINEAR_MIPMAP_NEAREST
                    (Nearest, Linear) -> gl_NEAREST_MIPMAP_LINEAR                                                        
                    (Linear, Linear) -> gl_LINEAR_MIPMAP_LINEAR
          glmag = case magf of
                    Nearest -> gl_NEAREST                                                                            
                    Linear -> gl_LINEAR
          

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

fromLod' :: SampleLod' v x -> SampleLod v x
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
sampleCube          :: forall c x. ColorSampleable c =>  SamplerCube c        -> SampleLod V3 x -> V3 (S x Float) -> ColorSample x c

sample1DShadow      :: forall x. Sampler1D Shadow     -> SampleLod V1 x -> SampleProj x -> SampleOffset V1 x -> S x Float -> V1 (S x Float) -> S x Float
sample2DShadow      :: forall x. Sampler2D Shadow     -> SampleLod V2 x -> SampleProj x -> SampleOffset V2 x -> S x Float -> V2 (S x Float) -> S x Float
sample1DArrayShadow :: forall x. Sampler1DArray Shadow-> SampleLod V1 x -> SampleOffset V1 x -> S x Float -> V2 (S x Float) -> S x Float
sample2DArrayShadow :: forall x. Sampler2DArray Shadow-> SampleLod' V2 x -> SampleOffset V2 x -> S x Float -> V3 (S x Float)-> S x Float
sampleCubeShadow    :: forall x. SamplerCube Shadow   -> SampleLod' V3 x -> S x Float -> V3 (S x Float) -> S x Float

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
    b <- biasParam lod
    return $ "texture" ++ projName proj ++ lodName lod ++ offName off ++ '(' : s ++ ',' : pc ++ l ++ o ++ b ++ ")"  
  where 
    o = offParam off ivToS
    
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

data Image f = Image TexName Int Int (V2 Int) (CUInt -> IO ()) -- the two Ints is last two in FBOKey

imageEquals :: Image a -> Image b -> Bool
imageEquals (Image tn' k1' k2' _ _) (Image tn k1 k2 _ _) = tn' == tn && k1' == k1 && k2' == k2

getImageBinding :: Image t -> CUInt -> IO ()
getImageBinding (Image _ _ _ _ io) = io

getImageFBOKey :: Image t -> IO FBOKey
getImageFBOKey (Image tn k1 k2 _ _) = do tn' <- readIORef tn
                                         return $ FBOKey tn' k1 k2 

imageSize :: Image f -> V2 Int
imageSize (Image _ _ _ s _) = s

getTexture1DImage :: Texture1D os f -> Level -> Render os f (Image f) 
getTexture2DImage :: Texture2D os f -> Level -> Render os f (Image f) 
getTexture3DImage :: Texture3D os f -> Level -> Int -> Render os f (Image f) 
getTexture1DArray :: Texture1DArray os f -> Level -> Slice -> Render os f (Image f) 
getTexture2DArray :: Texture2DArray os f -> Level -> Slice -> Render os f (Image f) 
getTextureCube :: TextureCube os f -> Level -> CubeSide -> Render os f (Image f) 

getTexture1DImage t@(Texture1D tn _ ls) l' = let l = min ls l' in return $ Image tn 0 l (V2 (fromV1 (texture1DSizes t !! l)) 1) $ \attP -> do { n <- readIORef tn; glFramebufferTexture1D gl_DRAW_FRAMEBUFFER attP gl_TEXTURE_1D n (fromIntegral l) }
getTexture2DImage t@(Texture2D tn _ ls) l' = let l = min ls l' in return $ Image tn 0 l (texture2DSizes t !! l) $ \attP -> do { n <- readIORef tn; glFramebufferTexture2D gl_DRAW_FRAMEBUFFER attP gl_TEXTURE_2D n (fromIntegral l) }
getTexture2DImage t@(RenderBuffer2D tn _) _ = return $ Image tn (-1) 0 (head $ texture2DSizes t) $ \attP -> do { n <- readIORef tn; glFramebufferRenderbuffer gl_DRAW_FRAMEBUFFER attP gl_RENDERBUFFER n }
getTexture3DImage t@(Texture3D tn _ ls) l' z' = let l = min ls l' 
                                                    V3 x y z = texture3DSizes t !! l 
                                                in return $ Image tn z' l (V2 x y) $ \attP -> do { n <- readIORef tn; glFramebufferTextureLayer gl_DRAW_FRAMEBUFFER attP n (fromIntegral l) (fromIntegral $ min z' (z-1)) }
getTexture1DArray t@(Texture1DArray tn _ ls) l' y' = let l = min ls l' 
                                                         V2 x y = texture1DArraySizes t !! l 
                                                     in return $ Image tn y' l (V2 x 1) $ \attP -> do { n <- readIORef tn; glFramebufferTextureLayer gl_DRAW_FRAMEBUFFER attP n (fromIntegral l) (fromIntegral $ min y' (y-1)) }
getTexture2DArray t@(Texture2DArray tn _ ls) l' z' = let l = min ls l' 
                                                         V3 x y z = texture2DArraySizes t !! l 
                                                     in return $ Image tn z' l (V2 x y) $ \attP -> do { n <- readIORef tn; glFramebufferTextureLayer gl_DRAW_FRAMEBUFFER attP n (fromIntegral l) (fromIntegral $ min z' (z-1)) } 
getTextureCube t@(TextureCube tn _ ls) l' s = let l = min ls l' 
                                                  x = textureCubeSizes t !! l
                                                  s' = getGlCubeSide s
                                              in return $ Image tn (fromIntegral s') l (V2 x x) $ \attP -> do { n <- readIORef tn; glFramebufferTexture2D gl_DRAW_FRAMEBUFFER attP s' n (fromIntegral l) }

getGlCubeSide :: CubeSide -> GLenum
getGlCubeSide CubePosX = gl_TEXTURE_CUBE_MAP_POSITIVE_X 
getGlCubeSide CubeNegX = gl_TEXTURE_CUBE_MAP_NEGATIVE_X 
getGlCubeSide CubePosY = gl_TEXTURE_CUBE_MAP_POSITIVE_Y
getGlCubeSide CubeNegY = gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
getGlCubeSide CubePosZ = gl_TEXTURE_CUBE_MAP_POSITIVE_Z
getGlCubeSide CubeNegZ = gl_TEXTURE_CUBE_MAP_NEGATIVE_Z




