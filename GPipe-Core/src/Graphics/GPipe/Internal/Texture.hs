{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, ScopedTypeVariables, AllowAmbiguousTypes, EmptyDataDecls #-}
module Graphics.GPipe.Internal.Texture where

import Graphics.GPipe.Internal.Format
import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.Context
import Graphics.GPipe.Internal.Shader
import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.Buffer
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IntMap.Lazy (insert)

import Graphics.GL.Core33
import Graphics.GL.Types
import Graphics.GL.Ext.EXT.TextureFilterAnisotropic

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Control.Monad
import Data.IORef
import Control.Applicative
import Control.Monad.Exception (bracket, MonadAsyncException)
import Linear.V4
import Linear.V3
import Linear.V2
import Control.Exception (throwIO)
import Control.Monad.Trans.Class (lift)

data Texture1D os a = Texture1D TexName Size1 MaxLevels
data Texture1DArray os a = Texture1DArray TexName Size2 MaxLevels
data Texture2D os a = Texture2D TexName Size2 MaxLevels 
                    | RenderBuffer2D TexName Size2
data Texture2DArray os a = Texture2DArray TexName Size3 MaxLevels
data Texture3D os a = Texture3D TexName Size3 MaxLevels
data TextureCube os a = TextureCube TexName Size1 MaxLevels

type MaxLevels = Int

type Size1 = Int
type Size2 = V2 Int
type Size3 = V3 Int

newTexture1D :: forall w os f c m. (ColorSampleable c, MonadIO m) => Format c -> Size1 -> MaxLevels -> ContextT w os f m (Texture1D os (Format c))
newTexture1DArray :: forall w os f c m. (ColorSampleable c, MonadIO m) => Format c -> Size2 -> MaxLevels -> ContextT w os f m (Texture1DArray os (Format c))
newTexture2D :: forall w os f c m. (TextureFormat c, MonadIO m) => Format c -> Size2 -> MaxLevels -> ContextT w os f m (Texture2D os (Format c))
newTexture2DArray :: forall w os f c m. (ColorSampleable c, MonadIO m) => Format c -> Size3 -> MaxLevels -> ContextT w os f m (Texture2DArray os (Format c))
newTexture3D :: forall w os f c m. (ColorRenderable c, MonadIO m) => Format c -> Size3 -> MaxLevels -> ContextT w os f m (Texture3D os (Format c))
newTextureCube :: forall w os f c m. (ColorSampleable c, MonadIO m) => Format c -> Size1 -> MaxLevels -> ContextT w os f m (TextureCube os (Format c))

newTexture1D f s mx | s < 0 = error "newTexture1D, negative size" 
                    | mx <= 0 = error "newTexture1D, non-positive MaxLevels"
                    | otherwise = do
                        mxSize <- getGlValue GL_MAX_TEXTURE_SIZE
                        if s > mxSize 
                          then liftIO $ throwIO $ GPipeException "newTexture1D, size larger then maximum supported by graphics driver"
                          else do
                            t <- makeTex
                            let glintf = fromIntegral $ getGlInternalFormat f
                                glf = getGlFormat (undefined :: c)
                                ls = min mx (calcMaxLevels s)
                                tex = Texture1D t s ls
                            liftContextIOAsync $ do
                                useTexSync t GL_TEXTURE_1D
                                forM_ (zip (texture1DSizes tex) [0..]) $ \(lw, l) ->
                                    glTexImage1D GL_TEXTURE_1D l glintf (fromIntegral lw) 0 glf GL_BYTE nullPtr
                                setDefaultTexParams GL_TEXTURE_1D (ls-1)                                    
                            return tex  
newTexture1DArray f s@(V2 w sl) mx 
                    | w < 0 || sl < 0 = error "newTexture1DArray, negative size" 
                    | mx <= 0 = error "newTexture1DArray, non-positive MaxLevels"
                    | otherwise = do
                            mxSize <- getGlValue GL_MAX_TEXTURE_SIZE
                            if w > mxSize || sl > mxSize 
                              then liftIO $ throwIO $ GPipeException "newTexture1DArray, size larger then maximum supported by graphics driver"
                              else do
                                t <- makeTex
                                let glintf = fromIntegral $ getGlInternalFormat f
                                    glf = getGlFormat (undefined :: c)
                                    ls = min mx (calcMaxLevels w)
                                    tex = Texture1DArray t s ls
                                liftContextIOAsync $ do
                                    useTexSync t GL_TEXTURE_1D_ARRAY
                                    forM_ (zip (texture1DArraySizes tex) [0..]) $ \(V2 lw _, l) ->
                                        glTexImage2D GL_TEXTURE_1D_ARRAY l glintf (fromIntegral lw) (fromIntegral sl) 0 glf GL_BYTE nullPtr
                                    setDefaultTexParams GL_TEXTURE_1D_ARRAY (ls-1)                                    
                                return tex
newTexture2D f s@(V2 w h) mx | w < 0 || h < 0 = error "newTexture2D, negative size" 
                             | mx <= 0 = error "newTexture2D, non-positive MaxLevels"
                             | getGlFormat (undefined :: c) == GL_STENCIL_INDEX = do 
                                mxSize <- getGlValue GL_MAX_RENDERBUFFER_SIZE
                                if w > mxSize || h > mxSize 
                                  then liftIO $ throwIO $ GPipeException "newTexture2D, size larger then maximum supported by graphics driver"
                                  else do
                                    t <- makeRenderBuff
                                    liftContextIOAsync $ 
                                       glRenderbufferStorage GL_RENDERBUFFER (getGlInternalFormat f) (fromIntegral w) (fromIntegral h)
                                    return $ RenderBuffer2D t s
                             | otherwise = do
                                mxSize <- getGlValue GL_MAX_TEXTURE_SIZE
                                if w > mxSize || h > mxSize 
                                  then liftIO $ throwIO $ GPipeException "newTexture2D, size larger then maximum supported by graphics driver"
                                  else do
                                    t <- makeTex
                                    let glintf = fromIntegral $ getGlInternalFormat f
                                        glf = getGlFormat (undefined :: c)
                                        ls = min mx (calcMaxLevels (max w h))
                                        tex = Texture2D t s ls
                                    liftContextIOAsync $ do
                                        useTexSync t GL_TEXTURE_2D
                                        forM_ (zip (texture2DSizes tex) [0..]) $ \(V2 lw lh, l) ->
                                            glTexImage2D GL_TEXTURE_2D l glintf (fromIntegral lw) (fromIntegral lh) 0 glf GL_BYTE nullPtr
                                        setDefaultTexParams GL_TEXTURE_2D (ls-1)                                    
                                    return tex

newTexture2DArray f s@(V3 w h sl) mx 
                                | w < 0 || h < 0 || sl < 0 = error "newTexture2DArray, negative size" 
                                | mx <= 0 = error "newTexture2DArray, non-positive MaxLevels"
                                | otherwise = do
                    mxSize <- getGlValue GL_MAX_TEXTURE_SIZE
                    if w > mxSize || h > mxSize || sl > mxSize 
                      then liftIO $ throwIO $ GPipeException "newTexture2DArray, size larger then maximum supported by graphics driver"
                      else do
                        t <- makeTex
                        let glintf = fromIntegral $ getGlInternalFormat f
                            glf = getGlFormat (undefined :: c)
                            ls = min mx (calcMaxLevels (max w h))
                            tex = Texture2DArray t s ls
                        liftContextIOAsync $ do
                            useTexSync t GL_TEXTURE_2D_ARRAY
                            forM_ (zip (texture2DArraySizes tex) [0..]) $ \(V3 lw lh _, l) ->
                                glTexImage3D GL_TEXTURE_2D_ARRAY l glintf (fromIntegral lw) (fromIntegral lh) (fromIntegral sl) 0 glf GL_BYTE nullPtr
                            setDefaultTexParams GL_TEXTURE_2D_ARRAY (ls-1)                                    
                        return tex  

newTexture3D f s@(V3 w h d) mx | w < 0 || h < 0 || d < 0 = error "newTexture3D, negative size" 
                               | mx <= 0 = error "newTexture3D, non-positive MaxLevels"
                               | otherwise = do
                    mxSize <- getGlValue GL_MAX_TEXTURE_SIZE
                    if w > mxSize || h > mxSize || d > mxSize 
                      then liftIO $ throwIO $ GPipeException "newTexture3D, size larger then maximum supported by graphics driver"
                      else do
                        t <- makeTex
                        let glintf = fromIntegral $ getGlInternalFormat f
                            glf = getGlFormat (undefined :: c)
                            ls = min mx (calcMaxLevels (max w (max h d)))
                            tex = Texture3D t s ls
                        liftContextIOAsync $ do
                            useTexSync t GL_TEXTURE_3D
                            forM_ (zip (texture3DSizes tex) [0..]) $ \(V3 lw lh ld, l) ->
                                glTexImage3D GL_TEXTURE_3D l glintf (fromIntegral lw) (fromIntegral lh) (fromIntegral ld) 0 glf GL_BYTE nullPtr
                            setDefaultTexParams GL_TEXTURE_3D (ls-1)                                    
                        return tex
newTextureCube f s mx | s < 0 = error "newTextureCube, negative size" 
                      | mx <= 0 = error "newTextureCube, non-positive MaxLevels"
                      | otherwise = do
                    mxSize <- getGlValue GL_MAX_CUBE_MAP_TEXTURE_SIZE
                    if s > mxSize 
                      then liftIO $ throwIO $ GPipeException "newTextureCube, size larger then maximum supported by graphics driver"
                      else do
                            t <- makeTex
                            let glintf = fromIntegral $ getGlInternalFormat f
                                glf = getGlFormat (undefined :: c)
                                ls = min mx (calcMaxLevels s)
                                tex = TextureCube t s ls
                            liftContextIOAsync $ do
                                useTexSync t GL_TEXTURE_CUBE_MAP
                                forM_ [(size, getGlCubeSide side) | size <- zip (textureCubeSizes tex) [0..], side <- [minBound..maxBound]] $ \((lx, l), side) ->
                                    glTexImage2D side l glintf (fromIntegral lx) (fromIntegral lx) 0 glf GL_BYTE nullPtr
                                setDefaultTexParams GL_TEXTURE_CUBE_MAP (ls-1)
                                glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
                                glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
                                glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_R GL_CLAMP_TO_EDGE                                    
                            return tex

getGlValue :: MonadIO m =>  GLenum -> ContextT w os f m Int
getGlValue enum = liftContextIO $ alloca (\ptr -> liftM fromIntegral (glGetIntegerv enum ptr >> peek ptr)) 

setDefaultTexParams :: GLenum -> Int -> IO ()
setDefaultTexParams t ml = do
                            glTexParameteri t GL_TEXTURE_BASE_LEVEL 0
                            glTexParameteri t GL_TEXTURE_MAX_LEVEL (fromIntegral ml)
                            glTexParameteri t GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_NEAREST
                            glTexParameteri t GL_TEXTURE_MAG_FILTER GL_NEAREST  


texture1DLevels :: Texture1D os f -> Int 
texture1DArrayLevels :: Texture1DArray os f -> Int 
texture2DLevels :: Texture2D os f -> Int 
texture2DArrayLevels :: Texture2DArray os f -> Int 
texture3DLevels :: Texture3D os f -> Int 
textureCubeLevels :: TextureCube os f -> Int 
texture1DLevels (Texture1D _ _ ls) = ls
texture1DArrayLevels (Texture1DArray _ _ ls) = ls 
texture2DLevels (Texture2D _ _ ls) = ls 
texture2DLevels (RenderBuffer2D _ _) = 1 
texture2DArrayLevels (Texture2DArray _ _ ls) = ls 
texture3DLevels (Texture3D _ _ ls) = ls 
textureCubeLevels (TextureCube _ _ ls) = ls 
                            
texture1DSizes :: Texture1D os f -> [Size1] 
texture1DArraySizes :: Texture1DArray os f -> [Size2] 
texture2DSizes :: Texture2D os f -> [Size2] 
texture2DArraySizes :: Texture2DArray os f -> [Size3] 
texture3DSizes :: Texture3D os f -> [Size3] 
textureCubeSizes :: TextureCube os f -> [Size1] 
texture1DSizes (Texture1D _ w ls) = map (calcLevelSize w) [0..(ls-1)] 
texture1DArraySizes (Texture1DArray _ (V2 w s) ls) = map (\l -> V2 (calcLevelSize w l) s) [0..(ls-1)]
texture2DSizes (Texture2D _ (V2 w h) ls) = map (\l -> V2 (calcLevelSize w l) (calcLevelSize h l)) [0..(ls-1)]
texture2DSizes (RenderBuffer2D _ s) = [s]
texture2DArraySizes (Texture2DArray _ (V3 w h s) ls) = map (\l -> V3 (calcLevelSize w l) (calcLevelSize h l) s) [0..(ls-1)] 
texture3DSizes (Texture3D _ (V3 w h d) ls) = map (\l -> V3 (calcLevelSize w l) (calcLevelSize h l) (calcLevelSize d l)) [0..(ls-1)] 
textureCubeSizes (TextureCube _ x ls) = map (calcLevelSize x) [0..(ls-1)]

calcLevelSize :: Int -> Int -> Int
calcLevelSize size0 level = max 1 (size0 `div` (2 ^ level))

calcMaxLevels :: Int -> Int
calcMaxLevels s = 1 + truncate (logBase 2.0 (fromIntegral s :: Double))

type TexName = IORef GLuint 

makeTex :: MonadIO m => ContextT w os f m TexName 
makeTex = do
    name <- liftContextIO $ alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
    tex <- liftIO $ newIORef name 
    addContextFinalizer tex $ with name (glDeleteTextures 1)
    addFBOTextureFinalizer False tex
    return tex 

makeRenderBuff :: MonadIO m => ContextT w os f m TexName 
makeRenderBuff = do
    name <- liftContextIO $ alloca (\ptr -> glGenRenderbuffers 1 ptr >> peek ptr)
    tex <- liftIO $ newIORef name 
    addContextFinalizer tex $ with name (glDeleteRenderbuffers 1)
    addFBOTextureFinalizer True tex
    return tex 
    
useTex :: Integral a => TexName -> GLenum -> a -> IO Int
useTex texNameRef t bind = do glActiveTexture (GL_TEXTURE0 + fromIntegral bind)
                              n <- readIORef texNameRef
                              glBindTexture t n
                              return (fromIntegral n)
                                             
useTexSync :: TexName -> GLenum -> IO ()
useTexSync tn t = do maxUnits <- alloca (\ptr -> glGetIntegerv GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS ptr >> peek ptr)  -- Use last for all sync actions, keeping 0.. for async drawcalls
                     void $ useTex tn t (maxUnits-1)
                                 

type Level = Int
data CubeSide = CubePosX | CubeNegX | CubePosY | CubeNegY | CubePosZ | CubeNegZ deriving (Eq, Enum, Bounded)

type StartPos1 = Int
type StartPos2 = V2 Int
type StartPos3 = V3 Int


writeTexture1D      :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture1D os (Format c) -> Level -> StartPos1 -> Size1 -> [h] -> ContextT w os f m ()
writeTexture1DArray :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture1DArray os (Format c) -> Level -> StartPos2 -> Size2 -> [h] -> ContextT w os f m ()
writeTexture2D      :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture2D os (Format c) -> Level -> StartPos2 -> Size2 -> [h] -> ContextT w os f m ()
writeTexture2DArray :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture2DArray os (Format c) -> Level -> StartPos3 -> Size3 -> [h] -> ContextT w os f m ()
writeTexture3D      :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture3D os (Format c) -> Level -> StartPos3 -> Size3 -> [h] -> ContextT w os f m ()
writeTextureCube    :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => TextureCube os (Format c) -> Level -> CubeSide -> StartPos2 -> Size2 -> [h] -> ContextT w os f m ()

writeTexture1DFromBuffer     :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture1D os (Format c) -> Level -> StartPos1 -> Size1 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
writeTexture1DArrayFromBuffer:: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture1DArray os (Format c) -> Level -> StartPos2 -> Size2 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
writeTexture2DFromBuffer     :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture2D os (Format c) -> Level -> StartPos2 -> Size2 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
writeTexture2DArrayFromBuffer:: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture2DArray os (Format c) -> Level -> StartPos3 -> Size3 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
writeTexture3DFromBuffer     :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture3D os (Format c) -> Level -> StartPos3 -> Size3 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
writeTextureCubeFromBuffer   :: forall b c h w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => TextureCube os (Format c) -> Level -> CubeSide -> StartPos2 -> Size2 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()


readTexture1D      :: forall a b c h w os f m. (MonadAsyncException m, MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture1D os (Format c) -> Level -> StartPos1 -> Size1 -> (a -> h -> ContextT w os f m a) -> a -> ContextT w os f m a
readTexture1DArray :: forall a b c h w os f m. (MonadAsyncException m, MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture1DArray os (Format c) -> Level -> StartPos2 -> Size1 -> (a -> h -> ContextT w os f m a) -> a -> ContextT w os f m a
readTexture2D      :: forall a b c h w os f m. (MonadAsyncException m, MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture2D os (Format c) -> Level -> StartPos2 -> Size2 -> (a -> h -> ContextT w os f m a) -> a -> ContextT w os f m a
readTexture2DArray :: forall a b c h w os f m. (MonadAsyncException m, MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture2DArray os (Format c) -> Level -> StartPos3 -> Size2 -> (a -> h -> ContextT w os f m a) -> a -> ContextT w os f m a
readTexture3D      :: forall a b c h w os f m. (MonadAsyncException m, MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => Texture3D os (Format c) -> Level -> StartPos3 -> Size2 -> (a -> h -> ContextT w os f m a) -> a -> ContextT w os f m a
readTextureCube    :: forall a b c h w os f m. (MonadAsyncException m, MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) => TextureCube os (Format c) -> Level -> CubeSide -> StartPos2 -> Size2 -> (a -> h -> ContextT w os f m a) -> a -> ContextT w os f m a

readTexture1DToBuffer     :: forall b c w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture1D os (Format c) -> Level -> StartPos1 -> Size1 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
readTexture1DArrayToBuffer:: forall b c w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture1DArray os (Format c) -> Level -> StartPos2 -> Size1 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
readTexture2DToBuffer     :: forall b c w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture2D os (Format c) -> Level -> StartPos2 -> Size2 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
readTexture2DArrayToBuffer:: forall b c w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture2DArray os (Format c) -> Level -> StartPos3 -> Size2 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
readTexture3DToBuffer     :: forall b c w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => Texture3D os (Format c) -> Level -> StartPos3 -> Size2 -> Buffer os b -> BufferStartPos -> ContextT w os f m ()
readTextureCubeToBuffer   :: forall b c w os f m. (MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) (HostFormat b) ~ b) => TextureCube os (Format c) -> Level -> CubeSide -> StartPos2 -> Size2 -> Buffer os b-> BufferStartPos -> ContextT w os f m ()

getGlColorFormat :: (TextureFormat f, BufferFormat b) => f -> b -> GLenum
getGlColorFormat f b = let x = getGlFormat f in if x == GL_DEPTH_STENCIL || x == GL_DEPTH_COMPONENT then GL_DEPTH_COMPONENT else getGlPaddedFormat b

writeTexture1D t@(Texture1D texn _ ml) l x w d
    | l < 0 || l >= ml = error "writeTexture1D, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture1D, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture1D, w out of bounds"
    | otherwise = liftContextIOAsync $ do 
                     let b = makeBuffer undefined undefined 0 :: Buffer os b
                         size = w*bufElementSize b
                     allocaBytes size $ \ ptr -> do
                         end <- bufferWriteInternal b ptr (take w d)
                         if end `minusPtr` ptr /= size 
                            then error "writeTexture1D, data list too short"
                            else do
                                useTexSync texn GL_TEXTURE_1D
                                glTexSubImage1D GL_TEXTURE_1D (fromIntegral l) (fromIntegral x) (fromIntegral w) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
    where mx = texture1DSizes t !! l  

writeTexture1DArray t@(Texture1DArray texn _ ml) l (V2 x y) (V2 w h) d 
    | l < 0 || l >= ml = error "writeTexture1DArray, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture1DArray, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture1DArray, w out of bounds"
    | y < 0 || y >= my = error "writeTexture1DArray, y out of bounds"
    | h < 0 || y+h > my = error "writeTexture2D, h out of bounds"
    | otherwise = liftContextIOAsync $ do 
                     let b = makeBuffer undefined undefined 0 :: Buffer os b
                         size = w*h*bufElementSize b
                     allocaBytes size $ \ ptr -> do
                         end <- bufferWriteInternal b ptr (take (w*h) d)
                         if end `minusPtr` ptr /= size 
                            then error "writeTexture1DArray, data list too short"
                            else do
                                useTexSync texn GL_TEXTURE_1D_ARRAY
                                glTexSubImage2D GL_TEXTURE_1D_ARRAY (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
    where V2 mx my = texture1DArraySizes t !! l  
writeTexture2D t@(Texture2D texn _ ml) l (V2 x y) (V2 w h) d 
    | l < 0 || l >= ml = error "writeTexture2D, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture2D, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture2D, w out of bounds"
    | y < 0 || y >= my = error "writeTexture2D, y out of bounds"
    | h < 0 || y+h > my = error "writeTexture2D, h out of bounds"
    | otherwise = liftContextIOAsync $ do 
                     let b = makeBuffer undefined undefined 0 :: Buffer os b
                         size = w*h*bufElementSize b
                     allocaBytes size $ \ ptr -> do
                         end <- bufferWriteInternal b ptr (take (w*h) d)
                         if end `minusPtr` ptr /= size 
                            then error "writeTexture2D, data list too short"
                            else do
                                useTexSync texn GL_TEXTURE_2D
                                glTexSubImage2D GL_TEXTURE_2D (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
    where V2 mx my = texture2DSizes t !! l                                                                                
writeTexture2DArray t@(Texture2DArray texn _ ml) l (V3 x y z) (V3 w h d) dat 
    | l < 0 || l >= ml = error "writeTexture2DArray, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture2DArray, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture2DArray, w out of bounds"
    | y < 0 || y >= my = error "writeTexture2DArray, y out of bounds"
    | h < 0 || y+h > my = error "writeTexture2DArray, h out of bounds"
    | z < 0 || z >= mz = error "writeTexture2DArray, z out of bounds"
    | d < 0 || z+d > mz = error "writeTexture2DArray, d out of bounds"
    | otherwise = liftContextIOAsync $ do 
                     let b = makeBuffer undefined undefined 0 :: Buffer os b
                         size = w*h*d*bufElementSize b
                     allocaBytes size $ \ ptr -> do
                         end <- bufferWriteInternal b ptr (take (w*h*d) dat)
                         if end `minusPtr` ptr /= size 
                            then error "writeTexture2DArray, data list too short"
                            else do
                                useTexSync texn GL_TEXTURE_2D_ARRAY
                                glTexSubImage3D GL_TEXTURE_2D_ARRAY (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w) (fromIntegral h) (fromIntegral d) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
    where V3 mx my mz = texture2DArraySizes t !! l 
writeTexture3D t@(Texture3D texn _ ml) l (V3 x y z) (V3 w h d) dat
    | l < 0 || l >= ml = error "writeTexture3D, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture3D, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture3D, w out of bounds"
    | y < 0 || y >= my = error "writeTexture3D, y out of bounds"
    | h < 0 || y+h > my = error "writeTexture3D, h out of bounds"
    | z < 0 || z >= mz = error "writeTexture3D, z out of bounds"
    | d < 0 || z+d > mz = error "writeTexture3D, d out of bounds"
    | otherwise = liftContextIOAsync $ do 
                     let b = makeBuffer undefined undefined 0 :: Buffer os b
                         size = w*h*d*bufElementSize b
                     allocaBytes size $ \ ptr -> do
                         end <- bufferWriteInternal b ptr (take (w*h*d) dat)
                         if end `minusPtr` ptr /= size 
                            then error "writeTexture3D, data list too short"
                            else do
                                useTexSync texn GL_TEXTURE_3D
                                glTexSubImage3D GL_TEXTURE_3D (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w) (fromIntegral h) (fromIntegral d) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
    where V3 mx my mz = texture3DSizes t !! l                                       
writeTextureCube t@(TextureCube texn _ ml) l s (V2 x y) (V2 w h) d 
    | l < 0 || l >= ml = error "writeTextureCube, level out of bounds"
    | x < 0 || x >= mxy = error "writeTextureCube, x out of bounds"
    | w < 0 || x+w > mxy = error "writeTextureCube, w out of bounds"
    | y < 0 || y >= mxy = error "writeTextureCube, y out of bounds"
    | h < 0 || y+h > mxy = error "writeTextureCube, h out of bounds"
    | otherwise = liftContextIOAsync $ do 
                     let b = makeBuffer undefined undefined 0 :: Buffer os b
                         size = w*h*bufElementSize b
                     allocaBytes size $ \ ptr -> do
                         end <- bufferWriteInternal b ptr (take (w*h) d)
                         if end `minusPtr` ptr /= size 
                            then error "writeTextureCube, data list too short"
                            else do
                                useTexSync texn GL_TEXTURE_CUBE_MAP
                                glTexSubImage2D (getGlCubeSide s) (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
    where mxy = textureCubeSizes t !! l

writeTexture1DFromBuffer t@(Texture1D texn _ ml) l x w b i 
    | l < 0 || l >= ml = error "writeTexture1DFromBuffer, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture1DFromBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture1DFromBuffer, w out of bounds"
    | i < 0 || i > bufferLength b = error "writeTexture1DFromBuffer, i out of bounds"
    | bufferLength b - i < w = error "writeTexture1DFromBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do 
                    useTexSync texn GL_TEXTURE_1D
                    bname <- readIORef $ bufName b
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER bname
                    glTexSubImage1D GL_TEXTURE_1D (fromIntegral l) (fromIntegral x) (fromIntegral w) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)        
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER 0
    where mx = texture1DSizes t !! l
writeTexture1DArrayFromBuffer t@(Texture1DArray texn _ ml) l (V2 x y) (V2 w h) b i 
    | l < 0 || l >= ml = error "writeTexture1DArrayFromBuffer, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture1DArrayFromBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture1DArrayFromBuffer, w out of bounds"
    | y < 0 || y >= my = error "writeTexture1DArrayFromBuffer, y out of bounds"
    | h < 0 || y+h > my = error "writeTexture1DArrayFromBuffer, h out of bounds"
    | i < 0 || i > bufferLength b = error "writeTexture1DArrayFromBuffer, i out of bounds"
    | bufferLength b - i < w*h = error "writeTexture1DArrayFromBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do 
                    useTexSync texn GL_TEXTURE_1D_ARRAY
                    bname <- readIORef $ bufName b
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER bname
                    glTexSubImage2D GL_TEXTURE_1D_ARRAY (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)        
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER 0
    where V2 mx my = texture1DArraySizes t !! l
writeTexture2DFromBuffer t@(Texture2D texn _ ml) l (V2 x y) (V2 w h) b i
    | l < 0 || l >= ml = error "writeTexture2DFromBuffer, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture2DFromBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture2DFromBuffer, w out of bounds"
    | y < 0 || y >= my = error "writeTexture2DFromBuffer, y out of bounds"
    | h < 0 || y+h > my = error "writeTexture2DFromBuffer, h out of bounds"
    | i < 0 || i > bufferLength b = error "writeTexture2DFromBuffer, i out of bounds"
    | bufferLength b - i < w*h = error "writeTexture2DFromBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do 
                    useTexSync texn GL_TEXTURE_2D
                    bname <- readIORef $ bufName b
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER bname
                    glTexSubImage2D GL_TEXTURE_2D (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)        
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER 0
    where V2 mx my = texture2DSizes t !! l                                                                                
writeTexture2DArrayFromBuffer t@(Texture2DArray texn _ ml) l (V3 x y z) (V3 w h d) b i 
    | l < 0 || l >= ml = error "writeTexture2DArrayFromBuffer, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture2DArrayFromBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture2DArrayFromBuffer, w out of bounds"
    | y < 0 || y >= my = error "writeTexture2DArrayFromBuffer, y out of bounds"
    | h < 0 || y+h > my = error "writeTexture2DArrayFromBuffer, h out of bounds"
    | z < 0 || z >= mz = error "writeTexture2DArrayFromBuffer, z out of bounds"
    | d < 0 || z+d > mz = error "writeTexture2DArrayFromBuffer, d out of bounds"
    | i < 0 || i > bufferLength b = error "writeTexture2DArrayFromBuffer, i out of bounds"
    | bufferLength b - i < w*h = error "writeTexture2DArrayFromBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do 
                    useTexSync texn GL_TEXTURE_2D_ARRAY
                    bname <- readIORef $ bufName b
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER bname
                    glTexSubImage3D GL_TEXTURE_2D_ARRAY (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w) (fromIntegral h) (fromIntegral d) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)        
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER 0
    where V3 mx my mz = texture2DArraySizes t !! l    
writeTexture3DFromBuffer t@(Texture3D texn _ ml) l (V3 x y z) (V3 w h d) b i 
    | l < 0 || l >= ml = error "writeTexture3DFromBuffer, level out of bounds"
    | x < 0 || x >= mx = error "writeTexture3DFromBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "writeTexture3DFromBuffer, w out of bounds"
    | y < 0 || y >= my = error "writeTexture3DFromBuffer, y out of bounds"
    | h < 0 || y+h > my = error "writeTexture3DFromBuffer, h out of bounds"
    | z < 0 || z >= mz = error "writeTexture3DFromBuffer, z out of bounds"
    | d < 0 || z+d > mz = error "writeTexture3DFromBuffer, d out of bounds"
    | i < 0 || i > bufferLength b = error "writeTexture3DFromBuffer, i out of bounds"
    | bufferLength b - i < w*h = error "writeTexture3DFromBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do 
                    useTexSync texn GL_TEXTURE_3D
                    bname <- readIORef $ bufName b
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER bname
                    glTexSubImage3D GL_TEXTURE_3D (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w) (fromIntegral h) (fromIntegral d) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)        
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER 0
    where V3 mx my mz = texture3DSizes t !! l    
writeTextureCubeFromBuffer t@(TextureCube texn _ ml) l s (V2 x y) (V2 w h) b i 
    | l < 0 || l >= ml = error "writeTextureCubeFromBuffer, level out of bounds"
    | x < 0 || x >= mxy = error "writeTextureCubeFromBuffer, x out of bounds"
    | w < 0 || x+w > mxy = error "writeTextureCubeFromBuffer, w out of bounds"
    | y < 0 || y >= mxy = error "writeTextureCubeFromBuffer, y out of bounds"
    | h < 0 || y+h > mxy = error "writeTextureCubeFromBuffer, h out of bounds"
    | i < 0 || i > bufferLength b = error "writeTextureCubeFromBuffer, i out of bounds"
    | bufferLength b - i < w*h = error "writeTextureCubeFromBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do 
                    useTexSync texn GL_TEXTURE_CUBE_MAP
                    bname <- readIORef $ bufName b
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER bname
                    glTexSubImage2D (getGlCubeSide s) (fromIntegral l) (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)        
                    glBindBuffer GL_PIXEL_UNPACK_BUFFER 0
    where mxy = textureCubeSizes t !! l           

        
readTexture1D t@(Texture1D texn _ ml) l x w f s 
    | l < 0 || l >= ml = error "readTexture1DArray, level out of bounds"
    | x < 0 || x >= mx = error "readTexture1DArray, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture1DArray, w out of bounds"
    | otherwise =  
                 let b = makeBuffer undefined undefined 0 :: Buffer os b
                     f' ptr a off = f a =<< liftIO (peekPixel (undefined :: b) (ptr `plusPtr` off))
                 in bracket
                   (liftContextIO $ do
                     ptr <- mallocBytes $ w*bufElementSize b 
                     setGlPixelStoreRange x 0 0 w 1
                     useTexSync texn GL_TEXTURE_1D_ARRAY
                     glGetTexImage GL_TEXTURE_1D_ARRAY (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
                     return ptr)
                   (liftIO . free)
                   (\ptr -> foldM (f' ptr) s [0,bufElementSize b..w*bufElementSize b -1])
    where mx = texture1DSizes t !! l                                             
readTexture1DArray t@(Texture1DArray texn _ ml) l (V2 x y) w f s 
    | l < 0 || l >= ml = error "readTexture1DArray, level out of bounds"
    | x < 0 || x >= mx = error "readTexture1DArray, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture1DArray, w out of bounds"
    | y < 0 || y >= my = error "readTexture1DArray, y out of bounds"
    | otherwise =  
                 let b = makeBuffer undefined undefined 0 :: Buffer os b
                     f' ptr a off = f a =<< liftIO (peekPixel (undefined :: b) (ptr `plusPtr` off))
                 in bracket
                   (liftContextIO $ do
                     ptr <- mallocBytes $ w*bufElementSize b 
                     setGlPixelStoreRange x y 0 w 1
                     useTexSync texn GL_TEXTURE_1D_ARRAY
                     glGetTexImage GL_TEXTURE_1D_ARRAY (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
                     return ptr)
                   (liftIO . free)
                   (\ptr -> foldM (f' ptr) s [0,bufElementSize b..w*bufElementSize b -1])
    where V2 mx my = texture1DArraySizes t !! l                                             
readTexture2D t@(Texture2D texn _ ml) l (V2 x y) (V2 w h) f s 
    | l < 0 || l >= ml = error "readTexture2D, level out of bounds"
    | x < 0 || x >= mx = error "readTexture2D, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture2D, w out of bounds"
    | y < 0 || y >= my = error "readTexture2D, y out of bounds"
    | h < 0 || y+h > my = error "readTexture2D, h out of bounds"
    | otherwise =  
                 let b = makeBuffer undefined undefined 0 :: Buffer os b
                     f' ptr a off = f a =<< liftIO (peekPixel (undefined :: b) (ptr `plusPtr` off))
                 in bracket
                   (liftContextIO $ do
                     ptr <- mallocBytes $ w*h*bufElementSize b 
                     setGlPixelStoreRange x y 0 w h
                     useTexSync texn GL_TEXTURE_2D
                     glGetTexImage GL_TEXTURE_2D (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
                     return ptr)
                   (liftIO . free)
                   (\ptr -> foldM (f' ptr) s [0,bufElementSize b..w*h*bufElementSize b -1])
    where V2 mx my = texture2DSizes t !! l                                                                                
readTexture2DArray t@(Texture2DArray texn _ ml) l (V3 x y z) (V2 w h) f s 
    | l < 0 || l >= ml = error "readTexture2DArray, level out of bounds"
    | x < 0 || x >= mx = error "readTexture2DArray, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture2DArray, w out of bounds"
    | y < 0 || y >= my = error "readTexture2DArray, y out of bounds"
    | h < 0 || y+h > my = error "readTexture2DArray, h out of bounds"
    | z < 0 || z >= mz = error "readTexture2DArray, y out of bounds"
    | otherwise =  
                 let b = makeBuffer undefined undefined 0 :: Buffer os b
                     f' ptr a off = f a =<< liftIO (peekPixel (undefined :: b) (ptr `plusPtr` off))
                 in bracket
                   (liftContextIO $ do
                     ptr <- mallocBytes $ w*h*bufElementSize b 
                     setGlPixelStoreRange x y z w h
                     useTexSync texn GL_TEXTURE_2D_ARRAY
                     glGetTexImage GL_TEXTURE_2D_ARRAY (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
                     return ptr)
                   (liftIO . free)
                   (\ptr -> foldM (f' ptr) s [0,bufElementSize b..w*h*bufElementSize b -1])
    where V3 mx my mz = texture2DArraySizes t !! l   
readTexture3D t@(Texture3D texn _ ml) l (V3 x y z) (V2 w h) f s 
    | l < 0 || l >= ml = error "readTexture3D, level out of bounds"
    | x < 0 || x >= mx = error "readTexture3D, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture3D, w out of bounds"
    | y < 0 || y >= my = error "readTexture3D, y out of bounds"
    | h < 0 || y+h > my = error "readTexture3D, h out of bounds"
    | z < 0 || z >= mz = error "readTexture3D, y out of bounds"
    | otherwise =  
                 let b = makeBuffer undefined undefined 0 :: Buffer os b
                     f' ptr a off = f a =<< liftIO (peekPixel (undefined :: b) (ptr `plusPtr` off))
                 in bracket
                   (liftContextIO $ do
                     ptr <- mallocBytes $ w*h*bufElementSize b 
                     setGlPixelStoreRange x y z w h
                     useTexSync texn GL_TEXTURE_3D
                     glGetTexImage GL_TEXTURE_3D (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
                     return ptr)
                   (liftIO . free)
                   (\ptr -> foldM (f' ptr) s [0,bufElementSize b..w*h*bufElementSize b -1])
    where V3 mx my mz = texture3DSizes t !! l
readTextureCube t@(TextureCube texn _ ml) l si (V2 x y) (V2 w h) f s 
    | l < 0 || l >= ml = error "readTextureCube, level out of bounds"
    | x < 0 || x >= mxy = error "readTextureCube, x out of bounds"
    | w < 0 || x+w > mxy = error "readTextureCube, w out of bounds"
    | y < 0 || y >= mxy = error "readTextureCube, y out of bounds"
    | h < 0 || y+h > mxy = error "readTextureCube, h out of bounds"
    | otherwise =  
                 let b = makeBuffer undefined undefined 0 :: Buffer os b
                     f' ptr a off = f a =<< liftIO (peekPixel (undefined :: b) (ptr `plusPtr` off))
                 in bracket
                   (liftContextIO $ do
                     ptr <- mallocBytes $ w*h*bufElementSize b 
                     setGlPixelStoreRange x y 0 w h
                     useTexSync texn GL_TEXTURE_CUBE_MAP
                     glGetTexImage (getGlCubeSide si) (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) ptr
                     return ptr)
                   (liftIO . free)
                   (\ptr -> foldM (f' ptr) s [0,bufElementSize b..w*h*bufElementSize b -1])
    where mxy = textureCubeSizes t !! l

readTexture1DToBuffer t@(Texture1D texn _ ml) l x w b i
    | l < 0 || l >= ml = error "readTexture1DToBuffer, level out of bounds"
    | x < 0 || x >= mx = error "readTexture1DToBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture1DToBuffer, w out of bounds"
    | i < 0 || i > bufferLength b = error "readTexture1DToBuffer, i out of bounds"
    | bufferLength b - i < w = error "readTexture1DToBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do
                     bname <- readIORef $ bufName b
                     glBindBuffer GL_PIXEL_PACK_BUFFER bname
                     setGlPixelStoreRange x 0 0 w 1
                     useTexSync texn GL_TEXTURE_1D
                     glGetTexImage GL_TEXTURE_1D (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)
                     glBindBuffer GL_PIXEL_PACK_BUFFER 0
    where mx = texture1DSizes t !! l
readTexture1DArrayToBuffer t@(Texture1DArray texn _ ml) l (V2 x y) w b i 
    | l < 0 || l >= ml = error "readTexture1DArrayToBuffer, level out of bounds"
    | x < 0 || x >= mx = error "readTexture1DArrayToBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture1DArrayToBuffer, w out of bounds"
    | y < 0 || y >= my = error "readTexture1DArrayToBuffer, y out of bounds"
    | i < 0 || i > bufferLength b = error "readTexture1DArrayToBuffer, i out of bounds"
    | bufferLength b - i < w = error "readTexture1DArrayToBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do
                     bname <- readIORef $ bufName b
                     glBindBuffer GL_PIXEL_PACK_BUFFER bname
                     setGlPixelStoreRange x y 0 w 1
                     useTexSync texn GL_TEXTURE_1D_ARRAY
                     glGetTexImage GL_TEXTURE_1D_ARRAY (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)
                     glBindBuffer GL_PIXEL_PACK_BUFFER 0
    where V2 mx my = texture1DArraySizes t !! l
readTexture2DToBuffer t@(Texture2D texn _ ml) l (V2 x y) (V2 w h) b i 
    | l < 0 || l >= ml = error "readTexture2DToBuffer, level out of bounds"
    | x < 0 || x >= mx = error "readTexture2DToBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture2DToBuffer, w out of bounds"
    | y < 0 || y >= my = error "readTexture2DToBuffer, y out of bounds"
    | h < 0 || y+h > my = error "readTexture2DToBuffer, h out of bounds"
    | i < 0 || i > bufferLength b = error "readTexture2DToBuffer, i out of bounds"
    | bufferLength b - i < w*h = error "readTexture2DToBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do
                     bname <- readIORef $ bufName b
                     glBindBuffer GL_PIXEL_PACK_BUFFER bname
                     setGlPixelStoreRange x y 0 w h
                     useTexSync texn GL_TEXTURE_2D
                     glGetTexImage GL_TEXTURE_2D (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)
                     glBindBuffer GL_PIXEL_PACK_BUFFER 0
    where V2 mx my = texture2DSizes t !! l                                                                                
readTexture2DArrayToBuffer t@(Texture2DArray texn _ ml) l (V3 x y z) (V2 w h) b i 
    | l < 0 || l >= ml = error "readTexture2DArrayToBuffer, level out of bounds"
    | x < 0 || x >= mx = error "readTexture2DArrayToBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture2DArrayToBuffer, w out of bounds"
    | y < 0 || y >= my = error "readTexture2DArrayToBuffer, y out of bounds"
    | h < 0 || y+h > my = error "readTexture2DArrayToBuffer, h out of bounds"
    | z < 0 || z >= mz = error "readTexture2DArrayToBuffer, z out of bounds"
    | i < 0 || i > bufferLength b = error "readTexture2DArrayToBuffer, i out of bounds"
    | bufferLength b - i < w*h = error "readTexture2DArrayToBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do
                     bname <- readIORef $ bufName b
                     glBindBuffer GL_PIXEL_PACK_BUFFER bname
                     setGlPixelStoreRange x y z w h
                     useTexSync texn GL_TEXTURE_2D_ARRAY
                     glGetTexImage GL_TEXTURE_2D_ARRAY (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)
                     glBindBuffer GL_PIXEL_PACK_BUFFER 0
    where V3 mx my mz = texture2DArraySizes t !! l
readTexture3DToBuffer t@(Texture3D texn _ ml) l (V3 x y z) (V2 w h) b i 
    | l < 0 || l >= ml = error "readTexture3DToBuffer, level out of bounds"
    | x < 0 || x >= mx = error "readTexture3DToBuffer, x out of bounds"
    | w < 0 || x+w > mx = error "readTexture3DToBuffer, w out of bounds"
    | y < 0 || y >= my = error "readTexture3DToBuffer, y out of bounds"
    | h < 0 || y+h > my = error "readTexture3DToBuffer, h out of bounds"
    | z < 0 || z >= mz = error "readTexture3DToBuffer, z out of bounds"
    | i < 0 || i > bufferLength b = error "readTexture3DToBuffer, i out of bounds"
    | bufferLength b - i < w*h = error "readTexture3DToBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do
                     bname <- readIORef $ bufName b
                     glBindBuffer GL_PIXEL_PACK_BUFFER bname
                     setGlPixelStoreRange x y z w h
                     useTexSync texn GL_TEXTURE_3D
                     glGetTexImage GL_TEXTURE_3D (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)
                     glBindBuffer GL_PIXEL_PACK_BUFFER 0
    where V3 mx my mz = texture3DSizes t !! l
readTextureCubeToBuffer t@(TextureCube texn _ ml) l s (V2 x y) (V2 w h) b i 
    | l < 0 || l >= ml = error "readTextureCubeToBuffer, level out of bounds"
    | x < 0 || x >= mxy = error "readTextureCubeToBuffer, x out of bounds"
    | w < 0 || x+w > mxy = error "readTextureCubeToBuffer, w out of bounds"
    | y < 0 || y >= mxy = error "readTextureCubeToBuffer, y out of bounds"
    | h < 0 || y+h > mxy = error "readTextureCubeToBuffer, h out of bounds"
    | i < 0 || i > bufferLength b = error "readTextureCubeToBuffer, i out of bounds"
    | bufferLength b - i < w*h = error "readTextureCubeToBuffer, buffer data too small"
    | otherwise = liftContextIOAsync $ do
                     setGlPixelStoreRange x y 0 w h
                     bname <- readIORef $ bufName b
                     glBindBuffer GL_PIXEL_PACK_BUFFER bname
                     useTexSync texn GL_TEXTURE_CUBE_MAP
                     glGetTexImage (getGlCubeSide s) (fromIntegral l) (getGlColorFormat (undefined :: c) (undefined :: b)) (getGlType (undefined :: b)) (wordPtrToPtr $ fromIntegral $ i*bufElementSize b)
                     glBindBuffer GL_PIXEL_PACK_BUFFER 0
    where mxy = textureCubeSizes t !! l                                                                                

setGlPixelStoreRange :: Int -> Int -> Int -> Int -> Int -> IO ()
setGlPixelStoreRange x y z w h = do
        glPixelStorei GL_PACK_SKIP_PIXELS $ fromIntegral x
        glPixelStorei GL_PACK_SKIP_ROWS $ fromIntegral y
        glPixelStorei GL_PACK_SKIP_IMAGES $ fromIntegral z
        glPixelStorei GL_PACK_ROW_LENGTH $ fromIntegral w
        glPixelStorei GL_PACK_IMAGE_HEIGHT $ fromIntegral h


generateTexture1DMipmap :: MonadIO m => Texture1D os f -> ContextT w os f' m ()
generateTexture1DArrayMipmap :: MonadIO m => Texture1DArray os f -> ContextT w os f' m ()
generateTexture2DMipmap :: MonadIO m => Texture2D os f -> ContextT w os f' m ()
generateTexture2DArrayMipmap :: MonadIO m => Texture2DArray os f -> ContextT w os f' m ()
generateTexture3DMipmap :: MonadIO m => Texture3D os f -> ContextT w os f' m ()
generateTextureCubeMipmap :: MonadIO m => TextureCube os f -> ContextT w os f' m ()

genMips texn target = liftContextIOAsync $ do
                     useTexSync texn target
                     glGenerateMipmap target

generateTexture1DMipmap (Texture1D texn _ _) = genMips texn GL_TEXTURE_1D
generateTexture1DArrayMipmap (Texture1DArray texn _ _) = genMips texn GL_TEXTURE_1D_ARRAY
generateTexture2DMipmap (Texture2D texn _ _) = genMips texn GL_TEXTURE_2D
generateTexture2DMipmap _ = return () -- Only one level for renderbuffers
generateTexture2DArrayMipmap (Texture2DArray texn _ _) = genMips texn GL_TEXTURE_2D_ARRAY
generateTexture3DMipmap (Texture3D texn _ _) = genMips texn GL_TEXTURE_3D
generateTextureCubeMipmap (TextureCube texn _ _) = genMips texn GL_TEXTURE_CUBE_MAP
       
----------------------------------------------------------------------
-- Samplers

data Filter = Nearest | Linear  deriving (Eq, Enum)
data EdgeMode = Repeat | Mirror | ClampToEdge | ClampToBorder deriving (Eq, Enum)
type BorderColor c = Color c (ColorElement c) 
type Anisotropy = Maybe Float

type MinFilter = Filter
type MagFilter = Filter
type LodFilter = Filter

-- | A GADT for sample filters, where 'SamplerFilter' cannot be used for integer textures. 
data SamplerFilter c where
    SamplerFilter :: (ColorElement c ~ Float) => MagFilter -> MinFilter -> LodFilter -> Anisotropy -> SamplerFilter c 
    SamplerNearest :: SamplerFilter c

type EdgeMode2 = V2 EdgeMode
type EdgeMode3 = V3 EdgeMode

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

getGlCompFunc :: (Num a, Eq a) => ComparisonFunction -> a
getGlCompFunc Never = GL_NEVER
getGlCompFunc Less = GL_LESS
getGlCompFunc Equal = GL_EQUAL
getGlCompFunc Lequal = GL_LEQUAL
getGlCompFunc Greater = GL_GREATER
getGlCompFunc Notequal = GL_NOTEQUAL
getGlCompFunc Gequal = GL_GEQUAL
getGlCompFunc Always = GL_ALWAYS
   
newSampler1D :: forall os f s c. ColorSampleable c => (s -> (Texture1D os (Format c), SamplerFilter c, (EdgeMode,  BorderColor c))) -> Shader os f s (Sampler1D (Format c))
newSampler1DArray :: forall os f s c. ColorSampleable c => (s -> (Texture1DArray os (Format c), SamplerFilter c, (EdgeMode, BorderColor c))) -> Shader os f s (Sampler1DArray (Format c))
newSampler2D :: forall os f s c. ColorSampleable c => (s -> (Texture2D os (Format c), SamplerFilter c, (EdgeMode2, BorderColor c))) -> Shader os f s (Sampler2D (Format c))
newSampler2DArray :: forall os f s c. ColorSampleable c => (s -> (Texture2DArray os (Format c), SamplerFilter c, (EdgeMode2, BorderColor c))) -> Shader os f s (Sampler2DArray (Format c))
newSampler3D :: forall os f s c. ColorRenderable c => (s -> (Texture3D os (Format c), SamplerFilter c, (EdgeMode3, BorderColor c))) -> Shader os f s (Sampler3D (Format c))
newSamplerCube :: forall os f s c. ColorSampleable c => (s -> (TextureCube os (Format c), SamplerFilter c)) -> Shader os f s (SamplerCube (Format c))

newSampler1DShadow :: forall os f s d. DepthRenderable d => (s -> (Texture1D os (Format d), SamplerFilter d, (EdgeMode, BorderColor d), ComparisonFunction)) -> Shader os f s (Sampler1D Shadow)
newSampler1DArrayShadow :: forall os f s d. DepthRenderable d => (s -> (Texture1DArray os (Format d), SamplerFilter d, (EdgeMode, BorderColor d), ComparisonFunction)) -> Shader os f s (Sampler1DArray Shadow)
newSampler2DShadow :: forall os f s d. DepthRenderable d => (s -> (Texture2D os d, SamplerFilter (Format d), (EdgeMode2, BorderColor d), ComparisonFunction)) -> Shader os f s (Sampler2D Shadow)
newSampler2DArrayShadow :: forall os f s d. DepthRenderable d => (s -> (Texture2DArray os (Format d), SamplerFilter d, (EdgeMode2, BorderColor d), ComparisonFunction)) -> Shader os f s (Sampler2DArray Shadow)
newSamplerCubeShadow :: forall os f s d. DepthRenderable d => (s -> (TextureCube os (Format d), SamplerFilter d, ComparisonFunction)) -> Shader os f s (SamplerCube Shadow)

newSampler1D sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture1D tn _ _, filt, (ex, ec)) = sf s
                                                    in  do n <- useTex tn GL_TEXTURE_1D bind
                                                           setNoShadowMode GL_TEXTURE_1D                                       
                                                           setSamplerFilter GL_TEXTURE_1D filt
                                                           setEdgeMode GL_TEXTURE_1D (Just ex, Nothing, Nothing) (setBorderColor (undefined :: c) GL_TEXTURE_1D ec)
                                                           return n
                   return $ Sampler1D sampId False (samplerPrefix (undefined :: c))
newSampler1DArray sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture1DArray tn _ _, filt, (ex, ec)) = sf s 
                                                    in  do n <- useTex tn GL_TEXTURE_1D_ARRAY bind
                                                           setNoShadowMode GL_TEXTURE_1D_ARRAY                                       
                                                           setSamplerFilter GL_TEXTURE_1D_ARRAY filt
                                                           setEdgeMode GL_TEXTURE_1D_ARRAY (Just ex, Nothing, Nothing) (setBorderColor (undefined :: c) GL_TEXTURE_1D_ARRAY ec)
                                                           return n
                   return $ Sampler1DArray sampId False (samplerPrefix (undefined :: c))
newSampler2D sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture2D tn _ _, filt, (V2 ex ey, ec)) = sf s 
                                                    in  do n <- useTex tn GL_TEXTURE_2D bind
                                                           setNoShadowMode GL_TEXTURE_2D                                      
                                                           setSamplerFilter GL_TEXTURE_2D filt
                                                           setEdgeMode GL_TEXTURE_2D (Just ex, Just ey, Nothing) (setBorderColor (undefined :: c) GL_TEXTURE_2D ec)
                                                           return n
                   return $ Sampler2D sampId False (samplerPrefix (undefined :: c))
newSampler2DArray sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture2DArray tn _ _, filt, (V2 ex ey, ec)) = sf s 
                                                    in  do n <- useTex tn GL_TEXTURE_2D_ARRAY bind
                                                           setNoShadowMode GL_TEXTURE_2D_ARRAY                                       
                                                           setSamplerFilter GL_TEXTURE_2D_ARRAY filt
                                                           setEdgeMode GL_TEXTURE_2D_ARRAY (Just ex, Just ey, Nothing) (setBorderColor (undefined :: c) GL_TEXTURE_2D_ARRAY ec)
                                                           return n
                   return $ Sampler2DArray sampId False (samplerPrefix (undefined :: c))
newSampler3D sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture3D tn _ _, filt, (V3 ex ey ez, ec)) = sf s 
                                                    in  do n <- useTex tn GL_TEXTURE_3D bind
                                                           setNoShadowMode GL_TEXTURE_3D                                       
                                                           setSamplerFilter GL_TEXTURE_3D filt
                                                           setEdgeMode GL_TEXTURE_3D (Just ex, Just ey, Just ez) (setBorderColor (undefined :: c) GL_TEXTURE_3D ec)
                                                           return n
                   return $ Sampler3D sampId False (samplerPrefix (undefined :: c))
newSamplerCube sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (TextureCube tn _ _, filt) = sf s 
                                                    in  do n <- useTex tn GL_TEXTURE_CUBE_MAP bind
                                                           setNoShadowMode GL_TEXTURE_CUBE_MAP                                       
                                                           setSamplerFilter GL_TEXTURE_CUBE_MAP filt
                                                           return n
                   return $ SamplerCube sampId False (samplerPrefix (undefined :: c))


newSampler1DShadow sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture1D tn _ _, filt, (ex, ec), cf) = sf s
                                                    in  do n <- useTex tn GL_TEXTURE_1D bind
                                                           setShadowFunc GL_TEXTURE_1D cf                                     
                                                           setSamplerFilter GL_TEXTURE_1D filt
                                                           setEdgeMode GL_TEXTURE_1D (Just ex, Nothing, Nothing) (setBorderColor (undefined :: d) GL_TEXTURE_1D ec)
                                                           return n
                   return $ Sampler1D sampId True ""
newSampler1DArrayShadow sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture1DArray tn _ _, filt, (ex, ec), cf) = sf s 
                                                    in  do n <- useTex tn GL_TEXTURE_1D_ARRAY bind
                                                           setShadowFunc GL_TEXTURE_1D_ARRAY cf                                       
                                                           setSamplerFilter GL_TEXTURE_1D_ARRAY filt
                                                           setEdgeMode GL_TEXTURE_1D_ARRAY (Just ex, Nothing, Nothing) (setBorderColor (undefined :: d) GL_TEXTURE_1D_ARRAY ec)
                                                           return n
                   return $ Sampler1DArray sampId True ""
newSampler2DShadow sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture2D tn _ _, filt, (V2 ex ey, ec), cf) = sf s 
                                                    in  do n <- useTex tn GL_TEXTURE_2D bind
                                                           setShadowFunc GL_TEXTURE_2D cf                                      
                                                           setSamplerFilter GL_TEXTURE_2D filt
                                                           setEdgeMode GL_TEXTURE_2D (Just ex, Just ey, Nothing) (setBorderColor (undefined :: d) GL_TEXTURE_2D ec)
                                                           return n
                   return $ Sampler2D sampId True ""
newSampler2DArrayShadow sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (Texture2DArray tn _ _, filt, (V2 ex ey, ec), cf) = sf s 
                                                    in  do n <- useTex tn GL_TEXTURE_2D_ARRAY bind
                                                           setShadowFunc GL_TEXTURE_2D_ARRAY cf                                       
                                                           setSamplerFilter GL_TEXTURE_2D_ARRAY filt
                                                           setEdgeMode GL_TEXTURE_2D_ARRAY (Just ex, Just ey, Nothing) (setBorderColor (undefined :: d) GL_TEXTURE_2D_ARRAY ec)
                                                           return n
                   return $ Sampler2DArray sampId True ""
newSamplerCubeShadow sf = Shader $ do 
                   sampId <- getName
                   doForSampler sampId $ \s bind -> let (TextureCube tn _ _, filt, cf) = sf s 
                                                    in  do n <- useTex tn GL_TEXTURE_CUBE_MAP bind
                                                           setShadowFunc GL_TEXTURE_CUBE_MAP cf                                       
                                                           setSamplerFilter GL_TEXTURE_CUBE_MAP filt
                                                           return n
                   return $ SamplerCube sampId True ""

setNoShadowMode :: GLenum -> IO ()
setNoShadowMode t = glTexParameteri t GL_TEXTURE_COMPARE_MODE GL_NONE

setShadowFunc :: GLenum -> ComparisonFunction -> IO ()
setShadowFunc t cf = do 
    glTexParameteri t GL_TEXTURE_COMPARE_MODE GL_COMPARE_REF_TO_TEXTURE 
    glTexParameteri t GL_TEXTURE_COMPARE_FUNC (getGlCompFunc cf) 
                                                           
setEdgeMode :: GLenum -> (Maybe EdgeMode, Maybe EdgeMode, Maybe EdgeMode) -> IO () -> IO ()
setEdgeMode t (se,te,re) bcio = do glwrap GL_TEXTURE_WRAP_S se
                                   glwrap GL_TEXTURE_WRAP_T te
                                   glwrap GL_TEXTURE_WRAP_R re
                                   when (se == Just ClampToBorder || te == Just ClampToBorder || re == Just ClampToBorder) 
                                      bcio
    where glwrap _ Nothing = return ()
          glwrap x (Just Repeat) = glTexParameteri t x GL_REPEAT
          glwrap x (Just Mirror) = glTexParameteri t x GL_MIRRORED_REPEAT
          glwrap x (Just ClampToEdge) = glTexParameteri t x GL_CLAMP_TO_EDGE
          glwrap x (Just ClampToBorder) = glTexParameteri t x GL_CLAMP_TO_BORDER
          
setSamplerFilter :: GLenum -> SamplerFilter a -> IO ()
setSamplerFilter t (SamplerFilter magf minf lodf a) = setSamplerFilter' t magf minf lodf a
setSamplerFilter t SamplerNearest = setSamplerFilter' t Nearest Nearest Nearest Nothing

setSamplerFilter' :: GLenum -> MagFilter -> MinFilter -> LodFilter -> Anisotropy -> IO () 
setSamplerFilter' t magf minf lodf a = do
                                           glTexParameteri t GL_TEXTURE_MIN_FILTER glmin
                                           glTexParameteri t GL_TEXTURE_MAG_FILTER glmag
                                           case a of
                                                Nothing -> return ()
                                                Just a' -> glTexParameterf t GL_TEXTURE_MAX_ANISOTROPY_EXT (realToFrac a')
    where glmin = case (minf, lodf) of
                    (Nearest, Nearest) -> GL_NEAREST_MIPMAP_NEAREST
                    (Linear, Nearest) -> GL_LINEAR_MIPMAP_NEAREST
                    (Nearest, Linear) -> GL_NEAREST_MIPMAP_LINEAR                                                        
                    (Linear, Linear) -> GL_LINEAR_MIPMAP_LINEAR
          glmag = case magf of
                    Nearest -> GL_NEAREST                                
                    Linear -> GL_LINEAR
          



doForSampler :: Int -> (s -> Binding -> IO Int) -> ShaderM s ()
doForSampler n io = modifyRenderIO (\s -> s { samplerNameToRenderIO = insert n io (samplerNameToRenderIO s) } )

-- | Used instead of 'Format' for shadow samplers. These samplers have specialized sampler values, see 'sample1DShadow' and friends.
data Shadow
data Sampler1D f = Sampler1D Int Bool String
data Sampler1DArray f = Sampler1DArray Int Bool String
data Sampler2D f = Sampler2D Int Bool String
data Sampler2DArray f = Sampler2DArray Int Bool String
data Sampler3D f = Sampler3D Int Bool String
data SamplerCube f = SamplerCube Int Bool String

-- | A GADT to specify where the level of detail and/or partial derivates should be taken from. Some values of this GADT are restricted to
--   only 'FragmentStream's.
data SampleLod vx x where
    SampleAuto :: SampleLod v F
    SampleBias :: FFloat -> SampleLod vx F   
    SampleLod :: S x Float -> SampleLod vx x
    SampleGrad :: vx -> vx -> SampleLod vx x

-- | For some reason, OpenGl doesnt allow explicit lod to be specified for some sampler types, hence this extra GADT. 
data SampleLod' vx x where
    SampleAuto' :: SampleLod' v F
    SampleBias' :: FFloat -> SampleLod' vx F   
    SampleGrad' :: vx -> vx -> SampleLod' vx x

type SampleLod1 x = SampleLod (S x Float) x
type SampleLod2 x = SampleLod (V2 (S x Float)) x
type SampleLod3 x = SampleLod (V3 (S x Float)) x
type SampleLod2' x = SampleLod' (V2 (S x Float)) x
type SampleLod3' x = SampleLod' (V3 (S x Float)) x

fromLod' :: SampleLod' v x -> SampleLod v x
fromLod' SampleAuto' = SampleAuto
fromLod' (SampleBias' x) = SampleBias x
fromLod' (SampleGrad' x y) = SampleGrad x y

type SampleProj x = Maybe (S x Float)
type SampleOffset1 x = Maybe Int 
type SampleOffset2 x = Maybe (V2 Int) 
type SampleOffset3 x = Maybe (V3 Int)

-- | The type of a color sample made by a texture t 
type ColorSample x f = Color f (S x (ColorElement f))
type ReferenceValue x = S x Float 

sample1D            :: forall c x. ColorSampleable c =>  Sampler1D (Format c)          -> SampleLod1 x -> SampleProj x -> SampleOffset1 x -> S x Float -> ColorSample x c
sample1DArray       :: forall c x. ColorSampleable c =>  Sampler1DArray (Format c)     -> SampleLod1 x -> SampleOffset1 x -> V2 (S x Float) -> ColorSample x c
sample2D            :: forall c x. ColorSampleable c =>  Sampler2D (Format c)          -> SampleLod2 x -> SampleProj x -> SampleOffset2 x -> V2 (S x Float) -> ColorSample x c
sample2DArray       :: forall c x. ColorSampleable c =>  Sampler2DArray (Format c)     -> SampleLod2 x -> SampleOffset2 x -> V3 (S x Float) -> ColorSample x c
sample3D            :: forall c x. ColorSampleable c =>  Sampler3D (Format c)          -> SampleLod3 x -> SampleProj x -> SampleOffset3 x -> V3 (S x Float) -> ColorSample x c
sampleCube          :: forall c x. ColorSampleable c =>  SamplerCube (Format c)        -> SampleLod3 x -> V3 (S x Float) -> ColorSample x c

sample1DShadow      :: forall x. Sampler1D Shadow     -> SampleLod1 x -> SampleProj x -> SampleOffset1 x -> ReferenceValue x -> S x Float -> S x Float
sample1DArrayShadow :: forall x. Sampler1DArray Shadow-> SampleLod1 x -> SampleOffset1 x -> ReferenceValue x -> V2 (S x Float) -> S x Float
sample2DShadow      :: forall x. Sampler2D Shadow     -> SampleLod2 x -> SampleProj x -> SampleOffset2 x -> ReferenceValue x -> V2 (S x Float) -> S x Float
sample2DArrayShadow :: forall x. Sampler2DArray Shadow-> SampleLod2' x -> SampleOffset2 x -> ReferenceValue x -> V3 (S x Float)-> S x Float
sampleCubeShadow    :: forall x. SamplerCube Shadow   -> SampleLod3' x -> ReferenceValue x -> V3 (S x Float) -> S x Float

sample1D (Sampler1D sampId _ prefix) lod proj off coord = toColor (undefined :: c) $ sample (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "1D" sampId lod proj off coord v1toF v1toF civ1toF pv1toF
sample1DArray (Sampler1DArray sampId _ prefix) lod off coord = toColor (undefined :: c) $ sample (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "1DArray" sampId lod Nothing off coord v2toF v1toF civ1toF undefined
sample2D (Sampler2D sampId _ prefix) lod proj off coord = toColor (undefined :: c) $ sample (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "2D" sampId lod proj off coord v2toF v2toF civ2toF pv2toF 
sample2DArray (Sampler2DArray sampId _ prefix) lod off coord = toColor (undefined :: c) $ sample (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "2DArray" sampId lod Nothing off coord v3toF v2toF civ2toF undefined
sample3D (Sampler3D sampId _ prefix) lod proj off coord = toColor (undefined :: c) $ sample (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "3D" sampId lod proj off coord v3toF v3toF civ3toF pv3toF
sampleCube (SamplerCube sampId _ prefix) lod coord = toColor (undefined :: c) $ sample (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "Cube" sampId lod Nothing Nothing coord v3toF v3toF undefined undefined

sample1DShadow (Sampler1D sampId _ _) lod proj off ref coord = sampleShadow "1D" sampId lod proj off (t1t3 coord ref) v3toF v1toF civ1toF pv3toF
sample1DArrayShadow (Sampler1DArray sampId _ _) lod off ref coord = sampleShadow "1DArray" sampId lod Nothing off (t2t3 coord ref) v3toF v1toF civ1toF undefined
sample2DShadow (Sampler2D sampId _ _) lod proj off ref coord = sampleShadow "2D" sampId lod proj off (t2t3 coord ref) v3toF v2toF civ2toF pv3toF
sample2DArrayShadow (Sampler2DArray sampId _ _) lod off ref coord = sampleShadow "2DArray" sampId (fromLod' lod) Nothing off (t3t4 coord ref) v4toF v2toF civ2toF undefined
sampleCubeShadow (SamplerCube sampId _ _) lod ref coord = sampleShadow "Cube" sampId (fromLod' lod) Nothing Nothing (t3t4 coord ref) v4toF v3toF undefined undefined

t1t3 :: S x Float -> S x Float -> V3 (S x Float)
t2t3 :: V2 t -> t -> V3 t
t3t4 :: V3 t -> t -> V4 t
t1t3 x = V3 x 0 
t2t3 (V2 x y) = V3 x y 
t3t4 (V3 x y z) = V4 x y z

texelFetch1D        :: forall c x. ColorSampleable c =>  Sampler1D (Format c)          -> SampleOffset1 x -> S x Level -> S x Int -> ColorSample x c
texelFetch1DArray   :: forall c x. ColorSampleable c =>  Sampler1DArray (Format c)     -> SampleOffset1 x -> S x Level -> V2(S x Int) -> ColorSample x c
texelFetch2D        :: forall c x. ColorSampleable c =>  Sampler2D (Format c)          -> SampleOffset2 x -> S x Level -> V2 (S x Int) -> ColorSample x c
texelFetch2DArray   :: forall c x. ColorSampleable c =>  Sampler2DArray (Format c)     -> SampleOffset2 x -> S x Level -> V3 (S x Int) -> ColorSample x c
texelFetch3D        :: forall c x. ColorSampleable c =>  Sampler3D (Format c)          -> SampleOffset3 x -> S x Level -> V3 (S x Int) -> ColorSample x c

texelFetch1D (Sampler1D sampId _ prefix) off lod coord = toColor (undefined :: c) $ fetch (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "1D" sampId lod off coord iv1toF civ1toF
texelFetch1DArray (Sampler1DArray sampId _ prefix) off lod coord = toColor (undefined :: c) $ fetch (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "1DArray" sampId lod off coord iv2toF civ1toF
texelFetch2D (Sampler2D sampId _ prefix) off lod coord = toColor (undefined :: c) $ fetch (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "2D" sampId lod off coord iv2toF civ2toF
texelFetch2DArray (Sampler2DArray sampId _ prefix) off lod coord = toColor (undefined :: c) $ fetch (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "2DArray" sampId lod off coord iv3toF civ2toF
texelFetch3D (Sampler3D sampId _ prefix) off lod coord = toColor (undefined :: c) $ fetch (undefined :: ColorElement c) prefix (typeStr4 (undefined :: c)) "3D" sampId lod off coord iv3toF civ3toF

sampler1DSize      :: Sampler1D f -> S x Level -> S x Int
sampler1DArraySize :: Sampler1DArray f -> S x Level -> V2 (S x Int)
sampler2DSize      :: Sampler2D f -> S x Level -> V2 (S x Int)
sampler2DArraySize :: Sampler2DArray f -> S x Level -> V3 (S x Int)
sampler3DSize      :: Sampler3D f -> S x Level -> V3 (S x Int)
samplerCubeSize    :: SamplerCube f -> S x Level -> S x Int

sampler1DSize (Sampler1D sampId shadow prefix) = scalarS STypeInt . getTextureSize prefix sampId (addShadowPrefix shadow "1D")
sampler1DArraySize (Sampler1DArray sampId shadow prefix) = vec2S (STypeIVec 2) . getTextureSize prefix sampId (addShadowPrefix shadow "1DArray")
sampler2DSize (Sampler2D sampId shadow prefix) = vec2S (STypeIVec 2) . getTextureSize prefix sampId (addShadowPrefix shadow "2D")
sampler2DArraySize (Sampler2DArray sampId shadow prefix) = vec3S (STypeIVec 3) . getTextureSize prefix sampId (addShadowPrefix shadow "2DArray")
sampler3DSize (Sampler3D sampId shadow prefix) = vec3S (STypeIVec 3) . getTextureSize prefix sampId (addShadowPrefix shadow "3D")
samplerCubeSize (SamplerCube sampId shadow prefix) = (\(V2 x _) -> x) . vec2S (STypeIVec 2) . getTextureSize prefix sampId (addShadowPrefix shadow "Cube")    

addShadowPrefix :: Bool -> String -> String
addShadowPrefix shadow = if shadow then (++ "Shadow") else id 

getTextureSize :: String -> Int -> String -> S c Int -> ExprM String
getTextureSize prefix sampId sName l = do s <- useSampler prefix sName sampId  
                                          l' <- unS l
                                          return $ "textureSize(" ++ s ++ ',' : l' ++ ")"

sample :: e -> String -> String -> String -> Int -> SampleLod lcoord x -> SampleProj x -> Maybe off -> coord -> (coord -> ExprM String) -> (lcoord -> ExprM String) -> (off -> String) -> (coord -> S x Float -> ExprM String) -> V4 (S x e)  
sample _ prefix sDynType sName sampId lod proj off coord vToS lvToS ivToS pvToS =
    vec4S (STypeDyn sDynType) $ do s <- useSampler prefix sName sampId
                                   sampleFunc s proj lod off coord vToS lvToS ivToS pvToS 

sampleShadow :: String -> Int -> SampleLod lcoord x -> SampleProj x -> Maybe off -> coord -> (coord -> ExprM String) -> (lcoord -> ExprM String) -> (off -> String) -> (coord -> S x Float -> ExprM String) -> S x Float  
sampleShadow sName sampId lod proj off coord vToS lvToS civToS pvToS =
    scalarS STypeFloat $ do s <- useSampler "" (sName ++ "Shadow") sampId
                            sampleFunc s proj lod off coord vToS lvToS civToS pvToS 

fetch :: e -> String -> String -> String -> Int -> S x Int -> Maybe off -> coord -> (coord -> ExprM String) -> (off -> String) -> V4 (S x e)  
fetch _ prefix sDynType sName sampId lod off coord ivToS civToS =
    vec4S (STypeDyn sDynType) $ do s <- useSampler prefix sName sampId
                                   fetchFunc s off coord lod ivToS civToS

v1toF :: S c Float -> ExprM String
v2toF :: V2 (S c Float) -> ExprM String
v3toF :: V3 (S c Float) -> ExprM String
v4toF :: V4 (S c Float) -> ExprM String
v1toF = unS
v2toF (V2 x y) = do x' <- unS x
                    y' <- unS y
                    return $ "vec2(" ++ x' ++ ',':y' ++ ")"   
v3toF (V3 x y z) = do x' <- unS x
                      y' <- unS y
                      z' <- unS z
                      return $ "vec3(" ++ x' ++ ',':y' ++ ',':z' ++ ")"   
v4toF (V4 x y z w) = do x' <- unS x
                        y' <- unS y
                        z' <- unS z
                        w' <- unS w
                        return $ "vec4(" ++ x' ++ ',':y' ++ ',':z' ++ ',':w' ++ ")"

iv1toF :: S c Int -> ExprM String
iv2toF :: V2 (S c Int) -> ExprM String
iv3toF :: V3 (S c Int) -> ExprM String
iv1toF = unS
iv2toF (V2 x y) = do x' <- unS x
                     y' <- unS y
                     return $ "ivec2(" ++ x' ++ ',':y' ++ ")"   
iv3toF (V3 x y z) = do x' <- unS x
                       y' <- unS y
                       z' <- unS z
                       return $ "ivec3(" ++ x' ++ ',':y' ++ ',':z' ++ ")"   
                                                
civ1toF :: Int -> String
civ2toF :: V2 Int -> String
civ3toF :: V3 Int -> String
civ1toF = show   
civ2toF (V2 x y) = "ivec2(" ++ show x ++ ',':show y ++ ")"   
civ3toF (V3 x y z) = "ivec3(" ++ show x ++ ',':show y ++ ',':show z ++ ")"   
pv1toF :: S c Float -> S c Float -> ExprM String
pv2toF :: V2 (S c Float) -> S c Float -> ExprM String
pv3toF :: V3 (S c Float) -> S c Float -> ExprM String

pv1toF x y = do x' <- unS x
                y' <- unS y
                return $ "vec2(" ++ x' ++ ',':y' ++ ")"
pv2toF (V2 x y) z = do x' <- unS x
                       y' <- unS y
                       z' <- unS z
                       return $ "vec3(" ++ x' ++ ',':y' ++ ',':z' ++ ")"
pv3toF (V3 x y z) w = do x' <- unS x
                         y' <- unS y
                         z' <- unS z
                         w' <- unS w
                         return $ "vec4(" ++ x' ++ ',':y' ++ ',':z' ++  ',':w' ++ ")"

sampleFunc s proj lod off coord vToS lvToS civToS pvToS = do
    pc <- projCoordParam proj  
    l <- lodParam lod 
    b <- biasParam lod
    return $ "texture" ++ projName proj ++ lodName lod ++ offName off ++ '(' : s ++ ',' : pc ++ l ++ o ++ b ++ ")"  
  where 
    o = offParam off civToS 
    
    projName Nothing = ""
    projName _ = "Proj"

    projCoordParam Nothing = vToS coord
    projCoordParam (Just p) = pvToS coord p
    
    lodParam (SampleLod x) = fmap (',':) (unS x)
    lodParam (SampleGrad x y) = (++) <$> fmap (',':) (lvToS x) <*> fmap (',':) (lvToS y)
    lodParam _ = return ""
    
    biasParam :: SampleLod v x -> ExprM String 
    biasParam (SampleBias (S x)) = do x' <- x
                                      return $ ',':x'
    biasParam _ = return ""    
       
    lodName (SampleLod _) = "Lod"
    lodName (SampleGrad _ _) = "Grad"
    lodName _ = ""
    
fetchFunc s off coord lod vToS civToS = do
    c <- vToS coord 
    l <- unS lod 
    return $ "fetch" ++ offName off ++ '(' : s ++ ',' : c ++ ',': l ++ o ++ ")"  
  where 
    o = offParam off civToS
        
offParam :: Maybe t -> (t -> String) -> String
offParam Nothing _ = ""
offParam (Just x) civToS = ',' : civToS x

offName :: Maybe t -> String
offName Nothing = ""
offName _ = "Offset"

----------------------------------------------------------------------------------

-- | A texture image is a reference to a 2D array of pixels in a texture. Some textures contain one 'Image' per level of detail while some contain several. 
data Image f = Image TexName Int Int (V2 Int) (GLuint -> IO ()) -- the two Ints is last two in FBOKey

instance Eq (Image f) where
    (==) = imageEquals 

-- | Compare two images that doesn't necessarily has same type
imageEquals :: Image a -> Image b -> Bool
imageEquals (Image tn' k1' k2' _ _) (Image tn k1 k2 _ _) = tn' == tn && k1' == k1 && k2' == k2

getImageBinding :: Image t -> GLuint -> IO ()
getImageBinding (Image _ _ _ _ io) = io

getImageFBOKey :: Image t -> IO FBOKey
getImageFBOKey (Image tn k1 k2 _ _) = do tn' <- readIORef tn
                                         return $ FBOKey tn' k1 k2 
-- | Retrieve the 2D size an image
imageSize :: Image f -> V2 Int
imageSize (Image _ _ _ s _) = s

getTexture1DImage :: Texture1D os f -> Level -> Render os f' (Image f) 
getTexture1DArrayImage :: Texture1DArray os f -> Level -> Int -> Render os f' (Image f) 
getTexture2DImage :: Texture2D os f -> Level -> Render os f' (Image f) 
getTexture2DArrayImage :: Texture2DArray os f -> Level -> Int -> Render os f' (Image f) 
getTexture3DImage :: Texture3D os f -> Level -> Int -> Render os f' (Image f) 
getTextureCubeImage :: TextureCube os f -> Level -> CubeSide -> Render os f' (Image f) 

registerRenderWriteTextureName tn = Render (lift $ lift $ lift $ readIORef tn) >>= registerRenderWriteTexture . fromIntegral

getTexture1DImage t@(Texture1D tn _ ls) l' = let l = min ls l' in do registerRenderWriteTextureName tn 
                                                                     return $ Image tn 0 l (V2 (texture1DSizes t !! l) 1) $ \attP -> do { n <- readIORef tn; glFramebufferTexture1D GL_DRAW_FRAMEBUFFER attP GL_TEXTURE_1D n (fromIntegral l) }
getTexture1DArrayImage t@(Texture1DArray tn _ ls) l' y' = let l = min ls l' 
                                                              V2 x y = texture1DArraySizes t !! l 
                                                          in do registerRenderWriteTextureName tn 
                                                                return $ Image tn y' l (V2 x 1) $ \attP -> do { n <- readIORef tn; glFramebufferTextureLayer GL_DRAW_FRAMEBUFFER attP n (fromIntegral l) (fromIntegral $ min y' (y-1)) }
getTexture2DImage t@(Texture2D tn _ ls) l' = let l = min ls l' in do registerRenderWriteTextureName tn 
                                                                     return $ Image tn 0 l (texture2DSizes t !! l) $ \attP -> do { n <- readIORef tn; glFramebufferTexture2D GL_DRAW_FRAMEBUFFER attP GL_TEXTURE_2D n (fromIntegral l) }
getTexture2DImage t@(RenderBuffer2D tn _) _ = return $ Image tn (-1) 0 (head $ texture2DSizes t) $ \attP -> do { n <- readIORef tn; glFramebufferRenderbuffer GL_DRAW_FRAMEBUFFER attP GL_RENDERBUFFER n }
getTexture2DArrayImage t@(Texture2DArray tn _ ls) l' z' = let l = min ls l' 
                                                              V3 x y z = texture2DArraySizes t !! l 
                                                          in do registerRenderWriteTextureName tn 
                                                                return $ Image tn z' l (V2 x y) $ \attP -> do { n <- readIORef tn; glFramebufferTextureLayer GL_DRAW_FRAMEBUFFER attP n (fromIntegral l) (fromIntegral $ min z' (z-1)) } 
getTexture3DImage t@(Texture3D tn _ ls) l' z' = let l = min ls l' 
                                                    V3 x y z = texture3DSizes t !! l 
                                                in do registerRenderWriteTextureName tn 
                                                      return $ Image tn z' l (V2 x y) $ \attP -> do { n <- readIORef tn; glFramebufferTextureLayer GL_DRAW_FRAMEBUFFER attP n (fromIntegral l) (fromIntegral $ min z' (z-1)) }
getTextureCubeImage t@(TextureCube tn _ ls) l' s = let l = min ls l' 
                                                       x = textureCubeSizes t !! l
                                                       s' = getGlCubeSide s
                                                   in do registerRenderWriteTextureName tn 
                                                         return $ Image tn (fromIntegral s') l (V2 x x) $ \attP -> do { n <- readIORef tn; glFramebufferTexture2D GL_DRAW_FRAMEBUFFER attP s' n (fromIntegral l) }

getGlCubeSide :: CubeSide -> GLenum
getGlCubeSide CubePosX = GL_TEXTURE_CUBE_MAP_POSITIVE_X 
getGlCubeSide CubeNegX = GL_TEXTURE_CUBE_MAP_NEGATIVE_X 
getGlCubeSide CubePosY = GL_TEXTURE_CUBE_MAP_POSITIVE_Y
getGlCubeSide CubeNegY = GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
getGlCubeSide CubePosZ = GL_TEXTURE_CUBE_MAP_POSITIVE_Z
getGlCubeSide CubeNegZ = GL_TEXTURE_CUBE_MAP_NEGATIVE_Z




