{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Graphics.GPipe.FrameBuffer where

import Graphics.GPipe.Texture
import Graphics.GPipe.Buffer
import Graphics.GPipe.Format
import Graphics.GPipe.Pipeline
import Graphics.GPipe.Context
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)

class (FramebufferFormat (MonadFramebufferFormat m),
       Monad m) => MonadFramebuffer m where       
    type FrameBufferObjectSpace m
    type MonadFramebufferFormat m
    clear :: Color (FramebufferColorFormat (MonadFramebufferFormat m)) (Maybe Float) 
             -> Maybe Float
             -> Maybe Float
             -> m ()
    runPipeline :: Pipeline (FrameBufferObjectSpace m) (MonadFramebufferFormat m) -> m ()
                        
newtype FrameBufferT os f m a = 
    FrameBufferT 
    (m a) deriving (Functor, Applicative, Monad, MonadIO)
        
runFrameBufferT :: 
    (MonadIO m, ContextProvider p, FramebufferFormat f) =>    
    FrameBufferT os f (ContextT p f2 os m) a ->
    Framebuffer os f ->
    ContextT p f2 os m a
runFrameBufferT (FrameBufferT m) f = do    
    liftContextIO $ setupFboGl f
    x <- m
    liftContextIO $ teardownFboGl f
    return x
    
instance (MonadIO m, ContextProvider p, FramebufferFormat f, Monad m) => MonadFramebuffer (FrameBufferT os f (ContextT p f2 os m)) where
    type FrameBufferObjectSpace (FrameBufferT os f (ContextT p f2 os m)) = os
    type MonadFramebufferFormat (FrameBufferT os f (ContextT p f2 os m)) = f
    clear c d s = FrameBufferT $ liftContextIO $ clearFrameBufferGl c d s
    runPipeline p = FrameBufferT $ liftContextIO $ runPipeLineGl p
                                        
instance (MonadIO m, ContextProvider p, FramebufferFormat f, Monad m) => MonadFramebuffer (ContextT p f os m) where
    type FrameBufferObjectSpace (ContextT p f os m) = os
    type MonadFramebufferFormat (ContextT p f os m) = f
    clear c d s = liftContextIO $ clearFrameBufferGl c d s
    runPipeline p = liftContextIO $ runPipeLineGl p


data Framebuffer os a = Framebuffer
instance Functor (Framebuffer os) -- for unzip
instance Applicative (Framebuffer os) -- for zip

attachRenderBuffer :: RenderBuffer os a -> Framebuffer os a
attachRenderBuffer = undefined

attachTexture :: Texture2D os a -> Framebuffer os a
attachTexture = undefined
   
data RenderBuffer os a = RenderBuffer Int




--------------------
-- Private 

setupFboGl f = undefined
teardownFboGl f = undefined
clearFrameBufferGl c d s = undefined
 
    
     