{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, RankNTypes, ExistentialQuantification, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Graphics.GPipe.FrameBuffer where

import Graphics.GPipe.Buffer
import Graphics.GPipe.Format
import Graphics.GPipe.Pipeline
import Graphics.GPipe.Context
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)

class (ColorFormat (FrameBufferColorFormat m),
       DepthFormat (FrameBufferDepthFormat m),
       StencilFormat (FrameBufferStencilFormat m), 
       Monad m) => MonadFrameBuffer m where       
    type FrameBufferObjectSpace m
    type FrameBufferColorFormat m
    type FrameBufferDepthFormat m
    type FrameBufferStencilFormat m
    clear :: Maybe (HostColorFormat (FrameBufferColorFormat m)) 
             -> Maybe (HostColorFormat (FrameBufferDepthFormat m))
             -> Maybe (HostColorFormat (FrameBufferStencilFormat m))
             -> m ()
    runPipeline :: Pipeline 
                        (FrameBufferObjectSpace m)
                        (FrameBufferColorFormat m)
                        (FrameBufferDepthFormat m)
                        (FrameBufferStencilFormat m)
                   -> m ()
                        
newtype FrameBufferT os c d s m a = 
    FrameBufferT 
    (m a) deriving (Functor, Applicative, Monad, MonadIO)
        
runFrameBufferT :: 
    (MonadIO m, ContextProvider p) =>
    FrameBufferT os c d s (ContextT p f os m) a ->
    Buffer os (BufferColorFormat c) -> 
    Buffer os (BufferDepthFormat d) ->
    Buffer os (BufferStencilFormat s) ->
    ContextT p f os m a
runFrameBufferT (FrameBufferT m) c d s = do    
    liftContextIO $ setupFboGl c d s
    x <- m
    liftContextIO $ teardownFboGl c d s
    return x
    
instance (MonadIO m, ContextProvider p, ColorFormat c, DepthFormat d, StencilFormat s, Monad m) => MonadFrameBuffer (FrameBufferT os c d s (ContextT p f os m)) where
    type FrameBufferObjectSpace (FrameBufferT os c d s (ContextT p f os m)) = os
    type FrameBufferColorFormat (FrameBufferT os c d s (ContextT p f os m)) = c
    type FrameBufferDepthFormat (FrameBufferT os c d s (ContextT p f os m)) = d
    type FrameBufferStencilFormat (FrameBufferT os c d s (ContextT p f os m)) = s
    clear c d s = FrameBufferT $ liftContextIO $ clearFrameBufferGl c d s
    runPipeline p = FrameBufferT $ liftContextIO $ runPipeLineGl p
                                        
instance (MonadIO m, ContextProvider p, ColorFormat c, DepthFormat d, StencilFormat s, Monad m) => MonadFrameBuffer (ContextT p (ContextFormat c d s) os m) where
    type FrameBufferObjectSpace (ContextT p (ContextFormat c d s) os m) = os
    type FrameBufferColorFormat (ContextT p (ContextFormat c d s) os m) = c
    type FrameBufferDepthFormat (ContextT p (ContextFormat c d s) os m) = d
    type FrameBufferStencilFormat (ContextT p (ContextFormat c d s) os m) = s
    clear c d s = liftContextIO $ clearFrameBufferGl c d s
    runPipeline p = liftContextIO $ runPipeLineGl p
    

--------------------
-- Private 

setupFboGl c d s = undefined
teardownFboGl c d s = undefined
clearFrameBufferGl c d s = undefined
 
    
     