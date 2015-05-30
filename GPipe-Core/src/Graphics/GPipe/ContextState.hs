module Graphics.GPipe.ContextState where

import Control.Monad.Trans.State.Lazy
import System.Mem.StableName
import qualified Data.HashTable.IO as HT
import Data.Functor.Identity
import Control.Monad (liftM)

data ContextState = ContextState { shaderCache :: HT.BasicHashTable (StableName (StaticFrameT Identity ())) CompiledFrame }

newContextState :: IO ContextState
newContextState = liftM ContextState HT.new 

data CompiledFrame = CompiledFrame

newtype FrameState = FrameState Int

type StaticFrameT m = StateT FrameState m
runStaticFrameT :: Monad m => StateT FrameState m a -> m a
runStaticFrameT m = evalStateT m (FrameState 0)

