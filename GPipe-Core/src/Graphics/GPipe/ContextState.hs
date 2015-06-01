module Graphics.GPipe.ContextState where

import Graphics.GPipe.Shader

import Control.Monad.Trans.State.Lazy
import System.Mem.StableName
import qualified Data.HashTable.IO as HT
import Control.Monad (liftM)
import qualified Data.IntMap.Lazy as Map
import Control.Monad.Trans.Writer.Lazy (Writer)

data ContextState = ContextState { shaderCache :: HT.BasicHashTable (StableName (StaticFrameT StaticFrame ())) CompiledFrame }

type CompiledFrame = NameToIOforProgAndIndex -> Either String (IO ())

newContextState :: IO ContextState
newContextState = liftM ContextState HT.new 

data FrameState = FrameState Int Int

type StaticFrameT m = StateT FrameState m
runStaticFrameT :: Monad m => StateT FrameState m a -> m a
runStaticFrameT m = evalStateT m (FrameState 0 1)

-- All objects doesnt use all of these. Uniform use all, vertex array use only index and texture use the last two(?)
type ProgramName = Int
type Index = Int
type Binding = Int
type NameToIOforProgAndIndex = Map.IntMap (ProgramName -> Index -> Binding -> IO ())

type FragOutputName = Int 
type ErrorName = String 
data DrawCall = DrawCall FragOutputName ErrorName (ShaderM String) (FragmentStreamData) -- the shader is a vec4 return value in a fragment  

type ShaderPos = ShaderM ()
type DrawCallName = Int
data VertexStreamData = VertexStreamData DrawCallName
data FragmentStreamData = FragmentStreamData Side ShaderPos VertexStreamData


data Side = Front | Back | FrontAndBack

type StaticFrame = Writer [DrawCall] 

