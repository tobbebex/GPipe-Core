{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}

module Graphics.GPipe.Shader where

import Prelude hiding ((.), id)
import Data.Int
import Data.Word
import Data.Text.Lazy.Builder
import Control.Arrow
import Control.Category
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.Monoid (mconcat, mappend)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.SNMap
import qualified Data.IntMap as Map

data Shader os a b = Shader 
        (Kleisli (ShaderT IO) a b)
        (Kleisli (ShaderSetupT IO) a b)

instance Category (Shader os) where
    Shader a b . Shader x y = Shader (a . x) (b . y)
    id = Shader id id
instance Arrow (Shader os) where
    first (Shader a b) = Shader (first a) (first b)
    arr f = Shader (arr f) (arr f)

type NextTempVar = Int
type NextGlobal = Int

type ShaderT m = ShaderSetupT (ShaderGenT m)

type ShaderSetupT = StateT NextGlobal  

type UniformBufferMap = Map.IntMap (String, Int) -- Offset to type+name + size
    
type ShaderGenT m = ReaderT (SNMap RValue Int) (WriterT Builder (WriterT Builder (StateT (NextTempVar, [UniformBufferMap]) m)))

newtype S c a = S { unS :: ShaderT IO String }

scalarS :: VarType -> RValue -> S c a
scalarS typ = S . fmap show . tellAssignment typ 


vec2S :: VarType -> RValue -> (S c a, S c a)
vec2S typ s = let (x,y,_z,_w) = vec4S typ s
              in (x,y)
vec3S :: VarType -> RValue -> (S c a, S c a, S c a)
vec3S typ s = let (x,y,z,_w) = vec4S typ s
              in (x,y,z)
vec4S :: VarType -> RValue -> (S c a, S c a, S c a, S c a)
vec4S typ s = let m = unS $ scalarS typ s
                  f p = S $ fmap (++ p) m
              in (f ".x", f ".y", f".z", f ".w")


data V
data P
data F

type VFloat = S V Float
type VInt32 = S V Int32
type VInt16 = S V Int16
type VInt8 = S V Int8
type VWord8 = S V Word8
type VWord16 = S V Word16
type VWord32 = S V Word32


getNextGlobal :: Monad m => StateT Int m Int
getNextGlobal = do
    s <- get
    put $ s + 1
    return s


getNextVar :: Monad m => StateT (Int, a) m Int
getNextVar = do
    (s, x) <- get
    put (s + 1, x)
    return s

getTempVar :: Monad m => ShaderT m Int
getTempVar = lift $ lift $ lift $ lift $ getNextVar

pushUMap :: Monad m => ShaderT m ()
pushUMap = lift $ lift $ lift $ lift $ do
    (s, x) <- get
    put (s, Map.empty : x)

popUMap :: Monad m => ShaderT m UniformBufferMap
popUMap = lift $ lift $ lift $ lift $ do
    (s, x:xs) <- get
    put (s, xs)
    return x

addToUMap :: Monad m => Int -> String -> Int -> ShaderT m ()
addToUMap a b c = lift $ lift $ lift $ lift $ do
    (s, x:xs) <- get
    let y = Map.insert a (b,c) x
    put (s, y : xs)
          

type VarType = String
type RValue = String

tellAssignment :: VarType -> RValue -> ShaderT IO Int
tellAssignment typ = lift . memoizeM f
    where f string = lift $ do var <- lift $ lift getNextVar
                               tell $ mconcat [
                                       fromString typ,
                                       fromString " t",
                                       fromString (show var),
                                       fromString " = ",
                                       fromString string,
                                       fromString ";\n"
                                       ]
                               return var

tellGlobalDecl :: String -> ShaderT IO ()
tellGlobalDecl string = lift $ lift $ tell $ fromString string `mappend` fromString ";\n"

tellGlobal :: String -> ShaderT IO ()
tellGlobal string = lift $ lift $ tell $ fromString string
