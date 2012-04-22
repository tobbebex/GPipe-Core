{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}

module Graphics.GPipe.Shader where

import Data.Int
import Data.Word
import Data.Text.Lazy.Builder
import Control.Arrow
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.Monoid (mconcat, mappend)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.SNMap

data Shader os a b = Shader 
        (a -> b)
        (Kleisli (ShaderSetupT IO) a b)

type NextTempVar = Int
type NextGlobal = Int

type ShaderT m = ShaderSetupT (ShaderGenT m)

type ShaderSetupT = StateT NextGlobal  
    
type ShaderGenT m = ReaderT (SNMap RValue Int) (WriterT Builder (WriterT Builder (StateT NextTempVar m)))

data SElement = SScalar | Sx | Sy | Sz | Sw 

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


getNext :: Monad m => StateT Int m Int
getNext = do s <- get
             put $ s + 1
             return s

type VarType = String
type RValue = String

tellAssignment :: VarType -> RValue -> ShaderT IO Int
tellAssignment typ = lift . memoizeM f
    where f string = lift $ do var <- lift $ lift getNext
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
