{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}

module Graphics.GPipe.Shader where

import Data.Monoid (mappend)
import Data.Int
import Data.Word
import Data.Text.Lazy.Builder
import Control.Arrow
import Control.Monad.Trans.Writer
import Data.Functor.Identity (Identity)
import Control.Monad.Trans.State
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class (lift)

data Shader os a b = Shader 
        (Kleisli (ShaderT Identity) a b) -- TODO: Add other shaders later
        (Kleisli (ShaderSetupT IO) a b)

type NextTempVar = Int
type NextGlobal = Int

type ShaderT m = ShaderSetupT (ShaderGenT m)

type ShaderSetupT = StateT NextGlobal  
    
type ShaderGenT m = WriterT Builder (WriterT Builder (StateT NextTempVar m))

data SElement = SScalar | Sx | Sy | Sz | Sw 

data S c a = S String

toSScalar :: Int -> S c a
toSScalar var = S (show var)
toSVec2 :: Int -> (S c a, S c a)
toSVec2 var = let (x,y,_,_) = toSVec4 var in (x,y)  
toSVec3 :: Int -> (S c a, S c a, S c a)
toSVec3 var = let (x,y,z,_) = toSVec4 var in (x,y,z)  
toSVec4 :: Int -> (S c a, S c a, S c a, S c a)
toSVec4 var = let s = show var in (S $ s ++ ".x", S $ s ++ ".y", S $ s ++ ".z", S $ s ++ ".w")

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

tellAssignment :: VarType -> RValue -> ShaderT Identity Int
tellAssignment typ string = lift $ do var <- lift $ lift getNext
                                      tell $ mconcat [
                                        fromString typ,
                                        fromString " t",
                                        fromString (show var),
                                        fromString " = ",
                                        fromString string,
                                        fromString ";\n"
                                        ]
                                      return var

tellGlobalDecl :: String -> ShaderT Identity ()
tellGlobalDecl string = lift $ lift $ tell $ fromString string `mappend` fromString ";\n"
                                 