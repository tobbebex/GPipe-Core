{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}

module Graphics.GPipe.Shader where

import Prelude hiding ((.), id)
import Data.Int
import Data.Word
import Data.Text.Lazy.Builder
import Control.Category
import Control.Monad (when)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.Monoid (mconcat, mappend)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Maybe
import Data.SNMap
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set

--data Shader os a b = Shader 
--        (Kleisli (ShaderT IO) a b)
--        (Kleisli (ShaderSetupT IO) a b)
--
--instance Category (Shader os) where
--    Shader a b . Shader x y = Shader (a . x) (b . y)
--    id = Shader id id
--instance Arrow (Shader os) where
--    first (Shader a b) = Shader (first a) (first b)
--    arr f = Shader (arr f) (arr f)

type NextTempVar = Int
type NextGlobal = Int

data SType = STypeFloat | STypeInt | STypeBool | STypeUInt

stypeName :: SType -> String
stypeName STypeFloat = "float"
stypeName STypeInt = "int"
stypeName STypeBool = "bool"
stypeName STypeUInt = "uint"

stypeSize :: SType -> Int
stypeSize _ = 4

stypeAlign :: SType -> Int
stypeAlign _ = 4

   
type ShaderM = StateT ShaderState ShaderGen
type ShaderGen = ReaderT (SNMap RValue Int) (WriterT Builder (StateT NextTempVar IO)) -- IO for stable names
type InputNameToIndex = Map.IntMap Int
data ShaderState = ShaderState { shaderInputNameToIndex :: InputNameToIndex, shaderUsedUniformBlocks :: Set.IntSet }

type ShaderGlobDeclM = Writer Builder

newtype S c a = S { unS :: ShaderM String } 

scalarS :: SType -> RValue -> S c a
scalarS typ = S . tellAssignment typ 

vec2S :: SType -> RValue -> (S c a, S c a)
vec2S typ s = let (x,y,_z,_w) = vec4S typ s
              in (x,y)
vec3S :: SType -> RValue -> (S c a, S c a, S c a)
vec3S typ s = let (x,y,z,_w) = vec4S typ s
              in (x,y,z)
vec4S :: SType -> RValue -> (S c a, S c a, S c a, S c a)
vec4S typ s = let m = tellAssignment typ s
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

type FFloat = S F Float
type FInt32 = S F Int32
type FInt16 = S F Int16
type FInt8 = S F Int8
type FWord8 = S F Word8
type FWord16 = S F Word16
type FWord32 = S F Word32

--getNextGlobal :: Monad m => StateT Int m Int
--getNextGlobal = do
--    s <- get
--    put $ s + 1 
--    return s


gDeclInput :: SType -> Int -> ShaderGlobDeclM ()
gDeclInput stype i =
             do tellGlobal "in "
                tellGlobal $ stypeName stype          
                tellGlobal " in"
                tellGlobalLn $ show i

gDeclUniformBlock :: Int -> ShaderGlobDeclM () -> ShaderGlobDeclM ()
gDeclUniformBlock blockI decls =
    do  let blockStr = show blockI
        tellGlobal "uniform uBlock"
        tellGlobal blockStr
        tellGlobal " {\n"
        decls
        tellGlobal "} u"
        tellGlobalLn blockStr

useInput :: Int -> ShaderM String
useInput i = 
             do s <- get
                let nextIndex = Map.size $ shaderInputNameToIndex s
                put $ s {shaderInputNameToIndex = Map.insertWith (flip const) i nextIndex $ shaderInputNameToIndex s}                
                return $ "in" ++ show i

useUniform :: Int -> Int -> ShaderM String
useUniform blockI offset = 
             do modify $ \ s -> s { shaderUsedUniformBlocks = Set.insert blockI $ shaderUsedUniformBlocks s } 
                return $ 'u':show blockI ++ '.':'u': show offset -- "u8.u4"

getNext :: Monad m => StateT Int m Int
getNext = do
    s <- get
    put $ s + 1
    return s

--getTempVar :: ShaderM Int
--getTempVar = lift $ lift $ lift $ lift getNext
       
type RValue = String

tellAssignment :: SType -> RValue -> ShaderM String
tellAssignment typ = fmap (('t':) . show) . lift . memoizeM f
    where f string = lift $ do var <- lift getNext
                               tell $ mconcat [
                                       fromString $ stypeName typ,
                                       fromString " t",
                                       fromString (show var),
                                       fromString " = ",
                                       fromString string,
                                       fromString ";\n"
                                       ]
                               return var
--
tellGlobalLn :: String -> ShaderGlobDeclM ()
tellGlobalLn string = tell $ fromString string `mappend` fromString ";\n"
--
tellGlobal :: String -> ShaderGlobDeclM ()
tellGlobal = tell . fromString


data CompiledShader = CompiledShader { cshaderName :: Int, cshaderUniBlockNameToIndex :: Map.IntMap Int } 


