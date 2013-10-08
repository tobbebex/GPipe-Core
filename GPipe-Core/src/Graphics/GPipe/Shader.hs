{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction, TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleInstances #-}

module Graphics.GPipe.Shader where

import Prelude hiding ((.), id)
import Data.Int
import Data.Word
import Data.Text.Lazy.Builder
import Control.Arrow
import Control.Category
import Control.Monad (void)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Monoid (mconcat, mappend)
import qualified Control.Monad.Trans.Class as T (lift)
import Data.SNMap
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set

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

type ShaderM = SNMapReaderT [String] (StateT ShaderState (WriterT Builder (StateT NextTempVar IO))) -- IO for stable names
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
type FBool = S F Bool

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
             do s <- T.lift get
                let nextIndex = Map.size $ shaderInputNameToIndex s
                T.lift $ put $ s {shaderInputNameToIndex = Map.insertWith (flip const) i nextIndex $ shaderInputNameToIndex s}                
                return $ "in" ++ show i

useUniform :: Int -> Int -> ShaderM String
useUniform blockI offset = 
             do T.lift $ modify $ \ s -> s { shaderUsedUniformBlocks = Set.insert blockI $ shaderUsedUniformBlocks s } 
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
tellAssignment typ val = fmap head . memoizeM $ do
                                 var <- T.lift $ T.lift $ T.lift getNext
                                 let name = 't' : show var
                                 tellAssignment' typ name val
                                 return [name]

tellAssignment' :: SType -> String -> RValue -> ShaderM ()
tellAssignment' typ name string = T.lift $ T.lift $ tell $ mconcat [
                                       fromString $ stypeName typ,
                                       fromString " ",
                                       fromString name,
                                       fromString " = ",
                                       fromString string,
                                       fromString ";\n"
                                       ]


--
tellGlobalLn :: String -> ShaderGlobDeclM ()
tellGlobalLn string = tell $ fromString string `mappend` fromString ";\n"
--
tellGlobal :: String -> ShaderGlobDeclM ()
tellGlobal = tell . fromString


data CompiledShader = CompiledShader { cshaderName :: Int, cshaderUniBlockNameToIndex :: Map.IntMap Int } 



-----------------------

class Liftable a where
    type Lifted a
    lift :: Lift a (Lifted a)
    unlift :: Unlift (Lifted a) a

data L a    

type Rooter = WriterT [String] ShaderM
type UnRooter = StateT [String] ShaderM 
type Returner = ReaderT (ShaderM [String]) (State Int) 
data Lift a b = Lift (Kleisli Rooter a b) (Kleisli UnRooter a b) (Kleisli Returner a b)
newtype Unlift a b = Unlift (a -> b) deriving (Category, Arrow)
instance Category Lift where
    id = Lift id id id
    Lift a b c . Lift x y z = Lift (a.x) (b.y) (c.z)   
instance Arrow Lift where
    arr f = Lift (arr f) (arr f) (arr f)
    first (Lift a b c) = Lift (first a) (first b) (first c)
    
 
instance Liftable (S c Int) where
    type Lifted (S c Int) = S (L c) Int
    lift = Lift (Kleisli rooter) (Kleisli unrooter) (Kleisli returner)
            where rooter (S shaderM) = do ul <- T.lift shaderM
                                          root <- T.lift $ tellAssignment STypeInt ul
                                          tell [root]
                                          return $ S $ return root
                  unrooter (S shaderM) = do ul <- T.lift shaderM
                                            x:xs <- get
                                            put xs
                                            T.lift $ tellAssignment' STypeInt x ul
                                            return undefined
                  returner _ = do i <- T.lift getNext
                                  m <- ask
                                  return $ S $ fmap (!!i) m
    unlift = Unlift $ \(S shaderM) -> S shaderM

if' :: forall a b x. (Liftable a, Liftable b) => a -> S x Bool -> (Lifted a -> Lifted b) -> (Lifted a -> Lifted b) -> b
if' = undefined

while :: forall a x. (Liftable a) => (Lifted a -> S x Bool) -> (Lifted a -> Lifted a) -> a -> a
while bool loopF a = let Lift (Kleisli rootM) (Kleisli unrootM) (Kleisli returner) = lift :: Lift a (Lifted a)
                         Unlift unliftF = unlift :: Unlift (Lifted a) a
                         whileM = memoizeM $ do
                                   (lifted :: Lifted a, roots) <- runWriterT $ rootM a
                                   boolStr <- unS $ bool lifted
                                   boolRoot <- tellAssignment STypeBool boolStr
                                   T.lift $ T.lift $ tell $ mconcat [
                                                               fromString "while(",
                                                               fromString boolRoot,
                                                               fromString "){\n"
                                                               ]
                                   let looped = loopF lifted 
                                   void $ evalStateT (unrootM $ unliftF looped) roots 
                                   loopedBoolStr <- unS $ bool looped
                                   tellAssignment' STypeBool boolRoot loopedBoolStr
                                   T.lift $ T.lift $ tell $ fromString "}\n"
                                   return roots
                     in unliftF $ evalState (runReaderT (returner (undefined :: a)) whileM) 0
