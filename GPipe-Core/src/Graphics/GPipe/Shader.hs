{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction, TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleInstances, RankNTypes, MultiParamTypeClasses, FlexibleContexts #-}

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
import Control.Monad.ST.Safe
import Data.Monoid (mconcat, mappend)
import qualified Control.Monad.Trans.Class as T (lift)
import Data.SNMap
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set
import Data.STRef

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
                                 T.lift $ T.lift $ tell (fromString $ stypeName typ ++ " ")
                                 tellAssignment' name val
                                 return [name]

tellAssignment' :: String -> RValue -> ShaderM ()
tellAssignment' name string = T.lift $ T.lift $ tell $ mconcat [
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

class ShaderBase a x where
    shaderbaseDeclare :: x -> a -> WriterT [String] ShaderM a
    shaderbaseAssign :: x -> a -> StateT [String] ShaderM ()
    shaderbaseReturn :: x -> a -> ReaderT (ShaderM [String]) (State Int) a 

instance ShaderBase (S c Int) c where
    shaderbaseDeclare _ _ = do var <- T.lift $ T.lift $ T.lift $ T.lift getNext
                               let root = 't' : show var
                               T.lift $ T.lift $ T.lift $ tell $ mconcat [
                                                           fromString $ stypeName STypeInt,
                                                           fromString " ",
                                                           fromString root]
                               tell [root]
                               return $ S $ return root
    shaderbaseAssign _ (S shaderM) = do ul <- T.lift shaderM
                                        x:xs <- get
                                        put xs
                                        T.lift $ tellAssignment' x ul
                                        return ()
    shaderbaseReturn _ _ = do i <- T.lift getNext
                              m <- ask
                              return $ S $ fmap (!!i) m

instance ShaderBase () x where
    shaderbaseDeclare _ = return
    shaderbaseAssign _ _ = return ()
    shaderbaseReturn _ = return   
    
class ShaderBase (ShaderBaseType a) x => ShaderType a x where
    type ShaderBaseType a
    toBase :: x -> a -> ShaderBaseType a
    fromBase :: x -> ShaderBaseType a -> a
    
instance ShaderType (S c Int) c where
    type ShaderBaseType (S c Int) = S c Int
    toBase _ = id
    fromBase _ = id

instance ShaderType () x where
    type ShaderBaseType () = ()
    toBase _ = id
    fromBase _ = id

ifThenElse' :: forall a x. (ShaderType a x) => S x Bool -> a -> a -> a
ifThenElse' b t e = ifThenElse b (const t) (const e) ()

ifThenElse :: forall a b x. (ShaderType a x, ShaderType b x) => S x Bool -> (a -> b) -> (a -> b) -> a -> b
ifThenElse c t e i = fromBase x $ ifThenElse_ c (toBase x . t . fromBase x) (toBase x . e . fromBase x) (toBase x i)
    where
        x = undefined :: x
        ifThenElse_ :: S x Bool -> (ShaderBaseType a -> ShaderBaseType b) -> (ShaderBaseType a -> ShaderBaseType b) -> ShaderBaseType a -> ShaderBaseType b
        ifThenElse_ bool thn els a = 
            let ifM = memoizeM $ do
                           boolStr <- unS bool
                           (lifted, aDecls) <- runWriterT $ shaderbaseDeclare x undefined
                           void $ evalStateT (shaderbaseAssign x a) aDecls
                           decls <- execWriterT $ shaderbaseDeclare x (undefined :: ShaderBaseType b)
                           tellIf boolStr                
                           void $ evalStateT (shaderbaseAssign x $ thn lifted) decls                                    
                           T.lift $ T.lift $ tell $ fromString "} else {\n"                   
                           void $ evalStateT (shaderbaseAssign x $ els lifted) decls
                           T.lift $ T.lift $ tell $ fromString "}\n"                                                 
                           return decls
            in evalState (runReaderT (shaderbaseReturn x undefined) ifM) 0

ifThen :: forall a x. (ShaderType a x) => S x Bool -> (a -> a) -> a -> a
ifThen c t i = fromBase x $ ifThen_ c (toBase x . t . fromBase x) (toBase x i)
    where
        x = undefined :: x
        ifThen_ :: S x Bool -> (ShaderBaseType a -> ShaderBaseType a) -> ShaderBaseType a -> ShaderBaseType a
        ifThen_ bool thn a = 
            let ifM = memoizeM $ do
                           boolStr <- unS bool
                           (lifted, decls) <- runWriterT $ shaderbaseDeclare x undefined
                           void $ evalStateT (shaderbaseAssign x a) decls
                           tellIf boolStr
                           void $ evalStateT (shaderbaseAssign x $ thn lifted) decls                                    
                           T.lift $ T.lift $ tell $ fromString "}\n"
                           return decls
            in evalState (runReaderT (shaderbaseReturn x undefined) ifM) 0
    

tellIf :: RValue -> ShaderM ()
tellIf boolStr = T.lift $ T.lift $ tell $ mconcat [
                                               fromString "if(",
                                               fromString boolStr,
                                               fromString "){\n"
                                               ]
while :: forall a x. (ShaderType a x) => (a -> S x Bool) -> (a -> a) -> a -> a
while c f i = fromBase x $ while_ (c . fromBase x) (toBase x . f . fromBase x) (toBase x i)            
    where
        x = undefined :: x
        while_ :: (ShaderBaseType a -> S x Bool) -> (ShaderBaseType a -> ShaderBaseType a) -> ShaderBaseType a -> ShaderBaseType a                                 
        while_ bool loopF a = let whileM = memoizeM $ do
                                           (lifted, decls) <- runWriterT $ shaderbaseDeclare x a
                                           void $ evalStateT (shaderbaseAssign x a) decls
                                           boolStr <- unS $ bool a
                                           boolDecl <- tellAssignment STypeBool boolStr
                                           T.lift $ T.lift $ tell $ mconcat [
                                                                       fromString "while(",
                                                                       fromString boolDecl,
                                                                       fromString "){\n"
                                                                       ]
                                           let looped = loopF lifted                                
                                           void $ evalStateT (shaderbaseAssign x looped) decls 
                                           loopedBoolStr <- unS $ bool looped
                                           tellAssignment' boolDecl loopedBoolStr
                                           T.lift $ T.lift $ tell $ fromString "}\n"
                                           return decls
                             in evalState (runReaderT (shaderbaseReturn x undefined) whileM) 0
