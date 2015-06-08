{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction,
  TypeFamilies, ScopedTypeVariables, FlexibleInstances, RankNTypes,
  MultiParamTypeClasses, FlexibleContexts, OverloadedStrings #-}

module Graphics.GPipe.Shader where

import Prelude hiding ((.), id)
import Data.Int
import Data.Word
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder
import Control.Category
import Control.Monad (void)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Monoid (mconcat, mappend)
import qualified Control.Monad.Trans.Class as T (lift)
import Data.SNMap
import qualified Data.IntMap as Map

type NextTempVar = Int
type NextGlobal = Int

data SType = STypeFloat | STypeInt | STypeBool | STypeUInt | STypeDyn String

stypeName :: SType -> String
stypeName STypeFloat = "float"
stypeName STypeInt = "int"
stypeName STypeBool = "bool"
stypeName STypeUInt = "uint"
stypeName (STypeDyn s) = s

stypeSize :: SType -> Int
stypeSize _ = 4

stypeAlign :: SType -> Int
stypeAlign _ = 4

type ShaderSource = Text

type ShaderM = SNMapReaderT [String] (StateT ShaderState (WriterT Builder (StateT NextTempVar IO))) -- IO for stable names
data ShaderState = ShaderState { 
                shaderUsedUniformBlocks :: Map.IntMap (ShaderGlobDeclM ()), 
                shaderUsedSamplers :: Map.IntMap (ShaderGlobDeclM ()),
                shaderUsedInput :: Map.IntMap (ShaderGlobDeclM (), (ShaderM (), ShaderGlobDeclM ())) -- For vertex shaders, the shaderM is always undefined and the int is the parameter name, for later shader stages it uses some name local to the transition instead    
                 }
                
runShaderM :: ShaderGlobDeclM () -> ShaderM () -> IO (Text, [Int], [Int], [Int], ShaderGlobDeclM (), ShaderM ())
runShaderM d m = do
               (st, body) <- evalStateT (runWriterT (execStateT (runSNMapReaderT (m :: ShaderM ())) (ShaderState Map.empty Map.empty Map.empty))) 0
               let (unis, uniDecls) = unzip $ Map.toAscList (shaderUsedUniformBlocks st)
                   (samps, sampDecls) = unzip $ Map.toAscList (shaderUsedSamplers st)
                   (inps, inpDescs) = unzip $ Map.toAscList (shaderUsedInput st)
                   (inpDecls, prevDesc) = unzip inpDescs
                   (prevSs, prevDecls) = unzip prevDesc
                   decls = do d
                              sequence_ uniDecls 
                              sequence_ sampDecls
                              sequence_ inpDecls
                   prevDecl = sequence_ prevDecls
                   prevS    = sequence_ prevSs
               return (makeShader decls body, unis, samps, inps, prevDecl, prevS)
    where
        makeShader :: ShaderGlobDeclM () -> Builder -> Text 
        makeShader m b = toLazyText $ mconcat [
                                execWriter m,
                                "main() {\n",
                                b,
                                "}"]
                         

type ShaderGlobDeclM = Writer Builder

newtype S c a = S { unS :: ShaderM String } 

scalarS :: SType -> ShaderM RValue -> S c a
scalarS typ = S . tellAssignment typ 

vec2S :: SType -> ShaderM RValue -> (S c a, S c a)
vec2S typ s = let (x,y,_z,_w) = vec4S typ s
              in (x,y)
vec3S :: SType -> ShaderM RValue -> (S c a, S c a, S c a)
vec3S typ s = let (x,y,z,_w) = vec4S typ s
              in (x,y,z)
vec4S :: SType -> ShaderM RValue -> (S c a, S c a, S c a, S c a)
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

-- TODO: Add func to generate shader decl header

useVInput :: SType -> Int -> ShaderM String
useVInput stype i = 
             do s <- T.lift get
                T.lift $ put $ s { shaderUsedInput = Map.insert i (gDeclInput, undefined) $ shaderUsedInput s }                
                return $ "in" ++ show i
    where
        gDeclInput = do tellGlobal "in "
                        tellGlobal $ stypeName stype          
                        tellGlobal " in"
                        tellGlobalLn $ show i

useFInput :: String -> SType -> Int -> ShaderM String -> ShaderM String
useFInput prefix stype i v =
             do s <- T.lift get
                T.lift $ put $ s { shaderUsedInput = Map.insert i (gDecl "in ", (assignOutput, gDecl "out ")) $ shaderUsedInput s }                
                return $ prefix ++ show i
    where
        assignOutput = do val <- v
                          let name = prefix ++ show i
                          tellAssignment' name val
                    
        gDecl s =    do tellGlobal s
                        tellGlobal $ stypeName stype          
                        tellGlobal $ ' ':prefix
                        tellGlobalLn $ show i

   
useUniform :: ShaderGlobDeclM () -> Int -> Int -> ShaderM String
useUniform decls blockI offset = 
             do T.lift $ modify $ \ s -> s { shaderUsedUniformBlocks = Map.insert blockI (gDeclUniformBlock blockI decls) $ shaderUsedUniformBlocks s } 
                return $ 'u':show blockI ++ '.':'u': show offset -- "u8.u4"
    where
        gDeclUniformBlock blockI decls =
            do  let blockStr = show blockI
                tellGlobal "uniform uBlock"
                tellGlobal blockStr
                tellGlobal " {\n"
                decls
                tellGlobal "} u"
                tellGlobalLn blockStr

useSampler :: Int -> ShaderM String
useSampler name = 
             do T.lift $ modify $ \ s -> s { shaderUsedSamplers = Map.insert name (gDeclSampler name) $ shaderUsedUniformBlocks s } 
                return $ 's':show name
    where
        gDeclSampler name = error "gDeclSampler not implemented"                

getNext :: Monad m => StateT Int m Int
getNext = do
    s <- get
    put $ s + 1
    return s

--getTempVar :: ShaderM Int
--getTempVar = lift $ lift $ lift $ lift getNext
       
type RValue = String

tellAssignment :: SType -> ShaderM RValue -> ShaderM String
tellAssignment typ m = fmap head . memoizeM $ do
                                 val <- m
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


data CompiledShader = CompiledShader { cshaderName :: Int, cshaderUniBlockNameToIndex :: Map.IntMap Int, cshaderSamplerNameToIndex :: Map.IntMap Int } 


-----------------------

class ShaderBase a x where
    shaderbaseDeclare :: x -> a -> WriterT [String] ShaderM a
    shaderbaseAssign :: x -> a -> StateT [String] ShaderM ()
    shaderbaseReturn :: x -> a -> ReaderT (ShaderM [String]) (State Int) a 

instance (ShaderBase a x, ShaderBase b x) => ShaderBase (a, b) x where
    shaderbaseDeclare _ _ = do a' <- shaderbaseDeclare (undefined :: x) (undefined :: a)
                               b' <- shaderbaseDeclare (undefined :: x) (undefined :: b)
                               return (a', b')
    shaderbaseAssign _ (a,b) = do shaderbaseAssign (undefined :: x) a
                                  shaderbaseAssign (undefined :: x) b    
    shaderbaseReturn _ _ = do a' <- shaderbaseReturn (undefined :: x) (undefined :: a)
                              b' <- shaderbaseReturn (undefined :: x) (undefined :: b)
                              return (a', b')  

instance ShaderBase (S c Int) c where
    shaderbaseDeclare _ _ = do var <- T.lift $ T.lift $ T.lift $ T.lift getNext
                               let root = 't' : show var
                               T.lift $ T.lift $ T.lift $ tell $ mconcat [
                                                           fromString $ stypeName STypeInt,
                                                           fromString " ",
                                                           fromString root,
                                                           fromString ";\n"]
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

instance ShaderBase (S c Float) c where
    shaderbaseDeclare _ _ = do var <- T.lift $ T.lift $ T.lift $ T.lift getNext
                               let root = 't' : show var
                               T.lift $ T.lift $ T.lift $ tell $ mconcat [
                                                           fromString $ stypeName STypeFloat,
                                                           fromString " ",
                                                           fromString root,
                                                           fromString ";\n"]
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

instance ShaderType (S c Float) c where
    type ShaderBaseType (S c Float) = S c Float
    toBase _ = id
    fromBase _ = id

instance ShaderType () x where
    type ShaderBaseType () = ()
    toBase _ = id
    fromBase _ = id

instance (ShaderType a x, ShaderType b x) => ShaderType (a,b) x where
    type ShaderBaseType (a,b) = (ShaderBaseType a, ShaderBaseType b)
    toBase x (a,b) = (toBase x a, toBase x b)
    fromBase x (a,b) = (fromBase x a, fromBase x b)
instance (ShaderType a x, ShaderType b x, ShaderType c x) => ShaderType (a,b,c) x where
    type ShaderBaseType (a,b,c) = (ShaderBaseType a, (ShaderBaseType b, ShaderBaseType c))
    toBase x (a,b,c) = (toBase x a, (toBase x b, toBase x c))
    fromBase x (a,(b,c)) = (fromBase x a, fromBase x b, fromBase x c)
    
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
                                           boolDecl <- tellAssignment STypeBool (unS $ bool a)
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
