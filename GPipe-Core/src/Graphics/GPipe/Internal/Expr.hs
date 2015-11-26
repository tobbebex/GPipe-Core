{-# LANGUAGE GADTs, EmptyDataDecls, NoMonomorphismRestriction,
  TypeFamilies, ScopedTypeVariables, FlexibleInstances, RankNTypes,
  MultiParamTypeClasses, FlexibleContexts, OverloadedStrings #-}

module Graphics.GPipe.Internal.Expr where

import Prelude hiding ((.), id, (<*))
import Control.Category
import Control.Monad (void, when)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Monoid (mconcat, mappend)
import qualified Control.Monad.Trans.Class as T (lift)
import Data.SNMap
import qualified Data.IntMap as Map
import Data.Boolean
import Data.List (intercalate)
import Control.Applicative ((<$>), liftA, liftA2, liftA3)
import Linear.V4
import Linear.V3
import Linear.V2
import Linear.V1
import Linear.V0
import Linear.Affine
import Linear.Metric
import Linear.Matrix
import Linear.Vector
import Linear.Conjugate
import Data.Foldable (toList, Foldable)
import Data.Int
import Data.Word

type NextTempVar = Int
type NextGlobal = Int

data SType = STypeFloat | STypeInt | STypeBool | STypeUInt | STypeDyn String | STypeMat Int Int | STypeVec Int | STypeIVec Int | STypeUVec Int

stypeName :: SType -> String
stypeName STypeFloat = "float"
stypeName STypeInt = "int"
stypeName STypeBool = "bool"
stypeName STypeUInt = "uint"
stypeName (STypeDyn s) = s
stypeName (STypeMat r c) = "mat" ++ show c ++ 'x' : show r
stypeName (STypeVec n) = "vec" ++ show n
stypeName (STypeIVec n) = "ivec" ++ show n
stypeName (STypeUVec n) = "uvec" ++ show n

stypeSize :: SType -> Int
stypeSize (STypeVec n) = n * 4
stypeSize (STypeIVec n) = n * 4
stypeSize (STypeUVec n) = n * 4
stypeSize _ = 4

type ExprM = SNMapReaderT [String] (StateT ExprState (WriterT String (StateT NextTempVar IO))) -- IO for stable names
data ExprState = ExprState {
                shaderUsedUniformBlocks :: Map.IntMap (GlobDeclM ()),
                shaderUsedSamplers :: Map.IntMap (GlobDeclM ()),
                shaderUsedInput :: Map.IntMap (GlobDeclM (), (ExprM (), GlobDeclM ())) -- For vertex shaders, the shaderM is always undefined and the int is the parameter name, for later shader stages it uses some name local to the transition instead
                 }

runExprM :: GlobDeclM () -> ExprM () -> IO (String, [Int], [Int], [Int], GlobDeclM (), ExprM ())
runExprM d m = do
               (st, body) <- evalStateT (runWriterT (execStateT (runSNMapReaderT (m :: ExprM ())) (ExprState Map.empty Map.empty Map.empty))) 0
               let (unis, uniDecls) = unzip $ Map.toAscList (shaderUsedUniformBlocks st)
                   (samps, sampDecls) = unzip $ Map.toAscList (shaderUsedSamplers st)
                   (inps, inpDescs) = unzip $ Map.toAscList (shaderUsedInput st)
                   (inpDecls, prevDesc) = unzip inpDescs
                   (prevSs, prevDecls) = unzip prevDesc
                   decls = do d
                              sequence_ uniDecls
                              sequence_ sampDecls
                              sequence_ inpDecls
                   source = mconcat [
                                "#version 330\n",
                                execWriter decls,
                                "void main() {\n",
                                body,
                                "}\n"]
               return (source, unis, samps, inps, sequence_ prevDecls, sequence_ prevSs)

type GlobDeclM = Writer String

newtype S x a = S { unS :: ExprM String }

scalarS :: SType -> ExprM RValue -> S c a
scalarS typ = S . tellAssignment typ

vec2S :: SType -> ExprM RValue -> V2 (S c a)
vec2S typ s = let V4 x y _z _w = vec4S typ s
              in V2 x y
vec3S :: SType -> ExprM RValue -> V3 (S c a)
vec3S typ s = let V4 x y z _w = vec4S typ s
              in V3 x y z
vec4S :: SType -> ExprM RValue -> V4 (S c a)
vec4S typ s = let m = tellAssignment typ s
                  f p = S $ fmap (++ p) m
              in V4 (f ".x") (f ".y") (f ".z") (f ".w")

scalarS' :: RValue -> S c a
scalarS' = S . return

vec2S' :: RValue -> V2 (S c a)
vec2S' = vec2S'' . S . return
vec3S' :: RValue -> V3 (S c a)
vec3S' = vec3S'' . S . return
vec4S' :: RValue -> V4 (S c a)
vec4S' = vec4S'' . S . return

vec2S'' :: S c a -> V2 (S c a)
vec2S'' s = let V4 x y _z _w = vec4S'' s
            in V2 x y
vec3S'' :: S c a -> V3 (S c a)
vec3S'' s = let V4 x y z _w = vec4S'' s
            in V3 x y z
vec4S'' :: S c a -> V4 (S c a)
vec4S'' s = let f p = S $ fmap (++ ('[': show (p :: Int) ++"]")) (unS s)
            in V4 (f 0) (f 1) (f 2) (f 3)

-- | Phantom type used as first argument in @'S' 'V' a@ that denotes that the shader value is a vertex value
data V
--data P
-- | Phantom type used as first argument in @'S' 'F' a@ that denotes that the shader value is a fragment value
data F

type VFloat = S V Float
type VInt = S V Int
type VWord = S V Word
type VBool = S V Bool

type FFloat = S F Float
type FInt = S F Int
type FWord = S F Word
type FBool = S F Bool

useVInput :: SType -> Int -> ExprM String
useVInput stype i =
             do s <- T.lift get
                T.lift $ put $ s { shaderUsedInput = Map.insert i (gDeclInput, undefined) $ shaderUsedInput s }
                return $ "in" ++ show i
    where
        gDeclInput = do tellGlobal "in "
                        tellGlobal $ stypeName stype
                        tellGlobal " in"
                        tellGlobalLn $ show i

useFInput :: String -> String -> SType -> Int -> ExprM String -> ExprM String
useFInput qual prefix stype i v =
             do s <- T.lift get
                T.lift $ put $ s { shaderUsedInput = Map.insert i (gDecl (qual ++ " in "), (assignOutput, gDecl (qual ++ " out "))) $ shaderUsedInput s }
                return $ prefix ++ show i
    where
        assignOutput = do val <- v
                          let name = prefix ++ show i
                          tellAssignment' name val

        gDecl s =    do tellGlobal s
                        tellGlobal $ stypeName stype
                        tellGlobal $ ' ':prefix
                        tellGlobalLn $ show i


useUniform :: GlobDeclM () -> Int -> Int -> ExprM String
useUniform decls blockI offset =
             do T.lift $ modify $ \ s -> s { shaderUsedUniformBlocks = Map.insert blockI gDeclUniformBlock $ shaderUsedUniformBlocks s }
                return $ 'u':show blockI ++ '.':'u': show offset -- "u8.u4"
    where
        gDeclUniformBlock =
            do  let blockStr = show blockI
                tellGlobal "layout(std140) uniform uBlock"
                tellGlobal blockStr
                tellGlobal " {\n"
                decls
                tellGlobal "} u"
                tellGlobalLn blockStr

useSampler :: String -> String -> Int -> ExprM String
useSampler prefix str name =
             do T.lift $ modify $ \ s -> s { shaderUsedSamplers = Map.insert name gDeclSampler $ shaderUsedSamplers s }
                return $ 's':show name
    where
        gDeclSampler = do tellGlobal "uniform "
                          tellGlobal prefix
                          tellGlobal "sampler"
                          tellGlobal str
                          tellGlobal " s"
                          tellGlobalLn $ show name

getNext :: Monad m => StateT Int m Int
getNext = do
    s <- get
    put $ s + 1
    return s

type RValue = String

tellAssignment :: SType -> ExprM RValue -> ExprM String
tellAssignment typ m = fmap head . memoizeM $ do
                                 val <- m
                                 var <- T.lift $ T.lift $ T.lift getNext
                                 let name = 't' : show var
                                 T.lift $ T.lift $ tell (stypeName typ ++ " ")
                                 tellAssignment' name val
                                 return [name]

tellAssignment' :: String -> RValue -> ExprM ()
tellAssignment' name string = T.lift $ T.lift $ tell $ mconcat [name, " = ", string, ";\n"]

discard :: FBool -> ExprM ()
discard (S m) = do b <- m
                   when (b /= "true") $ T.lift $ T.lift $ tell $ mconcat ["if (!(", b, ")) discard;\n"]

--
tellGlobalLn :: String -> GlobDeclM ()
tellGlobalLn string = tell $ string `mappend` ";\n"
--
tellGlobal :: String -> GlobDeclM ()
tellGlobal = tell

-----------------------

-- | An opaque type
data ShaderBase a x where
    ShaderBaseFloat :: S x Float -> ShaderBase (S x Float) x
    ShaderBaseInt :: S x Int -> ShaderBase (S x Int) x
    ShaderBaseWord :: S x Word -> ShaderBase (S x Word) x
    ShaderBaseBool :: S x Bool -> ShaderBase (S x Bool) x
    ShaderBaseUnit :: ShaderBase () x
    ShaderBaseProd :: ShaderBase a x -> ShaderBase b x -> ShaderBase (a,b) x

shaderbaseDeclare :: ShaderBase a x -> WriterT [String] ExprM (ShaderBase a x)
shaderbaseAssign :: ShaderBase a x -> StateT [String] ExprM ()
shaderbaseReturn :: ShaderBase a x -> ReaderT (ExprM [String]) (State Int) (ShaderBase a x)

shaderbaseDeclare (ShaderBaseFloat _) = ShaderBaseFloat <$> shaderbaseDeclareDef STypeFloat
shaderbaseDeclare (ShaderBaseInt _) = ShaderBaseInt <$> shaderbaseDeclareDef STypeInt
shaderbaseDeclare (ShaderBaseWord _) = ShaderBaseWord <$> shaderbaseDeclareDef STypeUInt
shaderbaseDeclare (ShaderBaseBool _) = ShaderBaseBool <$> shaderbaseDeclareDef STypeBool
shaderbaseDeclare ShaderBaseUnit = return ShaderBaseUnit
shaderbaseDeclare (ShaderBaseProd a b) = do a' <- shaderbaseDeclare a
                                            b' <- shaderbaseDeclare b
                                            return $ ShaderBaseProd a' b'

shaderbaseAssign (ShaderBaseFloat a) = shaderbaseAssignDef a
shaderbaseAssign (ShaderBaseInt a) = shaderbaseAssignDef a
shaderbaseAssign (ShaderBaseWord a) = shaderbaseAssignDef a
shaderbaseAssign (ShaderBaseBool a) = shaderbaseAssignDef a
shaderbaseAssign ShaderBaseUnit = return ()
shaderbaseAssign (ShaderBaseProd a b) = do shaderbaseAssign a
                                           shaderbaseAssign b

shaderbaseReturn (ShaderBaseFloat _) = ShaderBaseFloat <$> shaderbaseReturnDef
shaderbaseReturn (ShaderBaseInt _) = ShaderBaseInt <$> shaderbaseReturnDef
shaderbaseReturn (ShaderBaseWord _) = ShaderBaseWord <$> shaderbaseReturnDef
shaderbaseReturn (ShaderBaseBool _) = ShaderBaseBool <$> shaderbaseReturnDef
shaderbaseReturn ShaderBaseUnit = return ShaderBaseUnit
shaderbaseReturn (ShaderBaseProd a b) = do a' <- shaderbaseReturn a
                                           b' <- shaderbaseReturn b
                                           return $ ShaderBaseProd a' b'

shaderbaseDeclareDef :: SType -> WriterT [String] ExprM (S x a)
shaderbaseDeclareDef styp = do var <- T.lift $ T.lift $ T.lift $ T.lift getNext
                               let root = 't' : show var
                               T.lift $ T.lift $ T.lift $ tell $ mconcat [stypeName styp, ' ':root, ";\n"]
                               tell [root]
                               return $ S $ return root

shaderbaseAssignDef  (S shaderM) = do ul <- T.lift shaderM
                                      x:xs <- get
                                      put xs
                                      T.lift $ tellAssignment' x ul
                                      return ()
shaderbaseReturnDef :: ReaderT (ExprM [String]) (State Int) (S x a)
shaderbaseReturnDef = do i <- T.lift getNext
                         m <- ask
                         return $ S $ fmap (!!i) m

-- | Constraint for types that may pass in and out of shader control structures. Define your own instances in terms of others and make sure to
--   make toBase as lazy as possible.
class ShaderType a x where
    -- | A base type that this type can convert into. Use the 'ShaderBaseType' function on an existing instance of 'ShaderType' to define this in your instance.
    type ShaderBaseType a
    -- | Convert this type to the shader base type. Make sure this is as lazy as possible (e.g. use tilde (@~@) on each pattern match).
    toBase :: x -> a -> ShaderBase (ShaderBaseType a) x
    -- | Convert back from the shader base type to this type.
    fromBase :: x -> ShaderBase (ShaderBaseType a) x -> a

instance ShaderType (S x Float) x where
    type ShaderBaseType (S x Float) = (S x Float)
    toBase _ = ShaderBaseFloat
    fromBase _ (ShaderBaseFloat a) = a

instance ShaderType (S x Int) x where
    type ShaderBaseType (S x Int) = (S x Int)
    toBase _ = ShaderBaseInt
    fromBase _ (ShaderBaseInt a) = a

instance ShaderType (S x Word) x where
    type ShaderBaseType (S x Word) = (S x Word)
    toBase _ = ShaderBaseWord
    fromBase _ (ShaderBaseWord a) = a

instance ShaderType (S x Bool) x where
    type ShaderBaseType (S x Bool) = (S x Bool)
    toBase _ = ShaderBaseBool
    fromBase _ (ShaderBaseBool a) = a

instance ShaderType () x where
    type ShaderBaseType () = ()
    toBase _ _ = ShaderBaseUnit
    fromBase _ ShaderBaseUnit = ()

instance ShaderType a x => ShaderType (V0 a) x where
    type ShaderBaseType (V0 a) = ()
    toBase _ V0 = ShaderBaseUnit
    fromBase _ ShaderBaseUnit = V0
instance ShaderType a x => ShaderType (V1 a) x where
    type ShaderBaseType (V1 a) = ShaderBaseType a
    toBase x ~(V1 a) = toBase x a
    fromBase x a = V1 (fromBase x a)
instance ShaderType a x => ShaderType (V2 a) x where
    type ShaderBaseType (V2 a) = (ShaderBaseType a, ShaderBaseType a)
    toBase x ~(V2 a b) = ShaderBaseProd (toBase x a) (toBase x b)
    fromBase x (ShaderBaseProd a b) = V2 (fromBase x a) (fromBase x b)
instance ShaderType a x => ShaderType (V3 a) x where
    type ShaderBaseType (V3 a) = (ShaderBaseType a, (ShaderBaseType a, ShaderBaseType a))
    toBase x ~(V3 a b c) = ShaderBaseProd (toBase x a) (ShaderBaseProd (toBase x b) (toBase x c))
    fromBase x (ShaderBaseProd a (ShaderBaseProd b c)) = V3 (fromBase x a) (fromBase x b) (fromBase x c)
instance ShaderType a x => ShaderType (V4 a) x where
    type ShaderBaseType (V4 a) = (ShaderBaseType a, (ShaderBaseType a, (ShaderBaseType a, ShaderBaseType a)))
    toBase x ~(V4 a b c d) = ShaderBaseProd (toBase x a) (ShaderBaseProd (toBase x b) (ShaderBaseProd (toBase x c) (toBase x d)))
    fromBase x (ShaderBaseProd a (ShaderBaseProd b (ShaderBaseProd c d))) = V4 (fromBase x a) (fromBase x b) (fromBase x c) (fromBase x d)

instance (ShaderType a x, ShaderType b x) => ShaderType (a,b) x where
    type ShaderBaseType (a,b) = (ShaderBaseType a, ShaderBaseType b)
    toBase x ~(a,b) = ShaderBaseProd (toBase x a) (toBase x b)
    fromBase x (ShaderBaseProd a b) = (fromBase x a, fromBase x b)
instance (ShaderType a x, ShaderType b x, ShaderType c x) => ShaderType (a,b,c) x where
    type ShaderBaseType (a,b,c) = (ShaderBaseType a, (ShaderBaseType b, ShaderBaseType c))
    toBase x ~(a,b,c) = ShaderBaseProd (toBase x a) (ShaderBaseProd (toBase x b) (toBase x c))
    fromBase x (ShaderBaseProd a (ShaderBaseProd b c)) = (fromBase x a, fromBase x b, fromBase x c)
instance (ShaderType a x, ShaderType b x, ShaderType c x, ShaderType d x) => ShaderType (a,b,c,d) x where
    type ShaderBaseType (a,b,c,d) = (ShaderBaseType a, (ShaderBaseType b, (ShaderBaseType c, ShaderBaseType d)))
    toBase x ~(a,b,c,d) = ShaderBaseProd (toBase x a) (ShaderBaseProd (toBase x b) (ShaderBaseProd (toBase x c) (toBase x d)))
    fromBase x (ShaderBaseProd a (ShaderBaseProd b (ShaderBaseProd c d))) = (fromBase x a, fromBase x b, fromBase x c, fromBase x d)
instance (ShaderType a x, ShaderType b x, ShaderType c x, ShaderType d x, ShaderType e x) => ShaderType (a,b,c,d,e) x where
    type ShaderBaseType (a,b,c,d,e) = (ShaderBaseType a, (ShaderBaseType b, (ShaderBaseType c, (ShaderBaseType d, ShaderBaseType e))))
    toBase x ~(a,b,c,d,e) = ShaderBaseProd (toBase x a) (ShaderBaseProd (toBase x b) (ShaderBaseProd (toBase x c) (ShaderBaseProd (toBase x d) (toBase x e))))
    fromBase x (ShaderBaseProd a (ShaderBaseProd b (ShaderBaseProd c (ShaderBaseProd d e)))) = (fromBase x a, fromBase x b, fromBase x c, fromBase x d, fromBase x e)
instance (ShaderType a x, ShaderType b x, ShaderType c x, ShaderType d x, ShaderType e x, ShaderType f x) => ShaderType (a,b,c,d,e,f) x where
    type ShaderBaseType (a,b,c,d,e,f) = (ShaderBaseType a, (ShaderBaseType b, (ShaderBaseType c, (ShaderBaseType d, (ShaderBaseType e, ShaderBaseType f)))))
    toBase x ~(a,b,c,d,e,f) = ShaderBaseProd (toBase x a) (ShaderBaseProd (toBase x b) (ShaderBaseProd (toBase x c) (ShaderBaseProd (toBase x d) (ShaderBaseProd (toBase x e) (toBase x f)))))
    fromBase x (ShaderBaseProd a (ShaderBaseProd b (ShaderBaseProd c (ShaderBaseProd d (ShaderBaseProd e f))))) = (fromBase x a, fromBase x b, fromBase x c, fromBase x d, fromBase x e, fromBase x f)
instance (ShaderType a x, ShaderType b x, ShaderType c x, ShaderType d x, ShaderType e x, ShaderType f x, ShaderType g x) => ShaderType (a,b,c,d,e,f,g) x where
    type ShaderBaseType (a,b,c,d,e,f,g) = (ShaderBaseType a, (ShaderBaseType b, (ShaderBaseType c, (ShaderBaseType d, (ShaderBaseType e, (ShaderBaseType f, ShaderBaseType g))))))
    toBase x ~(a,b,c,d,e,f,g) = ShaderBaseProd (toBase x a) (ShaderBaseProd (toBase x b) (ShaderBaseProd (toBase x c) (ShaderBaseProd (toBase x d) (ShaderBaseProd (toBase x e) (ShaderBaseProd (toBase x f) (toBase x g))))))
    fromBase x (ShaderBaseProd a (ShaderBaseProd b (ShaderBaseProd c (ShaderBaseProd d (ShaderBaseProd e (ShaderBaseProd f g)))))) = (fromBase x a, fromBase x b, fromBase x c, fromBase x d, fromBase x e, fromBase x f, fromBase x g)

-- | Works just like 'ifB', return second argument if first is 'true' otherwise return third argument.
--
-- The difference from 'ifB' is that it in most cases generate more efficient code when @a@ is a compound type (e.g. a tuple or a vector).
-- For simple types such as @S x Float@, @ifThenElse' == ifB@.
ifThenElse' :: forall a x. (ShaderType a x) => S x Bool -> a -> a -> a
ifThenElse' b t e = ifThenElse b (const t) (const e) ()

-- | @ifThenElse c f g x@ will return @f x@ if @c@ evaluates to 'true' or @g x@ otherwise.
--
--   In most cases functionally equivalent to 'ifThenElse'' but
--   usually generate smaller shader code since the last argument is not inlined into the two branches, which also would affect implicit derivates (e.g. 'dFdx', 'dFdy' or sampling using @SampleAuto@)
ifThenElse :: forall a b x. (ShaderType a x, ShaderType b x) => S x Bool -> (a -> b) -> (a -> b) -> a -> b
ifThenElse c t e i = fromBase x $ ifThenElse_ c (toBase x . t . fromBase x) (toBase x . e . fromBase x) (toBase x i)
    where
        x = undefined :: x
        ifThenElse_ :: S x Bool -> (ShaderBase (ShaderBaseType a) x -> ShaderBase (ShaderBaseType b) x) -> (ShaderBase (ShaderBaseType a) x -> ShaderBase (ShaderBaseType b) x) -> ShaderBase (ShaderBaseType a) x -> ShaderBase (ShaderBaseType b) x
        ifThenElse_ bool thn els a =
            let ifM = memoizeM $ do
                           boolStr <- unS bool
                           (lifted, aDecls) <- runWriterT $ shaderbaseDeclare (toBase x (errShaderType :: a))
                           void $ evalStateT (shaderbaseAssign a) aDecls
                           decls <- execWriterT $ shaderbaseDeclare (toBase x (errShaderType :: b))
                           tellIf boolStr
                           scopedM $ void $ evalStateT (shaderbaseAssign $ thn lifted) decls
                           T.lift $ T.lift $ tell "} else {\n"
                           scopedM $ void $ evalStateT (shaderbaseAssign $ els lifted) decls
                           T.lift $ T.lift $ tell "}\n"
                           return decls
            in evalState (runReaderT (shaderbaseReturn (toBase x (errShaderType :: b))) ifM) 0

-- | @ifThen c f x@ will return @f x@ if @c@ evaluates to 'true' or @x@ otherwise.
--
--   In most cases functionally equivalent to 'ifThenElse'' but
--   usually generate smaller shader code since the last argument is not inlined into the two branches, which also would affect implicit derivates (e.g. 'dFdx', 'dFdy' or sampling using @SampleAuto@)
ifThen :: forall a x. (ShaderType a x) => S x Bool -> (a -> a) -> a -> a
ifThen c t i = fromBase x $ ifThen_ c (toBase x . t . fromBase x) (toBase x i)
    where
        x = undefined :: x
        ifThen_ :: S x Bool -> (ShaderBase (ShaderBaseType a) x -> ShaderBase (ShaderBaseType a) x) -> ShaderBase (ShaderBaseType a) x -> ShaderBase (ShaderBaseType a) x
        ifThen_ bool thn a =
            let ifM = memoizeM $ do
                           boolStr <- unS bool
                           (lifted, decls) <- runWriterT $ shaderbaseDeclare (toBase x (errShaderType :: a))
                           void $ evalStateT (shaderbaseAssign a) decls
                           tellIf boolStr
                           scopedM $ void $ evalStateT (shaderbaseAssign $ thn lifted) decls
                           T.lift $ T.lift $ tell "}\n"
                           return decls
            in evalState (runReaderT (shaderbaseReturn (toBase x (errShaderType :: a))) ifM) 0

tellIf :: RValue -> ExprM ()
tellIf boolStr = T.lift $ T.lift $ tell $ mconcat ["if(", boolStr, "){\n" ]

-- | @while f g x@ will iteratively transform @x@ with @g@ as long as @f@ generates 'true'.
while :: forall a x. (ShaderType a x) => (a -> S x Bool) -> (a -> a) -> a -> a
while c f i = fromBase x $ while_ (c . fromBase x) (toBase x . f . fromBase x) (toBase x i)
    where
        x = undefined :: x
        while_ :: (ShaderBase (ShaderBaseType a) x -> S x Bool) -> (ShaderBase (ShaderBaseType a) x -> ShaderBase (ShaderBaseType a) x) -> ShaderBase (ShaderBaseType a) x -> ShaderBase (ShaderBaseType a) x
        while_ bool loopF a = let whileM = memoizeM $ do
                                           (lifted, decls) <- runWriterT $ shaderbaseDeclare (toBase x (errShaderType :: a))
                                           void $ evalStateT (shaderbaseAssign a) decls
                                           boolDecl <- tellAssignment STypeBool (unS $ bool a)
                                           T.lift $ T.lift $ tell $ mconcat ["while(", boolDecl, "){\n" ]
                                           let looped = loopF lifted
                                           scopedM $ do
                                               void $ evalStateT (shaderbaseAssign looped) decls
                                               loopedBoolStr <- unS $ bool looped
                                               tellAssignment' boolDecl loopedBoolStr
                                           T.lift $ T.lift $ tell "}\n"
                                           return decls
                             in evalState (runReaderT (shaderbaseReturn (toBase x (errShaderType :: a))) whileM) 0

errShaderType = error "toBase in an instance of ShaderType is not lazy enough! Make sure you use tilde (~) for each pattern match on a data constructor."

--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------


bin :: SType -> String -> S c x -> S c y -> S c z
bin typ o (S a) (S b) = S $ tellAssignment typ $ do a' <- a
                                                    b' <- b
                                                    return $ '(' : a' ++ o ++ b' ++ ")"

fun1 :: SType -> String -> S c x -> S c y
fun1 typ f (S a) = S $ tellAssignment typ $ do a' <- a
                                               return $ f ++ '(' : a' ++ ")"

fun2 :: SType -> String -> S c x -> S c y -> S c z
fun2 typ f (S a) (S b) = S $ tellAssignment typ $ do a' <- a
                                                     b' <- b
                                                     return $ f ++ '(' : a' ++ ',' : b' ++ ")"

fun3 :: SType -> String -> S c x -> S c y -> S c z -> S c w
fun3 typ f (S a) (S b) (S c) = S $ tellAssignment typ $ do a' <- a
                                                           b' <- b
                                                           c' <- c
                                                           return $ f ++ '(' : a' ++ ',' : b' ++ ',' : c' ++")"

fun4 :: SType -> String -> S c x -> S c y -> S c z -> S c w -> S c r
fun4 typ f (S a) (S b) (S c) (S d) = S $ tellAssignment typ $ do a' <- a
                                                                 b' <- b
                                                                 c' <- c
                                                                 d' <- d
                                                                 return $ f ++ '(' : a' ++ ',' : b' ++ ',' : c' ++ ',' : d' ++")"

postop :: SType -> String -> S c x -> S c y
postop typ f (S a) = S $ tellAssignment typ $ do a' <- a
                                                 return $ '(' : a' ++ f ++ ")"

preop :: SType -> String -> S c x -> S c y
preop typ f (S a) = S $ tellAssignment typ $ do a' <- a
                                                return $ '(' : f ++ a' ++ ")"

binf :: String -> S c x -> S c y -> S c Float
binf = bin STypeFloat
fun1f :: String -> S c x -> S c Float
fun1f = fun1 STypeFloat
fun2f :: String -> S c x -> S c y -> S c Float
fun2f = fun2 STypeFloat
fun3f :: String -> S c x -> S c y -> S c z -> S c Float
fun3f = fun3 STypeFloat
preopf :: String -> S c x -> S c Float
preopf = preop STypeFloat
postopf :: String -> S c x -> S c Float
postopf = postop STypeFloat

bini :: String -> S c x -> S c y -> S c Int
bini = bin STypeInt
fun1i :: String -> S c x -> S c Int
fun1i = fun1 STypeInt
preopi :: String -> S c x -> S c Int
preopi = preop STypeInt

binu :: String -> S c x -> S c y -> S c Word
binu = bin STypeUInt
fun1u :: String -> S c x -> S c Word
fun1u = fun1 STypeUInt
preopu :: String -> S c x -> S c Word
preopu = preop STypeUInt

instance Num (S a Float) where
    (+) = binf "+"
    (-) = binf "-"
    abs = fun1f "abs"
    signum = fun1f "sign"
    (*) = binf "*"
    fromInteger = S . return . show
    negate = preopf "-"

instance Num (S a Int) where
    (+) = bini "+"
    (-) = bini "-"
    abs = fun1i "abs"
    signum = fun1i "sign"
    (*) = bini "*"
    fromInteger = S . return . show
    negate = preopi "-"

instance Num (S a Word) where
    (+) = binu "+"
    (-) = binu "-"
    abs = fun1u "abs"
    signum = fun1u "sign"
    (*) = binu "*"
    fromInteger x = S $ return $ show x ++ "u"
    negate = preopu "-"

instance Fractional (S a Float) where
  (/)          = binf "/"
  fromRational = S . return . show . (`asTypeOf` (undefined :: Float)) . fromRational

class Integral' a where
    div' :: a -> a -> a
    mod' :: a -> a -> a

instance Integral' Int where
    div' = div
    mod' = mod
instance Integral' Int32 where
    div' = div
    mod' = mod
instance Integral' Int16 where
    div' = div
    mod' = mod
instance Integral' Int8 where
    div' = div
    mod' = mod
instance Integral' Word where
    div' = div
    mod' = mod
instance Integral' Word32 where
    div' = div
    mod' = mod
instance Integral' Word16 where
    div' = div
    mod' = mod
instance Integral' Word8 where
    div' = div
    mod' = mod
instance Integral' (S a Int) where
    div' = bini "/"
    mod' = bini "%"
instance Integral' (S a Word) where
    div' = binu "/"
    mod' = binu "%"
instance Integral' a => Integral' (V0 a) where
    div' = liftA2 div'
    mod' = liftA2 mod'
instance Integral' a => Integral' (V1 a) where
    div' = liftA2 div'
    mod' = liftA2 mod'
instance Integral' a => Integral' (V2 a) where
    div' = liftA2 div'
    mod' = liftA2 mod'
instance Integral' a => Integral' (V3 a) where
    div' = liftA2 div'
    mod' = liftA2 mod'
instance Integral' a => Integral' (V4 a) where
    div' = liftA2 div'
    mod' = liftA2 mod'

instance Floating (S a Float) where
  pi    = S $ return $ show (pi :: Float)
  sqrt  = fun1f "sqrt"
  exp   = fun1f "exp"
  log   = fun1f "log"
  (**)  = fun2f "pow"
  sin   = fun1f "sin"
  cos   = fun1f "cos"
  tan   = fun1f "tan"
  asin  = fun1f "asin"
  acos  = fun1f "acos"
  atan  = fun1f "atan"
  sinh  = fun1f "sinh"
  cosh  = fun1f "cosh"
  asinh = fun1f "asinh"
  atanh = fun1f "atanh"
  acosh = fun1f "acosh"

instance Boolean (S a Bool) where
  true = S $ return "true"
  false = S $ return "false"
  notB  = preop STypeBool "!"
  (&&*) = bin STypeBool "&&"
  (||*) = bin STypeBool "||"

type instance BooleanOf (S a x) = S a Bool

instance Eq x => EqB (S a x) where
  (==*) = bin STypeBool "=="
  (/=*) = bin STypeBool "!="

instance Ord x => OrdB (S a x) where
  (<*) = bin STypeBool "<"
  (<=*) = bin STypeBool "<="
  (>=*) = bin STypeBool ">="
  (>*) = bin STypeBool ">"

instance IfB (S a Float) where ifB = ifThenElse'
instance IfB (S a Int) where ifB = ifThenElse'
instance IfB (S a Word) where ifB = ifThenElse'
instance IfB (S a Bool) where ifB = ifThenElse'

instance Conjugate (S a Float)
instance Conjugate (S a Int)
instance Conjugate (S a Word)
instance TrivialConjugate  (S a Float)
instance TrivialConjugate  (S a Int)
instance TrivialConjugate  (S a Word)

-- | This class provides the GPU functions either not found in Prelude's numerical classes, or that has wrong types.
--   Instances are also provided for normal 'Float's and 'Double's.
class Floating a => Real' a where
  rsqrt :: a -> a
  exp2 :: a -> a
  log2 :: a -> a
  floor' :: a -> a
  ceiling' :: a -> a
  fract' :: a -> a
  mod'' :: a -> a -> a
  mix :: a -> a -> a-> a

  rsqrt = (1/) . sqrt
  exp2 = (2**)
  log2 = logBase 2
  mix x y a = x*(1-a)+y*a
  fract' x = x - floor' x
  mod'' x y = x - y* floor' (x/y)
  floor' x = -ceiling' (-x)
  ceiling' x = -floor' (-x)

  {-# MINIMAL floor' | ceiling' #-}

instance Real' Float where
  floor' = fromIntegral . floor
  ceiling' = fromIntegral . ceiling

instance Real' Double where
  floor' = fromIntegral . floor
  ceiling' = fromIntegral . ceiling

instance Real' (S x Float) where
  rsqrt = fun1f "inversesqrt"
  exp2 = fun1f "exp2"
  log2 = fun1f "log2"
  floor' = fun1f "floor"
  ceiling' = fun1f "ceil"
  fract' = fun1f "fract"
  mod'' = fun2f "mod"
  mix = fun3f "mix"

instance (Real' a) => Real' (V0 a) where
  rsqrt = liftA rsqrt
  exp2 = liftA exp2
  log2 = liftA log2
  floor' = liftA floor'
  ceiling' = liftA ceiling'
  fract' = liftA fract'
  mod'' = liftA2 mod''
  mix = liftA3 mix
instance (Real' a) => Real' (V1 a) where
  rsqrt = liftA rsqrt
  exp2 = liftA exp2
  log2 = liftA log2
  floor' = liftA floor'
  ceiling' = liftA ceiling'
  fract' = liftA fract'
  mod'' = liftA2 mod''
  mix = liftA3 mix
instance (Real' a) => Real' (V2 a) where
  rsqrt = liftA rsqrt
  exp2 = liftA exp2
  log2 = liftA log2
  floor' = liftA floor'
  ceiling' = liftA ceiling'
  fract' = liftA fract'
  mod'' = liftA2 mod''
  mix = liftA3 mix
instance (Real' a) => Real' (V3 a) where
  rsqrt = liftA rsqrt
  exp2 = liftA exp2
  log2 = liftA log2
  floor' = liftA floor'
  ceiling' = liftA ceiling'
  fract' = liftA fract'
  mod'' = liftA2 mod''
  mix = liftA3 mix
instance (Real' a) => Real' (V4 a) where
  rsqrt = liftA rsqrt
  exp2 = liftA exp2
  log2 = liftA log2
  floor' = liftA floor'
  ceiling' = liftA ceiling'
  fract' = liftA fract'
  mod'' = liftA2 mod''
  mix = liftA3 mix

-- | This class provides various order comparing functions
class (IfB a, OrdB a, Floating a) => FloatingOrd a where
  clamp :: a -> a -> a -> a
  saturate :: a -> a
  step :: a -> a -> a
  smoothstep :: a -> a -> a -> a
  clamp x a = minB (maxB x a)
  saturate x = clamp x 0 1
  step a x = ifB (x <* a) 0 1
  smoothstep a b x = let t = saturate ((x-a) / (b-a))
                     in t*t*(3-2*t)

instance FloatingOrd Float
instance FloatingOrd Double
instance FloatingOrd (S x Float) where
  clamp = fun3f "clamp"
  step = fun2f "step"
  smoothstep = fun3f "smoothstep"

-- | Provides a common way to convert numeric types to integer and floating point representations.
class Convert a where
    type ConvertFloat a
    type ConvertInt a
    type ConvertWord a
    -- | Convert to a floating point number.
    toFloat :: a -> ConvertFloat a
    -- | Convert to an integral number, using truncation if necessary.
    toInt :: a -> ConvertInt a
    -- | Convert to an unsigned integral number, using truncation if necessary.
    toWord :: a -> ConvertWord a

instance Convert Float where
    type ConvertFloat Float = Float
    type ConvertInt Float = Int
    type ConvertWord Float = Word
    toFloat = id
    toInt = truncate
    toWord = truncate
instance Convert Int where
    type ConvertFloat Int = Float
    type ConvertInt Int = Int
    type ConvertWord Int = Word
    toFloat = fromIntegral
    toInt = id
    toWord = fromIntegral
instance Convert Word where
    type ConvertFloat Word = Float
    type ConvertInt Word = Int
    type ConvertWord Word = Word
    toFloat = fromIntegral
    toInt = fromIntegral
    toWord = id
instance Convert (S x Float) where
    type ConvertFloat (S x Float) = S x Float
    type ConvertInt (S x Float) = S x Int
    type ConvertWord (S x Float) = S x Word
    toFloat = id
    toInt = fun1i "int"
    toWord = fun1u "uint"
instance Convert (S x Int) where
    type ConvertFloat (S x Int) = S x Float
    type ConvertInt (S x Int) = S x Int
    type ConvertWord (S x Int) = S x Word
    toFloat = fun1f "float"
    toInt = id
    toWord = fun1u "uint"
instance Convert (S x Word) where
    type ConvertFloat (S x Word) = S x Float
    type ConvertInt (S x Word) = S x Int
    type ConvertWord (S x Word) = S x Word
    toFloat = fun1f "float"
    toInt = fun1i "int"
    toWord = id

-- | The derivative in x using local differencing of the rasterized value.
dFdx :: FFloat -> FFloat
-- | The derivative in y using local differencing of the rasterized value.
dFdy :: FFloat -> FFloat
-- | The sum of the absolute derivative in x and y using local differencing of the rasterized value.
fwidth :: FFloat -> FFloat
dFdx = fun1f "dFdx"
dFdy = fun1f "dFdy"
fwidth = fun1f "fwidth"

---------------------------------
fromV f s v = S $ do params <- mapM (unS . f) $ toList v
                     return $ s ++ '(' : intercalate "," params ++ ")"

fromVec4 :: V4 (S x Float) -> S x (V4 Float)
fromVec4 = fromV id "vec4"
fromVec3 :: V3 (S x Float) -> S x (V3 Float)
fromVec3 = fromV id "vec3"
fromVec2 :: V2 (S x Float) -> S x (V2 Float)
fromVec2 = fromV id "vec2"

-- FromMat will transpose to keep inner vectors packed
fromMat22 :: V2 (V2 (S x Float)) -> S x (V2 (V2 Float))
fromMat22 = fromV fromVec2 "mat2x2"
fromMat23 :: V2 (V3 (S x Float)) -> S x (V2 (V3 Float))
fromMat23 = fromV fromVec3 "mat2x3"
fromMat24 :: V2 (V4 (S x Float)) -> S x (V2 (V4 Float))
fromMat24 = fromV fromVec4 "mat2x4"

fromMat32 :: V3 (V2 (S x Float)) -> S x (V3 (V2 Float))
fromMat32 = fromV fromVec2 "mat3x2"
fromMat33 :: V3 (V3 (S x Float)) -> S x (V3 (V3 Float))
fromMat33 = fromV fromVec3 "mat3x3"
fromMat34 :: V3 (V4 (S x Float)) -> S x (V3 (V4 Float))
fromMat34 = fromV fromVec4 "mat3x4"

fromMat42 :: V4 (V2 (S x Float)) -> S x (V4 (V2 Float))
fromMat42 = fromV fromVec2 "mat4x2"
fromMat43 :: V4 (V3 (S x Float)) -> S x (V4 (V3 Float))
fromMat43 = fromV fromVec3 "mat4x3"
fromMat44 :: V4 (V4 (S x Float)) -> S x (V4 (V4 Float))
fromMat44 = fromV fromVec4 "mat4x4"

mulToV4 a b = vec4S'' $ bin (STypeVec 4) "*" a b
mulToV3 a b = vec3S'' $ bin (STypeVec 3) "*" a b
mulToV2 a b = vec2S'' $ bin (STypeVec 2) "*" a b

mulToM (r,x) (c,y) a b = fmap y $ x $ bin (STypeMat c r) "*" a b

d2 = (2,vec2S'')
d3 = (3,vec3S'')
d4 = (4,vec4S'')

unV1 :: V1 t -> t
unV1 (V1 x) = x

outerToM (r,x) (c,y) a b = fmap y $ x $ fun2 (STypeMat c r) "outerProduct" a b

------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------- Rewrite rules for linear types --------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------

{-# RULES "norm/length4" norm = length4 #-}
{-# RULES "norm/length3" norm = length3 #-}
{-# RULES "norm/length2" norm = length2 #-}
length4 :: V4 (S x Float) -> S x Float
length4 = fun1f "length" . fromVec4
length3 :: V3 (S x Float) -> S x Float
length3 = fun1f "length" . fromVec3
length2 :: V2 (S x Float) -> S x Float
length2 = fun1f "length" . fromVec2

{-# RULES "signorm/normalize4" signorm = normalize4 #-}
{-# RULES "signorm/normalize3" signorm = normalize3 #-}
{-# RULES "signorm/normalize2" signorm = normalize2 #-}
normalize4 :: V4 (S x Float) -> V4 (S x Float)
normalize4 = vec4S'' . fun1 (STypeVec 4) "normalize" . fromVec4
normalize3 :: V3 (S x Float) -> V3 (S x Float)
normalize3 = vec3S'' . fun1 (STypeVec 3) "normalize" . fromVec3
normalize2 :: V2 (S x Float) -> V2 (S x Float)
normalize2 = vec2S'' . fun1 (STypeVec 2) "normalize" . fromVec2

{-# RULES "distanceA/dist4" distanceA = dist4 #-}
{-# RULES "distanceA/dist3" distanceA = dist3 #-}
{-# RULES "distanceA/dist2" distanceA = dist2 #-}
{-# RULES "distance/dist4" distance = dist4 #-}
{-# RULES "distance/dist3" distance = dist3 #-}
{-# RULES "distance/dist2" distance = dist2 #-}
dist4 :: V4 (S x Float) -> V4 (S x Float) -> S x Float
dist4 a b = fun2f "distance" (fromVec4 a) (fromVec4 b)
dist3 :: V3 (S x Float) -> V3 (S x Float) -> S x Float
dist3 a b = fun2f "distance" (fromVec3 a) (fromVec3 b)
dist2 :: V2 (S x Float) -> V2 (S x Float) -> S x Float
dist2 a b = fun2f "distance" (fromVec2 a) (fromVec2 b)


{-# RULES "cross/S" cross = crossS #-}
crossS :: V3 (S x Float) -> V3 (S x Float) -> V3 (S x Float)
crossS a b = vec3S'' $ fun2 (STypeVec 3) "cross" (fromVec3 a) (fromVec3 b)

{-# RULES "minB/S" minB = minS #-}
{-# RULES "maxB/S" maxB = maxS #-}
minS :: S x Float -> S x Float -> S x Float
minS = binf "min"
maxS :: S x Float -> S x Float -> S x Float
maxS = binf "max"

--------------------------------------------------------------

-- Matrix*Matrix, Vector*Matrix, Matrix*Vextor and outer Vector*Vector multiplications have operands in flipped order since glsl is column major
-- inner products are not flipped since why bother :)

-- Also, special verions when explicit V1 matrices are used (so eg 4 version of each dot function: v*v, v*m, m*v, m*m )

-- No rules for scalar products with vectors or matrices (eg scalar * matrix), we hope the glsl compiler will manage to optimize that...

{-# RULES "mul_12_21vv" dot = mul_12_21vv #-}
{-# RULES "mul_13_31vv" dot = mul_13_31vv #-}
{-# RULES "mul_14_41vv" dot = mul_14_41vv #-}
mul_12_21vv :: V2 (S x Float) -> V2 (S x Float) -> S x Float
mul_12_21vv a b = fun2f "dot" (fromVec2 a) (fromVec2 b)
mul_13_31vv :: V3 (S x Float) -> V3 (S x Float) -> S x Float
mul_13_31vv a b = fun2f "dot" (fromVec3 a) (fromVec3 b)
mul_14_41vv :: V4 (S x Float) -> V4 (S x Float) -> S x Float
mul_14_41vv a b = fun2f "dot" (fromVec4 a) (fromVec4 b)

{-# RULES "mul_12_21vm" (*!) = mul_12_21vm #-}
{-# RULES "mul_13_31vm" (*!) = mul_13_31vm #-}
{-# RULES "mul_14_41vm" (*!) = mul_14_41vm #-}
mul_12_21vm :: V2 (S x Float) -> V2 (V1 (S x Float)) -> V1 (S x Float)
mul_12_21vm a b = V1 $ fun2f "dot" (fromVec2 a) (fromVec2 $ fmap unV1 b)
mul_13_31vm :: V3 (S x Float) -> V3 (V1 (S x Float)) -> V1 (S x Float)
mul_13_31vm a b = V1 $ fun2f "dot" (fromVec3 a) (fromVec3 $ fmap unV1 b)
mul_14_41vm :: V4 (S x Float) -> V4 (V1 (S x Float)) -> V1 (S x Float)
mul_14_41vm a b = V1 $ fun2f "dot" (fromVec4 a) (fromVec4 $ fmap unV1 b)

{-# RULES "mul_12_21mv" (!*) = mul_12_21mv #-}
{-# RULES "mul_13_31mv" (!*) = mul_13_31mv #-}
{-# RULES "mul_14_41mv" (!*) = mul_14_41mv #-}
mul_12_21mv :: V1 (V2 (S x Float)) -> V2 (S x Float) -> V1 (S x Float)
mul_12_21mv a b = V1 $ fun2f "dot" (fromVec2 $ unV1 a) (fromVec2 b)
mul_13_31mv :: V1 (V3 (S x Float)) -> V3 (S x Float) -> V1 (S x Float)
mul_13_31mv a b = V1 $ fun2f "dot" (fromVec3 $ unV1 a) (fromVec3 b)
mul_14_41mv :: V1 (V4 (S x Float)) -> V4 (S x Float) -> V1 (S x Float)
mul_14_41mv a b = V1 $ fun2f "dot" (fromVec4 $ unV1 a) (fromVec4 b)

{-# RULES "mul_12_21mm" (!*!) = mul_12_21mm #-}
{-# RULES "mul_13_31mm" (!*!) = mul_13_31mm #-}
{-# RULES "mul_14_41mm" (!*!) = mul_14_41mm #-}
mul_12_21mm :: V1 (V2 (S x Float)) -> V2 (V1 (S x Float)) -> V1 (V1 (S x Float))
mul_12_21mm a b = V1 $ V1 $ fun2f "dot" (fromVec2 $ unV1 a) (fromVec2 $ fmap unV1 b)
mul_13_31mm :: V1 (V3 (S x Float)) -> V3 (V1 (S x Float)) -> V1 (V1 (S x Float))
mul_13_31mm a b = V1 $ V1 $ fun2f "dot" (fromVec3 $ unV1 a) (fromVec3 $ fmap unV1 b)
mul_14_41mm :: V1 (V4 (S x Float)) -> V4 (V1 (S x Float)) -> V1 (V1 (S x Float))
mul_14_41mm a b = V1 $ V1 $ fun2f "dot" (fromVec4 $ unV1 a) (fromVec4 $ fmap unV1 b)


{-# RULES "mul_21_12" outer = mul_21_12 #-}
{-# RULES "mul_21_13" outer = mul_21_13 #-}
{-# RULES "mul_21_14" outer = mul_21_14 #-}
{-# RULES "mul_31_12" outer = mul_31_12 #-}
{-# RULES "mul_31_13" outer = mul_31_13 #-}
{-# RULES "mul_31_14" outer = mul_31_14 #-}
{-# RULES "mul_41_12" outer = mul_41_12 #-}
{-# RULES "mul_41_13" outer = mul_41_13 #-}
{-# RULES "mul_41_14" outer = mul_41_14 #-}
mul_21_12 :: V2 (S x Float) -> V2 (S x Float) -> V2 (V2 (S x Float))
mul_21_12 a b = outerToM d2 d2 (fromVec2 b) (fromVec2 a)
mul_21_13 :: V2 (S x Float) -> V3 (S x Float) -> V2 (V3 (S x Float))
mul_21_13 a b = outerToM d2 d3 (fromVec3 b) (fromVec2 a)
mul_21_14 :: V2 (S x Float) -> V4 (S x Float) -> V2 (V4 (S x Float))
mul_21_14 a b = outerToM d2 d4 (fromVec4 b) (fromVec2 a)
mul_31_12 :: V3 (S x Float) -> V2 (S x Float) -> V3 (V2 (S x Float))
mul_31_12 a b = outerToM d3 d2 (fromVec2 b) (fromVec3 a)
mul_31_13 :: V3 (S x Float) -> V3 (S x Float) -> V3 (V3 (S x Float))
mul_31_13 a b = outerToM d3 d3 (fromVec3 b) (fromVec3 a)
mul_31_14 :: V3 (S x Float) -> V4 (S x Float) -> V3 (V4 (S x Float))
mul_31_14 a b = outerToM d3 d4 (fromVec4 b) (fromVec3 a)
mul_41_12 :: V4 (S x Float) -> V2 (S x Float) -> V4 (V2 (S x Float))
mul_41_12 a b = outerToM d4 d2 (fromVec2 b) (fromVec4 a)
mul_41_13 :: V4 (S x Float) -> V3 (S x Float) -> V4 (V3 (S x Float))
mul_41_13 a b = outerToM d4 d3 (fromVec3 b) (fromVec4 a)
mul_41_14 :: V4 (S x Float) -> V4 (S x Float) -> V4 (V4 (S x Float))
mul_41_14 a b = outerToM d4 d4 (fromVec4 b) (fromVec4 a)
{-# RULES "mul_21_12m" (!*!) = mul_21_12m #-}
{-# RULES "mul_21_13m" (!*!) = mul_21_13m #-}
{-# RULES "mul_21_14m" (!*!) = mul_21_14m #-}
{-# RULES "mul_31_12m" (!*!) = mul_31_12m #-}
{-# RULES "mul_31_13m" (!*!) = mul_31_13m #-}
{-# RULES "mul_31_14m" (!*!) = mul_31_14m #-}
{-# RULES "mul_41_12m" (!*!) = mul_41_12m #-}
{-# RULES "mul_41_13m" (!*!) = mul_41_13m #-}
{-# RULES "mul_41_14m" (!*!) = mul_41_14m #-}
mul_21_12m :: V2 (V1 (S x Float)) -> V1 (V2 (S x Float)) -> V2 (V2 (S x Float))
mul_21_12m a b = outerToM d2 d2 (fromVec2 $ unV1 b) (fromVec2 $ fmap unV1 a)
mul_21_13m :: V2 (V1 (S x Float)) -> V1 (V3 (S x Float)) -> V2 (V3 (S x Float))
mul_21_13m a b = outerToM d2 d3 (fromVec3 $ unV1 b) (fromVec2 $ fmap unV1 a)
mul_21_14m :: V2 (V1 (S x Float)) -> V1 (V4 (S x Float)) -> V2 (V4 (S x Float))
mul_21_14m a b = outerToM d2 d4 (fromVec4 $ unV1 b) (fromVec2 $ fmap unV1 a)
mul_31_12m :: V3 (V1 (S x Float)) -> V1 (V2 (S x Float)) -> V3 (V2 (S x Float))
mul_31_12m a b = outerToM d3 d2 (fromVec2 $ unV1 b) (fromVec3 $ fmap unV1 a)
mul_31_13m :: V3 (V1 (S x Float)) -> V1 (V3 (S x Float)) -> V3 (V3 (S x Float))
mul_31_13m a b = outerToM d3 d3 (fromVec3 $ unV1 b) (fromVec3 $ fmap unV1 a)
mul_31_14m :: V3 (V1 (S x Float)) -> V1 (V4 (S x Float)) -> V3 (V4 (S x Float))
mul_31_14m a b = outerToM d3 d4 (fromVec4 $ unV1 b) (fromVec3 $ fmap unV1 a)
mul_41_12m :: V4 (V1 (S x Float)) -> V1 (V2 (S x Float)) -> V4 (V2 (S x Float))
mul_41_12m a b = outerToM d4 d2 (fromVec2 $ unV1 b) (fromVec4 $ fmap unV1 a)
mul_41_13m :: V4 (V1 (S x Float)) -> V1 (V3 (S x Float)) -> V4 (V3 (S x Float))
mul_41_13m a b = outerToM d4 d3 (fromVec3 $ unV1 b) (fromVec4 $ fmap unV1 a)
mul_41_14m :: V4 (V1 (S x Float)) -> V1 (V4 (S x Float)) -> V4 (V4 (S x Float))
mul_41_14m a b = outerToM d4 d4 (fromVec4 $ unV1 b) (fromVec4 $ fmap unV1 a)


{-# RULES "mul_12_22" (*!) = mul_12_22 #-}
{-# RULES "mul_13_32" (*!) = mul_13_32 #-}
{-# RULES "mul_14_42" (*!) = mul_14_42 #-}
{-# RULES "mul_12_23" (*!) = mul_12_23 #-}
{-# RULES "mul_13_33" (*!) = mul_13_33 #-}
{-# RULES "mul_14_43" (*!) = mul_14_43 #-}
{-# RULES "mul_12_24" (*!) = mul_12_24 #-}
{-# RULES "mul_13_34" (*!) = mul_13_34 #-}
{-# RULES "mul_14_44" (*!) = mul_14_44 #-}
mul_12_22 :: V2 (S x Float) -> V2 (V2 (S x Float)) -> V2 (S x Float)
mul_12_22 v m = mulToV2 (fromMat22 m) (fromVec2 v)
mul_13_32 :: V3 (S x Float) -> V3 (V2 (S x Float)) -> V2 (S x Float)
mul_13_32 v m = mulToV2 (fromMat32 m) (fromVec3 v)
mul_14_42 :: V4 (S x Float) -> V4 (V2 (S x Float)) -> V2 (S x Float)
mul_14_42 v m = mulToV2 (fromMat42 m) (fromVec4 v)
mul_12_23 :: V2 (S x Float) -> V2 (V3 (S x Float)) -> V3 (S x Float)
mul_12_23 v m = mulToV3 (fromMat23 m) (fromVec2 v)
mul_13_33 :: V3 (S x Float) -> V3 (V3 (S x Float)) -> V3 (S x Float)
mul_13_33 v m = mulToV3 (fromMat33 m) (fromVec3 v)
mul_14_43 :: V4 (S x Float) -> V4 (V3 (S x Float)) -> V3 (S x Float)
mul_14_43 v m = mulToV3 (fromMat43 m) (fromVec4 v)
mul_12_24 :: V2 (S x Float) -> V2 (V4 (S x Float)) -> V4 (S x Float)
mul_12_24 v m = mulToV4 (fromMat24 m) (fromVec2 v)
mul_13_34 :: V3 (S x Float) -> V3 (V4 (S x Float)) -> V4 (S x Float)
mul_13_34 v m = mulToV4 (fromMat34 m) (fromVec3 v)
mul_14_44 :: V4 (S x Float) -> V4 (V4 (S x Float)) -> V4 (S x Float)
mul_14_44 v m = mulToV4 (fromMat44 m) (fromVec4 v)

{-# RULES "mul_12_22m" (!*!) = mul_12_22m #-}
{-# RULES "mul_13_32m" (!*!) = mul_13_32m #-}
{-# RULES "mul_14_42m" (!*!) = mul_14_42m #-}
{-# RULES "mul_12_23m" (!*!) = mul_12_23m #-}
{-# RULES "mul_13_33m" (!*!) = mul_13_33m #-}
{-# RULES "mul_14_43m" (!*!) = mul_14_43m #-}
{-# RULES "mul_12_24m" (!*!) = mul_12_24m #-}
{-# RULES "mul_13_34m" (!*!) = mul_13_34m #-}
{-# RULES "mul_14_44m" (!*!) = mul_14_44m #-}
mul_12_22m :: V1 (V2 (S x Float)) -> V2 (V2 (S x Float)) -> V1 (V2 (S x Float))
mul_12_22m v m = V1 $ mulToV2 (fromMat22 m) (fromVec2 $ unV1 v)
mul_13_32m :: V1 (V3 (S x Float)) -> V3 (V2 (S x Float)) -> V1 (V2 (S x Float))
mul_13_32m v m = V1 $ mulToV2 (fromMat32 m) (fromVec3 $ unV1 v)
mul_14_42m :: V1 (V4 (S x Float)) -> V4 (V2 (S x Float)) -> V1 (V2 (S x Float))
mul_14_42m v m = V1 $ mulToV2 (fromMat42 m) (fromVec4 $ unV1 v)
mul_12_23m :: V1 (V2 (S x Float)) -> V2 (V3 (S x Float)) -> V1 (V3 (S x Float))
mul_12_23m v m = V1 $ mulToV3 (fromMat23 m) (fromVec2 $ unV1 v)
mul_13_33m :: V1 (V3 (S x Float)) -> V3 (V3 (S x Float)) -> V1 (V3 (S x Float))
mul_13_33m v m = V1 $ mulToV3 (fromMat33 m) (fromVec3 $ unV1 v)
mul_14_43m :: V1 (V4 (S x Float)) -> V4 (V3 (S x Float)) -> V1 (V3 (S x Float))
mul_14_43m v m = V1 $ mulToV3 (fromMat43 m) (fromVec4 $ unV1 v)
mul_12_24m :: V1 (V2 (S x Float)) -> V2 (V4 (S x Float)) -> V1 (V4 (S x Float))
mul_12_24m v m = V1 $ mulToV4 (fromMat24 m) (fromVec2 $ unV1 v)
mul_13_34m :: V1 (V3 (S x Float)) -> V3 (V4 (S x Float)) -> V1 (V4 (S x Float))
mul_13_34m v m = V1 $ mulToV4 (fromMat34 m) (fromVec3 $ unV1 v)
mul_14_44m :: V1 (V4 (S x Float)) -> V4 (V4 (S x Float)) -> V1 (V4 (S x Float))
mul_14_44m v m = V1 $ mulToV4 (fromMat44 m) (fromVec4 $ unV1 v)

{-# RULES "mul_22_21" (!*) = mul_22_21 #-}
{-# RULES "mul_23_31" (!*) = mul_23_31 #-}
{-# RULES "mul_24_41" (!*) = mul_24_41 #-}
{-# RULES "mul_32_21" (!*) = mul_32_21 #-}
{-# RULES "mul_33_31" (!*) = mul_33_31 #-}
{-# RULES "mul_34_41" (!*) = mul_34_41 #-}
{-# RULES "mul_42_21" (!*) = mul_42_21 #-}
{-# RULES "mul_43_31" (!*) = mul_43_31 #-}
{-# RULES "mul_44_41" (!*) = mul_44_41 #-}
mul_22_21 :: V2 (V2 (S x Float)) -> V2 (S x Float) -> V2 (S x Float)
mul_22_21 m v = mulToV2 (fromVec2 v) (fromMat22 m)
mul_23_31 :: V2 (V3 (S x Float)) -> V3 (S x Float) -> V2 (S x Float)
mul_23_31 m v = mulToV2 (fromVec3 v) (fromMat23 m)
mul_24_41 :: V2 (V4 (S x Float)) -> V4 (S x Float) -> V2 (S x Float)
mul_24_41 m v = mulToV2 (fromVec4 v) (fromMat24 m)
mul_32_21 :: V3 (V2 (S x Float)) -> V2 (S x Float) -> V3 (S x Float)
mul_32_21 m v = mulToV3 (fromVec2 v) (fromMat32 m)
mul_33_31 :: V3 (V3 (S x Float)) -> V3 (S x Float) -> V3 (S x Float)
mul_33_31 m v = mulToV3 (fromVec3 v) (fromMat33 m)
mul_34_41 :: V3 (V4 (S x Float)) -> V4 (S x Float) -> V3 (S x Float)
mul_34_41 m v = mulToV3 (fromVec4 v) (fromMat34 m)
mul_42_21 :: V4 (V2 (S x Float)) -> V2 (S x Float) -> V4 (S x Float)
mul_42_21 m v = mulToV4 (fromVec2 v) (fromMat42 m)
mul_43_31 :: V4 (V3 (S x Float)) -> V3 (S x Float) -> V4 (S x Float)
mul_43_31 m v = mulToV4 (fromVec3 v) (fromMat43 m)
mul_44_41 :: V4 (V4 (S x Float)) -> V4 (S x Float) -> V4 (S x Float)
mul_44_41 m v = mulToV4 (fromVec4 v) (fromMat44 m)

{-# RULES "mul_22_21m" (!*!) = mul_22_21m #-}
{-# RULES "mul_23_31m" (!*!) = mul_23_31m #-}
{-# RULES "mul_24_41m" (!*!) = mul_24_41m #-}
{-# RULES "mul_32_21m" (!*!) = mul_32_21m #-}
{-# RULES "mul_33_31m" (!*!) = mul_33_31m #-}
{-# RULES "mul_34_41m" (!*!) = mul_34_41m #-}
{-# RULES "mul_42_21m" (!*!) = mul_42_21m #-}
{-# RULES "mul_43_31m" (!*!) = mul_43_31m #-}
{-# RULES "mul_44_41m" (!*!) = mul_44_41m #-}
mul_22_21m :: V2 (V2 (S x Float)) -> V2 (V1 (S x Float)) -> V2 (V1 (S x Float))
mul_22_21m m v = V1 <$> mulToV2 (fromVec2 $ fmap unV1 v) (fromMat22 m)
mul_23_31m :: V2 (V3 (S x Float)) -> V3 (V1 (S x Float)) -> V2 (V1 (S x Float))
mul_23_31m m v = V1 <$> mulToV2 (fromVec3 $ fmap unV1 v) (fromMat23 m)
mul_24_41m :: V2 (V4 (S x Float)) -> V4 (V1 (S x Float)) -> V2 (V1 (S x Float))
mul_24_41m m v = V1 <$> mulToV2 (fromVec4 $ fmap unV1 v) (fromMat24 m)
mul_32_21m :: V3 (V2 (S x Float)) -> V2 (V1 (S x Float)) -> V3 (V1 (S x Float))
mul_32_21m m v = V1 <$> mulToV3 (fromVec2 $ fmap unV1 v) (fromMat32 m)
mul_33_31m :: V3 (V3 (S x Float)) -> V3 (V1 (S x Float)) -> V3 (V1 (S x Float))
mul_33_31m m v = V1 <$> mulToV3 (fromVec3 $ fmap unV1 v) (fromMat33 m)
mul_34_41m :: V3 (V4 (S x Float)) -> V4 (V1 (S x Float)) -> V3 (V1 (S x Float))
mul_34_41m m v = V1 <$> mulToV3 (fromVec4 $ fmap unV1 v) (fromMat34 m)
mul_42_21m :: V4 (V2 (S x Float)) -> V2 (V1 (S x Float)) -> V4 (V1 (S x Float))
mul_42_21m m v = V1 <$> mulToV4 (fromVec2 $ fmap unV1 v) (fromMat42 m)
mul_43_31m :: V4 (V3 (S x Float)) -> V3 (V1 (S x Float)) -> V4 (V1 (S x Float))
mul_43_31m m v = V1 <$> mulToV4 (fromVec3 $ fmap unV1 v) (fromMat43 m)
mul_44_41m :: V4 (V4 (S x Float)) -> V4 (V1 (S x Float)) -> V4 (V1 (S x Float))
mul_44_41m m v = V1 <$> mulToV4 (fromVec4 $ fmap unV1 v) (fromMat44 m)
-----------------------

{-# RULES "mul_22_22" (!*!) = mul_22_22 #-}
{-# RULES "mul_23_32" (!*!) = mul_23_32 #-}
{-# RULES "mul_24_42" (!*!) = mul_24_42 #-}
{-# RULES "mul_22_23" (!*!) = mul_22_23 #-}
{-# RULES "mul_23_33" (!*!) = mul_23_33 #-}
{-# RULES "mul_24_43" (!*!) = mul_24_43 #-}
{-# RULES "mul_22_24" (!*!) = mul_22_24 #-}
{-# RULES "mul_23_34" (!*!) = mul_23_34 #-}
{-# RULES "mul_24_44" (!*!) = mul_24_44 #-}
mul_22_22 :: V2 (V2 (S x Float)) -> V2 (V2 (S x Float)) -> V2 (V2 (S x Float))
mul_22_22 a b = mulToM d2 d2 (fromMat22 b) (fromMat22 a)
mul_23_32 :: V2 (V3 (S x Float)) -> V3 (V2 (S x Float)) -> V2 (V2 (S x Float))
mul_23_32 a b = mulToM d2 d2 (fromMat32 b) (fromMat23 a)
mul_24_42 :: V2 (V4 (S x Float)) -> V4 (V2 (S x Float)) -> V2 (V2 (S x Float))
mul_24_42 a b = mulToM d2 d2 (fromMat42 b) (fromMat24 a)
mul_22_23 :: V2 (V2 (S x Float)) -> V2 (V3 (S x Float)) -> V2 (V3 (S x Float))
mul_22_23 a b = mulToM d2 d3 (fromMat23 b) (fromMat22 a)
mul_23_33 :: V2 (V3 (S x Float)) -> V3 (V3 (S x Float)) -> V2 (V3 (S x Float))
mul_23_33 a b = mulToM d2 d3 (fromMat33 b) (fromMat23 a)
mul_24_43 :: V2 (V4 (S x Float)) -> V4 (V3 (S x Float)) -> V2 (V3 (S x Float))
mul_24_43 a b = mulToM d2 d3 (fromMat43 b) (fromMat24 a)
mul_22_24 :: V2 (V2 (S x Float)) -> V2 (V4 (S x Float)) -> V2 (V4 (S x Float))
mul_22_24 a b = mulToM d2 d4 (fromMat24 b) (fromMat22 a)
mul_23_34 :: V2 (V3 (S x Float)) -> V3 (V4 (S x Float)) -> V2 (V4 (S x Float))
mul_23_34 a b = mulToM d2 d4 (fromMat34 b) (fromMat23 a)
mul_24_44 :: V2 (V4 (S x Float)) -> V4 (V4 (S x Float)) -> V2 (V4 (S x Float))
mul_24_44 a b = mulToM d2 d4 (fromMat44 b) (fromMat24 a)


{-# RULES "mul_32_22" (!*!) = mul_32_22 #-}
{-# RULES "mul_33_32" (!*!) = mul_33_32 #-}
{-# RULES "mul_34_42" (!*!) = mul_34_42 #-}
{-# RULES "mul_32_23" (!*!) = mul_32_23 #-}
{-# RULES "mul_33_33" (!*!) = mul_33_33 #-}
{-# RULES "mul_34_43" (!*!) = mul_34_43 #-}
{-# RULES "mul_32_24" (!*!) = mul_32_24 #-}
{-# RULES "mul_33_34" (!*!) = mul_33_34 #-}
{-# RULES "mul_34_44" (!*!) = mul_34_44 #-}
mul_32_22 :: V3 (V2 (S x Float)) -> V2 (V2 (S x Float)) -> V3 (V2 (S x Float))
mul_32_22 a b = mulToM d3 d2 (fromMat22 b) (fromMat32 a)
mul_33_32 :: V3 (V3 (S x Float)) -> V3 (V2 (S x Float)) -> V3 (V2 (S x Float))
mul_33_32 a b = mulToM d3 d2 (fromMat32 b) (fromMat33 a)
mul_34_42 :: V3 (V4 (S x Float)) -> V4 (V2 (S x Float)) -> V3 (V2 (S x Float))
mul_34_42 a b = mulToM d3 d2 (fromMat42 b) (fromMat34 a)
mul_32_23 :: V3 (V2 (S x Float)) -> V2 (V3 (S x Float)) -> V3 (V3 (S x Float))
mul_32_23 a b = mulToM d3 d3 (fromMat23 b) (fromMat32 a)
mul_33_33 :: V3 (V3 (S x Float)) -> V3 (V3 (S x Float)) -> V3 (V3 (S x Float))
mul_33_33 a b = mulToM d3 d3 (fromMat33 b) (fromMat33 a)
mul_34_43 :: V3 (V4 (S x Float)) -> V4 (V3 (S x Float)) -> V3 (V3 (S x Float))
mul_34_43 a b = mulToM d3 d3 (fromMat43 b) (fromMat34 a)
mul_32_24 :: V3 (V2 (S x Float)) -> V2 (V4 (S x Float)) -> V3 (V4 (S x Float))
mul_32_24 a b = mulToM d3 d4 (fromMat24 b) (fromMat32 a)
mul_33_34 :: V3 (V3 (S x Float)) -> V3 (V4 (S x Float)) -> V3 (V4 (S x Float))
mul_33_34 a b = mulToM d3 d4 (fromMat34 b) (fromMat33 a)
mul_34_44 :: V3 (V4 (S x Float)) -> V4 (V4 (S x Float)) -> V3 (V4 (S x Float))
mul_34_44 a b = mulToM d3 d4 (fromMat44 b) (fromMat34 a)


{-# RULES "mul_42_22" (!*!) = mul_42_22 #-}
{-# RULES "mul_43_32" (!*!) = mul_43_32 #-}
{-# RULES "mul_44_42" (!*!) = mul_44_42 #-}
{-# RULES "mul_42_23" (!*!) = mul_42_23 #-}
{-# RULES "mul_43_33" (!*!) = mul_43_33 #-}
{-# RULES "mul_44_43" (!*!) = mul_44_43 #-}
{-# RULES "mul_42_24" (!*!) = mul_42_24 #-}
{-# RULES "mul_43_34" (!*!) = mul_43_34 #-}
{-# RULES "mul_44_44" (!*!) = mul_44_44 #-}
mul_42_22 :: V4 (V2 (S x Float)) -> V2 (V2 (S x Float)) -> V4 (V2 (S x Float))
mul_42_22 a b = mulToM d4 d2 (fromMat22 b) (fromMat42 a)
mul_43_32 :: V4 (V3 (S x Float)) -> V3 (V2 (S x Float)) -> V4 (V2 (S x Float))
mul_43_32 a b = mulToM d4 d2 (fromMat32 b) (fromMat43 a)
mul_44_42 :: V4 (V4 (S x Float)) -> V4 (V2 (S x Float)) -> V4 (V2 (S x Float))
mul_44_42 a b = mulToM d4 d2 (fromMat42 b) (fromMat44 a)
mul_42_23 :: V4 (V2 (S x Float)) -> V2 (V3 (S x Float)) -> V4 (V3 (S x Float))
mul_42_23 a b = mulToM d4 d3 (fromMat23 b) (fromMat42 a)
mul_43_33 :: V4 (V3 (S x Float)) -> V3 (V3 (S x Float)) -> V4 (V3 (S x Float))
mul_43_33 a b = mulToM d4 d3 (fromMat33 b) (fromMat43 a)
mul_44_43 :: V4 (V4 (S x Float)) -> V4 (V3 (S x Float)) -> V4 (V3 (S x Float))
mul_44_43 a b = mulToM d4 d3 (fromMat43 b) (fromMat44 a)
mul_42_24 :: V4 (V2 (S x Float)) -> V2 (V4 (S x Float)) -> V4 (V4 (S x Float))
mul_42_24 a b = mulToM d4 d4 (fromMat24 b) (fromMat42 a)
mul_43_34 :: V4 (V3 (S x Float)) -> V3 (V4 (S x Float)) -> V4 (V4 (S x Float))
mul_43_34 a b = mulToM d4 d4 (fromMat34 b) (fromMat43 a)
mul_44_44 :: V4 (V4 (S x Float)) -> V4 (V4 (S x Float)) -> V4 (V4 (S x Float))
mul_44_44 a b = mulToM d4 d4 (fromMat44 b) (fromMat44 a)
