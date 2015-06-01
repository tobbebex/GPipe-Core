{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Graphics.GPipe.FragmentStream where

import Control.Category hiding ((.))
import Control.Arrow 
import Graphics.GPipe.Shader
import Graphics.GPipe.VertexStream
import Graphics.GPipe.ContextState
import Control.Monad.Trans.State.Lazy

type VPos = (VFloat,VFloat,VFloat,VFloat)
data FragmentStream a = FragmentStream [(a, FragmentStreamData)]


newtype ToFragment a b = ToFragment (Kleisli (State Int) a b) deriving (Category, Arrow)

class FragmentInput a where
    type FragmentFormat a
    toFragment :: ToFragment a (FragmentFormat a)  
    
instance FragmentInput VFloat where
        type FragmentFormat VFloat = FFloat
        toFragment = ToFragment $ Kleisli $ \ (S s) -> do n <- get
                                                          put (n+1)
                                                          return $ S $ useFInput "vf" STypeFloat n s 
               

rasterize:: forall p a. (PrimitiveTopology p, FragmentInput a)
          => Side 
          -> VertexStream p (VPos, a)
          -> FragmentStream (FragmentFormat a) -- FBool {-- TODO make struct of all sorts of stuff --}, 
rasterize side (VertexStream xs) = 
    let ToFragment (Kleisli m) = toFragment :: ToFragment a (FragmentFormat a)
        f ((p, x),s) = (evalState (m x) 0, FragmentStreamData side (makePos p) s)
        makePos (S x,S y,S z,S w) = do x' <- x
                                       y' <- y
                                       z' <- z
                                       w' <- w
                                       tellAssignment' "gl_Position" $ "vec4("++x'++',':y'++',':z'++',':w'++")"
    in FragmentStream $ map f xs

-- TODO: Add scissor and viewport, make it a Frame arrow
 
{-
import Graphics.GPipe.Stream
import Graphics.GPipe.GeometryStream
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.Class as T (lift)
import qualified Data.IntSet as Set
import Data.SNMap (runSNMapReaderT)
import Data.Text.Lazy.Builder
import Data.StableFunctor (unStableFunctor, makeStableFunctor)

class FragmentInput a where
    type FragmentFormat a
    toFragment :: ToFragment a (FragmentFormat a)  

-- TOFRAGMENT:
-- (Generate b's (ie Shader values with "varying"s))
-- (Generate set of neededVarying to Shader(Output))
-- Generate vertex shader code that assigns a to varyingname and then return varying name in fragmentshader 

data ToFragment a b = ToFragment 
                        (Kleisli (State [((ShaderM (), ShaderGlobDeclM ()), ShaderGlobDeclM ())]) a b) -- list is per varyingname backward, ie last tripple is for varying 0. Tripple is (vertexshaderCode to assign to varying, globalshadercode to declare used varying in vertex, ...and in fragment)
                        

instance FragmentInput VInt32 where
        type FragmentFormat VInt32 = FInt32
        toFragment = ToFragment $ Kleisli $ \ sx -> do s <- get
                                                       let i = length s
                                                           vName = "var" ++ show i
                                                           assign = do x' <- unS sx
                                                                       tellAssignment' vName x'
                                                           decl1 = tellGlobalLn $ "out " ++ "int " ++ vName 
                                                           decl2 = tellGlobalLn $ "in " ++ "int " ++ vName
                                                       put $ ((assign, decl1), decl2):s
                                                       return $ S $ do T.lift $ modify $ \ st -> st {shaderUsedVaryings = Set.insert i $ shaderUsedVaryings st}                
                                                                       return vName 



vposShader (x,y,z,w) = do x' <- unS x
                          y' <- unS y
                          z' <- unS z
                          w' <- unS w
                          tellAssignment' "gl_Position" ("vec4(" ++ x' ++ ',' : y' ++ ',' : z' ++ ',' : w' ++")")


--TODO: Fix rasterize: (runToFragment on undefined input first for the sake of output, then on real input to get state result)
rasterize:: forall p a fr. (PrimitiveTopology p, FragmentInput a)
          => Stream fr p (VertexPosition, a) 
          -> Side
          -> Stream fr Fragments (FBool {-- TODO make struct of all sorts of stuff --}, FragmentFormat a)
rasterize = let ToFragment (Kleisli m) = toFragment :: ToFragment a (FragmentFormat a)
                b = evalState (m (undefined :: a)) []
            in \(Stream sa sd) side -> 
                let f (k, u, sa, PrimitiveStreamData inpF uBinds sBinds) =                       
                        --TODO get a stable name for k and store it in FragmentStreamData for later 
                        let (vpos, a) = unStableFunctor k
                            g v = let (vShDecl, fShs) = unzip $ selectReversedIndexed v $ execState (m a) []
                                      (vShs, vDecls) = unzip vShDecl
                                      q = do (st, main) <- runShaderM $ sequence_ vShs >> vposShader vpos
                                             let (inpGlobM, inpIo) = inpF $ shaderInputNameToIndex st
                                                 (vsUDecls, vsUioFs) = unzip $ selectReversedIndexed (shaderUsedUniformBlocks st) uBinds
                                                 (vsSDecls, vsSioFs) = unzip $ selectReversedIndexed (shaderUsedSamplers st) sBinds
                                                 vDecl = inpGlobM >> sequence_ vsUDecls >> sequence_ vsSDecls >> sequence_ vDecls
                                                 runAll ms cs = evalStateT (mapM_ ($cs) ms) 0
                                                 io cs = do inpIo
                                                            runAll vsUioFs cs 
                                                            runAll vsSioFs cs
                                             return (makeShader vDecl main, io)
                                  in (sequence_ fShs, q) 
                        in (makeStableFunctor (undefined, b), u, sa, FragmentStreamData g [] [])
                in Stream undefined undefined --map f (zip sa sd)
                

-- TODO
rasterizeGeometries :: (PrimitiveTopology p, FragmentInput a)
          => Side 
          -> Stream fr Geometries (VertexList p (VertexPosition, a))
          -> Stream fr Fragments (FBool, FragmentFormat a)
rasterizeGeometries = undefined
        
-}


































