{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, EmptyDataDecls, TypeFamilies, GADTs #-}
module Graphics.GPipe.Internal.PrimitiveArray where

import Graphics.GPipe.Internal.Buffer
import Graphics.GPipe.Internal.Shader
import Data.Monoid
import Data.IORef

import Data.Word

import           Graphics.GL.Core33
import           Graphics.GL.Types

-- | A vertex array is the basic building block for a primitive array. It is created from the contents of a 'Buffer', but unlike a 'Buffer',
--   it may be truncated, zipped with other vertex arrays, and even morphed into arrays of a different type with the provided 'Functor' instance.
--   A @VertexArray t a@ has elements of type @a@, and @t@ indicates whether the vertex array may be used as instances or not.
data VertexArray t a = VertexArray  {
    -- | Retrieve the number of elements in a 'VertexArray'.
    vertexArrayLength :: Int,
    vertexArraySkip :: Int,
    bArrBFunc:: BInput -> a
    }

-- | A phantom type to indicate that a 'VertexArray' may only be used for instances (in 'toPrimitiveArrayInstanced' and 'toPrimitiveArrayIndexedInstanced').
data Instances

-- | Create a 'VertexArray' from a 'Buffer'. The vertex array will have the same number of elements as the buffer, use 'takeVertices' and 'dropVertices' to make it smaller.
newVertexArray :: Buffer os a -> Render os (VertexArray t a)
newVertexArray buffer = Render $ return $ VertexArray (bufferLength buffer) 0 $ bufBElement buffer

instance Functor (VertexArray t) where
    fmap f (VertexArray n s g) = VertexArray n s (f . g)

-- | Zip two 'VertexArray's using the function given as first argument. If either of the argument 'VertexArray's are restriced to 'Instances' only, then so will the resulting
--   array be, as depicted by the 'Combine' type family.
zipVertices :: (a -> b -> c) -> VertexArray t a -> VertexArray t' b -> VertexArray (Combine t t') c
zipVertices h (VertexArray n s f) (VertexArray m t g) = VertexArray (min n m) totSkip newArrFun
    where totSkip = min s t
          newArrFun x = let baseSkip = bInSkipElems x - totSkip in h (f x { bInSkipElems = baseSkip + s}) (g x { bInSkipElems = baseSkip + t})

type family Combine t t' where
    Combine () Instances = Instances
    Combine Instances () = Instances
    Combine Instances Instances = Instances
    Combine () () = ()

-- | @takeVertices n a@ creates a shorter vertex array by taking the @n@ first elements of the array @a@.
takeVertices :: Int -> VertexArray t a -> VertexArray t a
takeVertices n (VertexArray l s f) = VertexArray (min (max n 0) l) s f

-- | @dropVertices n a@ creates a shorter vertex array by dropping the @n@ first elements of the array @a@. The argument array @a@ must not be
--   constrained to only 'Instances'.
dropVertices :: Int -> VertexArray () a -> VertexArray t a
dropVertices n (VertexArray l s f) = VertexArray (l - n') (s+n') f
        where
            n' = min (max n 0) l

-- | @replicateEach n a@ will create a longer vertex array, only to be used for instances, by replicating each element of the array @a@ @n@ times. E.g.
--   @replicateEach 3 {ABCD...}@ will yield @{AAABBBCCCDDD...}@. This is particulary useful before zipping the array with another that has a different replication rate.
replicateEach :: Int -> VertexArray t a -> VertexArray Instances a
replicateEach n (VertexArray m s f) = VertexArray (n*m) s (\x -> f $ x {bInInstanceDiv = bInInstanceDiv x * n})

type family IndexFormat a where
    IndexFormat (B Word32) = Word32
    IndexFormat (BPacked Word16) = Word16
    IndexFormat (BPacked Word8) = Word8

-- | An index array is like a vertex array, but contains only integer indices. These indices must come from a tightly packed 'Buffer', hence the lack of
--   a 'Functor' instance and no conversion from 'VertexArray's.
data IndexArray = IndexArray {
    iArrName :: IORef GLuint,
    -- | Numer of indices in an 'IndexArray'.
    indexArrayLength:: Int,
    offset:: Int,
    restart:: Maybe Int,
    indexType :: GLuint
    }

-- | Create an 'IndexArray' from a 'Buffer' of unsigned integers (as constrained by the closed 'IndexFormat' type family instances). The index array will have the same number of elements as the buffer, use 'takeIndices' and 'dropIndices' to make it smaller.
--   The @Maybe a@ argument is used to optionally denote a primitive restart index.
newIndexArray :: forall os f b a. (BufferFormat b, Integral a, IndexFormat b ~ a) => Buffer os b -> Maybe a -> Render os IndexArray
newIndexArray buf r = let a = undefined :: b in Render $ return $ IndexArray (bufName buf) (bufferLength buf) 0 (fmap fromIntegral r) (getGlType a)

-- | @takeIndices n a@ creates a shorter index array by taking the @n@ first indices of the array @a@.
takeIndices :: Int -> IndexArray -> IndexArray
takeIndices n i = i { indexArrayLength = min (max n 0) (indexArrayLength i) }

-- | @dropIndices n a@ creates a shorter index array by dropping the @n@ first indices of the array @a@.
dropIndices :: Int -> IndexArray -> IndexArray
dropIndices n i = i { indexArrayLength = l - n', offset = offset i + n' }
    where
        l = indexArrayLength i
        n' = min (max n 0) l

data Triangles
data Lines
data Points

data PrimitiveTopology p where
    TriangleList :: PrimitiveTopology Triangles
    TriangleStrip :: PrimitiveTopology Triangles
    TriangleFan :: PrimitiveTopology Triangles
    LineList :: PrimitiveTopology Lines
    LineStrip :: PrimitiveTopology Lines
    LineLoop :: PrimitiveTopology Lines
    PointList :: PrimitiveTopology Points

toGLtopology :: PrimitiveTopology p -> GLuint
toGLtopology TriangleList = GL_TRIANGLES
toGLtopology TriangleStrip = GL_TRIANGLE_STRIP
toGLtopology TriangleFan = GL_TRIANGLE_FAN
toGLtopology LineList = GL_LINES
toGLtopology LineStrip = GL_LINE_STRIP
toGLtopology LineLoop = GL_LINE_LOOP
toGLtopology PointList = GL_POINTS


{-
Some day:

instance PrimitiveTopology TrianglesWithAdjacency where
    toGLtopology TriangleStripWithAdjacency = 0
    data Geometry TrianglesWithAdjacency a = TriangleWithAdjacency a a a a a a

instance PrimitiveTopology LinesWithAdjacency where
    toGLtopology LinesWithAdjacencyList = 0
    toGLtopology LinesWithAdjacencyStrip = 1
    data Geometry LinesWithAdjacency a = LineWithAdjacency a a a a
-}

type InstanceCount = Int
type BaseVertex = Int

data PrimitiveArrayInt p a = PrimitiveArraySimple (PrimitiveTopology p) Int BaseVertex a
                           | PrimitiveArrayIndexed (PrimitiveTopology p) IndexArray BaseVertex a
                           | PrimitiveArrayInstanced (PrimitiveTopology p) InstanceCount Int BaseVertex a
                           | PrimitiveArrayIndexedInstanced (PrimitiveTopology p) IndexArray InstanceCount BaseVertex a

-- | An array of primitives
newtype PrimitiveArray p a = PrimitiveArray {getPrimitiveArray :: [PrimitiveArrayInt p a]}

instance Monoid (PrimitiveArray p a) where
    mempty = PrimitiveArray []
    mappend (PrimitiveArray a) (PrimitiveArray b) = PrimitiveArray (a ++ b)

instance Functor (PrimitiveArray p) where
    fmap f (PrimitiveArray xs) = PrimitiveArray  $ fmap g xs
        where g (PrimitiveArraySimple p l s a) = PrimitiveArraySimple p l s (f a)
              g (PrimitiveArrayIndexed p i s a) = PrimitiveArrayIndexed p i s (f a)
              g (PrimitiveArrayInstanced p il l s a) = PrimitiveArrayInstanced p il l s (f a)
              g (PrimitiveArrayIndexedInstanced p i il s a) = PrimitiveArrayIndexedInstanced p i il s (f a)

toPrimitiveArray :: PrimitiveTopology p -> VertexArray () a -> PrimitiveArray p a
toPrimitiveArray p va = PrimitiveArray [PrimitiveArraySimple p (vertexArrayLength va) (vertexArraySkip va) (bArrBFunc va (BInput 0 0))]
toPrimitiveArrayIndexed :: PrimitiveTopology p -> IndexArray -> VertexArray () a -> PrimitiveArray p a
toPrimitiveArrayIndexed p ia va = PrimitiveArray [PrimitiveArrayIndexed p ia (vertexArraySkip va) (bArrBFunc va (BInput 0 0))]
toPrimitiveArrayInstanced :: PrimitiveTopology p -> (a -> b -> c) -> VertexArray () a -> VertexArray t b -> PrimitiveArray p c
toPrimitiveArrayInstanced p f va ina = PrimitiveArray [PrimitiveArrayInstanced p (vertexArrayLength ina) (vertexArrayLength va) (vertexArraySkip va) (f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput (vertexArraySkip ina) 1))] -- Base instance not supported in GL < 4, so need to burn in
toPrimitiveArrayIndexedInstanced :: PrimitiveTopology p -> IndexArray -> (a -> b -> c) -> VertexArray () a -> VertexArray t b -> PrimitiveArray p c
toPrimitiveArrayIndexedInstanced p ia f va ina = PrimitiveArray [PrimitiveArrayIndexedInstanced p ia (vertexArrayLength ina) (vertexArraySkip va) (f (bArrBFunc va $ BInput 0 0) (bArrBFunc ina $ BInput (vertexArraySkip ina) 1))] -- Base instance not supported in GL < 4, so need to burn in
