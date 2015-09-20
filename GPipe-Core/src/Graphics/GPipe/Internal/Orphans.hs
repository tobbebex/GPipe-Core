{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Graphics.GPipe.Internal.Orphans where

import Data.Boolean
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Plucker (Plucker(..))
import Linear.Quaternion (Quaternion(..))
import Linear.Affine (Point(..))

type instance BooleanOf (V0 a) = BooleanOf a
type instance BooleanOf (V1 a) = BooleanOf a
type instance BooleanOf (V2 a) = BooleanOf a
type instance BooleanOf (V3 a) = BooleanOf a
type instance BooleanOf (V4 a) = BooleanOf a
type instance BooleanOf (Plucker a) = BooleanOf a
type instance BooleanOf (Quaternion a) = BooleanOf a
type instance BooleanOf (Point f a) = BooleanOf (f a)

instance EqB a => EqB (V0 a) where
  V0 ==* V0 = true
  V0 /=* V0 = false
instance EqB a => EqB (V1 a) where
  V1 a ==* V1 x = a ==* x
  V1 a /=* V1 x = a /=* x
instance EqB a => EqB (V2 a) where
  V2 a b ==* V2 x y = a ==* x &&* b ==* y
  V2 a b /=* V2 x y = a /=* x ||* b /=* y
instance EqB a => EqB (V3 a) where
  V3 a b c ==* V3 x y z = a ==* x &&* b ==* y &&* c ==* z
  V3 a b c /=* V3 x y z = a /=* x ||* b /=* y ||* c /=* z
instance EqB a => EqB (V4 a) where
  V4 a b c d ==* V4 x y z w = a ==* x &&* b ==* y &&* c ==* z &&* d ==* w
  V4 a b c d /=* V4 x y z w = a /=* x ||* b /=* y ||* c /=* z ||* d /=* w
instance EqB a => EqB (Quaternion a) where
  Quaternion a v ==* Quaternion b u = a ==* b &&* v ==* u
  Quaternion a v /=* Quaternion b u = a /=* b ||* v /=* u
instance EqB a => EqB (Plucker a) where
  Plucker a b c d e f ==* Plucker x y z w u v = a ==* x &&* b ==* y &&* c ==* z &&* d ==* w &&* e ==* u &&* f ==* v
  Plucker a b c d e f /=* Plucker x y z w u v= a /=* x ||* b /=* y ||* c /=* z ||* d /=* w ||* e /=* u ||* f /=* v
instance EqB (f a) => EqB (Point f a) where
  P a ==* P x = a ==* x
  P a /=* P x = a /=* x
  
instance IfB a => IfB (V0 a) where
        ifB q _ _ = V0 
instance IfB a => IfB (V1 a) where
        ifB q (V1 a) (V1 x) = V1 (ifB q a x) 
instance IfB a => IfB (V2 a) where
        ifB q (V2 a b) (V2 x y) = V2 (ifB q a x) (ifB q b y) 
instance IfB a => IfB (V3 a) where
        ifB q (V3 a b c) (V3 x y z) = V3 (ifB q a x) (ifB q b y) (ifB q c z) 
instance IfB a => IfB (V4 a) where
        ifB q (V4 a b c d) (V4 x y z w) = V4 (ifB q a x) (ifB q b y) (ifB q c z) (ifB q d w) 
instance IfB a => IfB (Quaternion a) where
        ifB q (Quaternion a v) (Quaternion b u) = Quaternion (ifB q a b) (ifB q v u)
instance IfB a => IfB (Plucker a) where
        ifB q (Plucker a b c d e f) (Plucker x y z w u v) = Plucker (ifB q a x) (ifB q b y) (ifB q c z) (ifB q d w) (ifB q e u) (ifB q f v)
instance IfB (f a) => IfB (Point f a) where
        ifB q (P a) (P x) = P (ifB q a x)
