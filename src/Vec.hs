{-# options_ghc -Wno-orphans #-}

module Vec (VectorSpace(..), Vec, V2(..), downVec, upVec, rotVec, Linear.distance, Linear.sumV) where

--------------------------------------------------------------------------------
import Linear (V2(..))
import qualified Linear -- TODO remove this dependency, it's overkill and needs a long time to compile
import qualified Linear.Affine
import qualified Linear.Metric
import Control.Applicative
--------------------------------------------------------------------------------

class (Eq a, Floating a) => VectorSpace v a | v -> a where
  zeroV :: v
  (^+^) :: v -> v -> v
  (*^) :: a -> v -> v
  (^/) :: v -> a -> v
  neg :: v -> v
  dot :: v -> v -> a
  norm :: v -> a
  normalize :: v -> v

  (^-^) :: v -> v -> v
  v ^-^ u = v ^+^ neg u

(^*) :: VectorSpace v a => v -> a -> v
(^*) = flip (*^)

instance VectorSpace Float Float where
  zeroV       = 0
  (^+^)       = (+)
  (*^)        = (*)
  (^/)        = (/)
  neg x       = (-x)
  dot         = (*)
  norm        = id
  normalize _ = fromInteger 1

instance VectorSpace Double Double where
  zeroV       = 0
  (^+^)       = (+)
  (*^)        = (*)
  (^/)        = (/)
  neg x       = (-x)
  dot         = (*)
  norm        = id
  normalize _ = fromInteger 1

type Vec = V2 Float

downVec :: Vec
downVec = V2 0 (-1)
upVec :: Vec
upVec = V2 0 1

instance (Linear.Epsilon a, Eq a, Floating a) => VectorSpace (V2 a) a where
  zeroV = Linear.zero
  (*^) = (Linear.*^)
  (^+^) = (Linear.^+^)
  (^-^) = (Linear.^-^)
  (^/) = (Linear.^/)
  neg (V2 x y) = V2 (-x) (-y)
  dot = Linear.Metric.dot
  norm = Linear.Metric.norm
  normalize = Linear.Metric.normalize

-- Rotate a vector clockwise, given an angle in degrees.
rotVec :: Float -> Vec -> Vec
rotVec theta (V2 x y) = V2
  (x * cos theta' - y * sin theta')
  (x * sin theta' + y * cos theta')
  where theta' = (-theta) * (pi / 180)
