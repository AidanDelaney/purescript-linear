module Linear.Vector where

import Prelude
import Control.Apply (lift2)
import Data.Lens
import Data.Complex
import Data.Floating

-- | Basis element
newtype E t = E { el :: forall x. Lens' (t x) x }

-- | A vector is an additive group with additional structure.
class Apply f <= Vector f where
  -- | The zero vector
  zero :: forall a. Floating a => f a

  -- | Compute the sum of two vectors
  --
  -- >>> V2 1 2 ^+^ V2 3 4
  -- V2 4 6
  vectorAdd :: forall a. Floating a => f a -> f a -> f a

  -- | Compute the difference between two vectors
  --
  -- >>> V2 4 5 ^-^ V2 3 1
  -- V2 1 4
  vectorDifference :: forall a. Floating a => f a -> f a -> f a

  -- | Linearly interpolate between two vectors.
  lerp :: forall a. Floating a => a -> f a -> f a -> f a

infixl 6 vectorAdd as ^+^
infixl 6 vectorDifference as ^-^

instance vectorComplex :: Vector Complex where
  zero = zero
  vectorAdd = lift2 (+)
  vectorDifference = lift2 (-)
  lerp x c1 c2 = c1 + offset
                 where
                   offset = pure x * (c1 - c2)