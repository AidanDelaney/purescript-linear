module Linear.V2 where

-- $setup
-- >>> import Control.Lens

-- | A 2-dimensional vector
--
-- >>> pure 1 :: V2 Int
-- V2 1 1
--
-- >>> V2 1 2 + V2 3 4
-- V2 4 6
--
-- >>> V2 1 2 * V2 3 4
-- V2 3 8
--
-- >>> sum (V2 1 2)
-- 3

import Prelude

data V2 a = V2 a a

instance ftorV2 :: Functor V2 where
  map f (V2 a b) = V2 (f a) (f b)

instance applyV2 :: Apply V2 where
  apply (V2 f g) (V2 a b) = V2 (f a) (g b)

instance applicativeV2 :: Applicative V2 where
  pure a = V2 a a

instance bindV2 :: Bind V2 where
  bind (V2 a b) f = V2 a' b' where
                      V2 a' _ = f a
                      V2 _ b' = f b

instance monadV2 :: Monad V2

instance showV2 :: (Show a) => Show (V2 a) where
  show (V2 a b) = "V2 " <> (show a) <> " " <> (show b)