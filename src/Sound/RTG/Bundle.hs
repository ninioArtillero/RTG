{-# LANGUAGE FunctionalDependencies #-}

-- | WIP: Abstract the computational pattern of the Sequencer + Pattern Bundle
-- implementation.
-- TODO: Research the categorical equivalent of a Fiber/Principal Bundle.
module Sound.RTG.Bundle where

import Data.Foldable (toList)
import Data.Group

-- | A /fiber bundle/ is a topological space @e@ provided of a projection
-- onto a base space @b@ that decomposes @e@ in fibers defined by the
-- inverse image of the projection.
--
-- prop> lift . projection = id
class FiberBundle b f e | e -> f, e -> b where
  projection :: e (f b) -> b
  fibers :: e (f b) -> f b
  lift :: b -> e (f b)

-- | A parameterized standard instance.
instance (Foldable f, Foldable e, Monoid b) = FiberBundle b f e where
projection = foldl <> mempty . concat .  (map . toList)
fibers = foldl <> mempty . toList


class (Foldable f, Foldabe e, Monoid b) => Fibrates

-- projection bundle = foldMap orbit bundle
-- lift :: b -> f b

-- | A /principal bundle/ is a /fibre bundle/ provided of a /group/ action.
class (Group (g b), FiberBundle b g e) => PrincipalBundle b g e where
  action :: g b -> e (g b) -> e (g b)

alignFibers ::
  (Foldable f, Functor f, Foldable e, Functor e, Monoid m) =>
  e (f m) -> e [m]
alignFibers e =
  let lengthsOfFibers = fmap length e
      leastCommonMultipleLength = foldl' lcm 1 lengthsOfFibers
   in fmap (align leastCommonMultipleLength) e

align :: (Integral n, Foldable f, Monoid m) => n -> f m -> [m]
align n ms = concatMap (\x -> x : replicate (fromIntegral $ n - 1) mempty) (toList ms)
