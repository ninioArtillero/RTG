-- | Utility functions

module Sound.RTG.Internal.Utils where

import qualified Data.Set as Set

setNub :: Ord a => [a] -> [a]
setNub = Set.toAscList . Set.fromList

-- |Rationals wrapped onto [0,1)
modOne :: Rational -> Rational
modOne = go . snd . properFraction
  where go x = if x >= 0 then x else 1 + x
