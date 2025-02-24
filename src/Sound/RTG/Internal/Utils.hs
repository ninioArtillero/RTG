{-|
Module      : Utils
Description : Utility functions
Copyright   : (c) Xavier Góngora, 2023
License     : GPL-3
Maintainer  : ixbalanque@protonmail.ch
Stability   : experimental
-}
module Sound.RTG.Internal.Utils where

import qualified Data.Set as Set

-- | Returns a list of rational values in ascending order and without duplicates,
-- and wrapped inside the interval [0,1) with 1 is excluded.
-- In effect, this normalizes cyclic time.
stdForm :: [Rational] -> [Rational]
stdForm = setNub . map modOne

-- | Returns a list in ascending order without duplicates.
setNub :: Ord a => [a] -> [a]
setNub = Set.toAscList . Set.fromList

-- | Rationals wrapped onto [0,1).
modOne :: Rational -> Rational
modOne = go . snd . properFraction
  where go x = if x >= 0 then x else 1 + x
