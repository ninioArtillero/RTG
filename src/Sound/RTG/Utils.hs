-- |
-- Module      : Utils
-- Description : Utility functions
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.Utils where

import qualified Data.Set as Set

-- | Returns a list of rational values in ascending order and without duplicates,
-- and wrapped inside the interval [0,1) with 1 is excluded.
-- In effect, this normalizes cyclic time.
stdForm :: [Rational] -> [Rational]
stdForm = setNub . map modOne

-- | Returns a list in ascending order without duplicates.
setNub :: (Ord a) => [a] -> [a]
setNub = Set.toAscList . Set.fromList

-- | Rationals wrapped onto [0,1).
modOne :: (RealFrac a) => a -> a
modOne = go . snd . properFraction
  where
    go x = if x >= 0 then x else 1 + x

patternEventDurationSec :: (RealFrac a, Integral b) => a -> b -> a
patternEventDurationSec cps patternLength = secondsPerCycle / eventsPerCycle
  where
    secondsPerCycle = 1 / cps
    eventsPerCycle = fromIntegral patternLength

interpolate :: (Fractional a, Integral n) => a -> a -> n -> [a]
interpolate x y n =
  if n > 0
    then x : map (\index -> x + ((fromIntegral index) / (fromIntegral n - 1)) * (y - x)) [1 .. n - 1]
    else error "Utils.interpolate: negative list length"
