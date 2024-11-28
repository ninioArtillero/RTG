{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Pattern synonym to decompose rationals
-- References:
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms
module Sound.RTG.Rhythm.RatioDecons (modOne) where

import           Data.Ratio (Ratio, denominator, numerator, (%))

-- TODO: This module is mostly deprecated and should be cleaned of
-- unnecessary functions. Nevertheless, they might still be used as a
-- show cased of the imported language extensions and might provide
-- a faster implementation.

numDenum :: Integral a => Ratio a -> (a, a)
numDenum x = (numerator x, denominator x)

pattern (:%) :: Integral a => a -> a -> Ratio a
pattern a :% b <-
  (numDenum -> (a, b))
  where
    a :% b = a % b

modOne' :: Rational -> Rational
modOne' = modOne'' . abs

modOne'' :: Rational -> Rational
modOne'' (x :% y) = if x < y then x % y else modOne' $ (x - y) % y

-- |Rationals wrapped onto [0,1)
modOne :: Rational -> Rational
modOne = go . snd . properFraction
  where go x = if x >= 0 then x else 1 + x
