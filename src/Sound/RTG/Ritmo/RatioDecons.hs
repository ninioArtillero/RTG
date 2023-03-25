{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
-- | Pattern synonym para desestructurar rationales
-- Ocupa las extensiones PatternSynonyms y ViewPatterns.
-- Referencias:
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms
module RatioDecons
    ( modOne
    ) where

import           Data.Ratio (Ratio, denominator, numerator, (%))

numDenum :: Integral a => Ratio a -> (a, a)
numDenum x = (numerator x, denominator x)

pattern (:%) :: Integral a => a -> a -> Ratio a
pattern a :% b <- (numDenum -> (a,b)) where
    a :% b = a % b

-- | Racionales módulo uno
modOne :: Rational -> Rational
-- TODO: error 'unmatched patterns' lsp
modOne (x :% y) = if x < y then x % y else (x - y) % y
