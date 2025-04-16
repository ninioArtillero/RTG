-- |
-- Module      : PerfectBalance
-- Description : Compute the evenness and balance of a pattern
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- Computing the evenness and balance of a pattern (list) of rational numbers representing
-- a rhythmic pattern in a discrete /chromatic/ universe in the circle. Reference:
--
-- Milne, Andrew, David Bulger, Steffen Herff, and William Sethares. 2015.
-- “Perfect Balance: A Novel Principle for the Construction of Musical Scales and Meters.”
-- In Mathematics and Computation in Music: 5th International Conference, MCM 2015; Proceedings, 97–108.
-- Lecture Notes in Computer Science 9110. London, UK.
-- https://doi.org/10.1007/978-3-319-20603-5.
module Sound.RTG.PerfectBalance (evenness, balance, indicatorVector) where

import Data.Complex (Complex (..), magnitude)
import Data.Ratio (denominator, numerator, (%))
import Sound.RTG.Utils (stdForm)

gcdRational :: Rational -> Rational -> Rational
gcdRational x y =
  gcd (numerator x) (numerator y) % lcm (denominator x) (denominator y)

gcdRationals :: [Rational] -> Rational
gcdRationals = foldr gcdRational 0

-- | Mínima subdivisión regular discreta del intervalo [0,1)
-- que contiene a un patrón.
-- The number of different rotations of a pattern
-- matches the cardinality of the chromatic universe,
-- as each element is surrounded by a unique interval sequence.
chromaticUniverse :: [Rational] -> [Rational]
chromaticUniverse xs =
  let n = denominator $ gcdRationals xs in [k % n | k <- [0 .. (n - 1)]]

-- | Transforms a list of rationals representing a pattern in a
-- discrete chromatic universe in the circle in to a binary list of 'Int's.
indicatorVector :: [Rational] -> [Int]
indicatorVector xs =
  [if x `elem` stdForm xs then 1 else 0 | x <- chromaticUniverse xs]

-- | Complex Unit
i :: Complex Double
i = 0 :+ 1

-- | Maps a list of rationals to the unit circule on the complex plane.
scaleVector :: [Rational] -> [Complex Double]
scaleVector = map (exp . (2 * pi * i *) . fromRational)

-- | Calculate the discrete fourier transformar (DFT) coeficients.
dft :: Int -> [Complex Double] -> Complex Double
dft t zs = sum terms / dimension
  where
    terms =
      map
        ( \(n, z) ->
            z
              * exp
                ((-2) * pi * i * fromIntegral t * (fromIntegral n / dimension))
        )
        $ indexList zs
    dimension = fromIntegral (length zs)

indexList :: [b] -> [(Int, b)]
indexList = zip [0 ..]

-- | Computes the evennes of a pattern. The evenness is equal to
-- the magnitud of the DFT's first coeficient.
evenness :: [Rational] -> Double
evenness = magnitude . dft 1 . scaleVector

-- | Computes the balance of a pattern. The balance is equal to
-- 1 minus the magnitude of the DFT's 0 coeficient.
balance :: [Rational] -> Double
balance = (1 -) . magnitude . dft 0 . scaleVector

-- | A variant of 'balance' using the indicatorVector.
-- TODO: compare implementations speed.
balance' :: [Rational] -> Double
balance' pat =
  let indicator = map fromIntegral $ indicatorVector pat
      elements = fromIntegral $ length (stdForm pat)
      dimension = fromIntegral $ length indicator
      scaleFactor = (dimension / elements)
   in (1 -) . (scaleFactor *) . magnitude . dft 1 $ indicator
