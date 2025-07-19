{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Euclidean
-- Description : An euclidean rhythm type using an abstract tuplet
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- The implementation of the group instance for euclidean rhythms
-- has the following specification:
--
--   1. @(0,1,0)@ in the identity element ('mempty').
--   2. @(0,n,0)@ modifies the granularity of another rhythm
--   3. The inverse of @(k,n,p)@ is @(-k,n,-p)@
--   4. Onset (@k@) and position (@p@) values are converted to their representation /modulo/ $n$
--   5. Negative values of @n@ mean a change in orientation of the position value @p@,
--   such that @(k,-n,p) == (k, n, -p)@. TODO: Could change to semantics of TiledMusic for
--   a negative tile.
module Sound.RTG.Euclidean (Euclidean, e) where

import Data.Group (Group (..))
import Sound.RTG.Bjorklund (euclideanPattern)
import Sound.RTG.Event (integralsToEvents)
import Sound.RTG.List (rotateLeft)
import Sound.RTG.RhythmicPattern (Pattern (..), Rhythmic (..))

-- | Euclidean rhythms, in its stardard semantics, are elements of
-- \(\{ (x,y,z) | x,z >= 0 , y > 0, y >= z \}\).
-- Analogous to the "prime form" in pitch-class theory.
data Euclidean = Euclidean !Onsets !Pulses !Position deriving (Ord)

type Onsets = Integer

type Pulses = Integer

type Position = Integer

-- | Implements an equivalence relation for euclidean rhythms
-- at the type level, based on a simplified form.
instance Eq Euclidean where
  Euclidean k n p == Euclidean k' n' p' =
    simpleForm (k, n, p) == simpleForm (k', n', p')

instance Show Euclidean where
  show (Euclidean k n p) = "Euclidean Rhythm: " ++ (show . simpleForm $ (k, n, p))

{- To show onset pattern (can be uninformative):
show (Euclidean k n p) = (description ++) . show . rotateLeft (fromIntegral p) $
    euclideanPattern (fromIntegral k) (fromIntegral n)
  where description = if p' == 0
                      then "Euclidean Rhythm: "
                      else "Euclidean Rhythm (in position " ++ show p' ++ "):"
        p' = p `mod` n
 -}

-- The following functions are used to try different equivalence
-- relations among euclidean rhythms in terms of form representation.

-- | A simple representation based on common euclidean rhythm usage and modular aritmetic.
-- It excludes full isocronous rhythms, /i.e./ where all pulses are onsets.
simpleForm :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
simpleForm (k, n, p) = (k', n', p')
  where
    k' = k `mod` n'
    n' = abs n
    p' = p `mod` n'

-- Isochronous rhythms have an infinite number
-- of different representations that sound the same:
-- (3,3,0), (3,6,0), (3,9,0), ...
-- (5,5,1), (5,10,1), (5,15,1), ...
-- Distinction can be account as granularity information.
-- In particular rhythms of the form (0,n,0)
-- would only contain granurality information.
-- The problem with this approach is that, with the proposed operation,
-- the identity would not be unique.

-- | A modified representation to make the identity unique and
-- allow full isochronous rhythms.
altForm :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
altForm (k, n, p) = (k', n', p')
  where
    k'
      | k == 0 = 0 -- only 0 onsets are empty
      | k `mod` n' == 0 = n' -- multiples of n become FIR
      | otherwise = k `mod` n
    n'
      | k == 0 && p == 0 = 1
      | otherwise = abs n
    p' = p `mod` n'

-- | Combines two euclidean rhythms using modular arithmetic
-- on the least common multiple of pulse granularity.
euclideanProduct :: Euclidean -> Euclidean -> Euclidean
euclideanProduct (Euclidean k n p) (Euclidean k' n' p')
  | (n /= 0) && (n' /= 0) =
      Euclidean ((k + k') `mod` grain) grain ((position + position') `mod` grain)
  | otherwise = error "No defined semantics for zero pulse euclidean rhythms"
  where
    grain = lcm n n'
    position =
      let scaleFactor = grain `div` n
       in (p `mod` n) * scaleFactor
    position' =
      let scaleFactor' = grain `div` n'
       in (p' `mod` n') * scaleFactor'

instance Semigroup Euclidean where
  a <> b = euclideanProduct a b

instance Monoid Euclidean where
  mempty = Euclidean 0 1 0

instance Group Euclidean where
  invert (Euclidean a b c) = Euclidean (-a) b (-c)

instance Rhythmic Euclidean where
  toRhythm (Euclidean k n p) = Pattern . integralsToEvents . rotateLeft (fromIntegral p) $ euclideanPattern (fromIntegral k) (fromIntegral n)

-- | Allow pairs and triplets as arguments to construct Euclidean Rhythms
class EuclideanArgument a where
  arg :: a -> (Integer, Integer, Integer)

instance EuclideanArgument (Integer, Integer, Integer) where
  arg = id

instance EuclideanArgument (Integer, Integer) where
  arg (x, y) = (x, y, 0)

-- | Construct and euclidean rhythm specified by @(n,k)@ or @(n,k,p)@
-- integer tuples in the set @[(k,n,p) | n >= 0, k <- [0..n], p <- [0..n]]@.
e :: (EuclideanArgument a) => a -> Euclidean
e tripleOrDuple = Euclidean x y z
  where
    (x, y, z) = arg tripleOrDuple
