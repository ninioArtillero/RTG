-- |
-- Module      : Bjorklund
-- Description : The Björklund algorithm for generating euclidean patterns
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- The Björklund algorithm is described in:
--
-- Toussaint, Godfried. 2005. “The Euclidean Algorithm Generates Traditional Musical Rhythms.”
-- In Renaissance Banff: Mathematics, Music, Art, Culture,
-- edited by Reza Sarhangi and Robert V. Moody, 47–56.
-- Southwestern College, Winfield, Kansas: Bridges Conference.
-- http://archive.bridgesmathart.org/2005/bridges2005-47.html.
module Sound.RTG.Bjorklund (euclideanPattern) where

import Sound.RTG.List (backDiff)

-- | Generates the euclidean pattern \((k,n)\) in default position.
-- Expects @k > 0 && n > 0 && n > k@.
euclideanPattern :: Int -> Int -> [Int]
euclideanPattern onsets pulses = concat $ bjorklund front back
  where
    front = replicate onsets [1]
    back = replicate (pulses - onsets) [0]

-- Las siguientes implementaciones difieren en
-- su tratamiento de valores negativos.

{-@ type Pos = { n:Int | n >0 }@-}

{-@ euclideanPattern :: Pos -> Pos -> Pos @-}

-- | 'euclideanPattern', but if @k >= n@ in \((k,n)\) then
-- produces an n-isochronous rhythm.
-- Expects @k > 0 && n > 0@.
euclideanPattern' :: Int -> Int -> [Int]
euclideanPattern' onsets pulses =
  if onsets < pulses then concat $ bjorklund front back else replicate pulses 1
  where
    front = replicate onsets [1]
    back = replicate (pulses - onsets) [0]

-- | 'euclideanPattern', but a negative /pulses/ value @k@ in \((k,n)\)
-- reverses the pattern.
-- TODO: review case logic
euclideanPattern'' :: Int -> Int -> [Int]
euclideanPattern'' onsets pulses =
  if orientation > 0
    then concat $ bjorklund front back
    else reverse $ concat $ bjorklund front back
  where
    orientation = signum pulses
    onsets' = if onsets /= pulses then onsets `rem` pulses else abs onsets
    front = replicate onsets' [1]
    back = replicate (abs $ pulses - onsets') [0]

-- | The Björklund algorithm.
bjorklund :: [[Int]] -> [[Int]] -> [[Int]]
bjorklund front back =
  if (not . null) front && length back > 1
    then bjorklund newFront newBack
    else front ++ back
  where
    newFront = zipWith (++) front back
    newBack = backDiff front back
