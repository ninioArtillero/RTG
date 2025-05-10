-- |
-- Module      : List
-- Description : Utility functions for lists
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.List where

import qualified Data.List as List

backDiff :: [a] -> [a] -> [a]
backDiff xs ys
  | lx > ly = drop ly xs
  | otherwise = drop lx ys
  where
    lx = length xs
    ly = length ys

frontDiff :: [a] -> [a] -> [a]
frontDiff xs ys
  | lx > ly = take (lx - ly) xs
  | otherwise = take (ly - lx) ys
  where
    lx = length xs
    ly = length ys

rotateLeft :: Int -> [a] -> [a]
rotateLeft _ [] = []
rotateLeft 0 list = list
rotateLeft n list =
  if n >= 0
    then zipWith const (drop n (cycle list)) list
    else rotateRight (negate n) list

rotateRight :: Int -> [a] -> [a]
rotateRight _ [] = []
rotateRight n xs =
  if n >= 0
    then take size $ drop (size - (n `mod` size)) (cycle xs)
    else rotateLeft (negate n) xs
  where
    size = length xs

listSum :: (Num a) => [a] -> [a] -> [a]
listSum = zipWith (+)

-- Functions with Monoid constraint

-- | Rotate a list so that it starts with a non-identity element
startPosition :: (Eq a, Monoid a) => [a] -> [a]
startPosition list = rotateLeft (position list) list

-- | Steps away from the first onset
position :: (Eq a, Monoid a) => [a] -> Int
position = length . takeWhile (== mempty)

-- | Groups each non-empty value with the empty values after it.
groupWithMempty :: (Eq a, Monoid a) => [a] -> [[a]]
groupWithMempty = List.groupBy nextIsMempty . startPosition
  where
    nextIsMempty _ y = y == mempty

-- | Factors out superflous empty values (like 'Sound.RTG.Rest') from a list.
compact :: (Eq a, Monoid a) => [a] -> [a]
compact [] = []
compact xs =
  let isolatedEvents = groupWithMempty xs
      eventDurations = map length isolatedEvents
      listGCD = foldr1 gcd eventDurations
      divisions = map (`div` listGCD) eventDurations
   in concat $ zipWith (take) divisions isolatedEvents

-- | Resize a list's length to a given number if the length divides it.
-- Otherwise return the list unaltered.
resize :: (Monoid a) => Int -> [a] -> [a]
resize _ [] = []
resize n xs =
  if n > 0 && modulus == 0
    then concatMap (: replicate (division - 1) mempty) xs
    else xs
  where
    len = length xs
    (division, modulus) = n `divMod` len

-- | A reflection at the center of the circle, i.e. 180 degree rotation.
-- Lists represent a /discrete universe/ wrapped around the circle.
-- Impair lists need to be doubled in size to ocuppy the proper positions.
-- TODO: Use to implement rhythmic pattern inverse.
centerReflection :: (Monoid a) => [a] -> [a]
centerReflection xs =
  if modulus == 0
    then rotateLeft (division) xs
    else rotateLeft (2 * division) $ resize (2 * len) xs
  where
    len = length xs
    (division, modulus) = len `divMod` 2
