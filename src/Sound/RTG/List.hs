-- |
-- Module      : List
-- Description : Utility functions for lists
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.List where

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
rotateLeft n xs =
  if n >= 0
    then zipWith const (drop n (cycle xs)) xs
    else rotateRight (negate n) xs

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
startPosition [] = []
startPosition pttrn@(x : xs)
  | null (dropIdentities pttrn) = []
  | x == mempty = startPosition $ rotateLeft 1 pttrn
  | otherwise = pttrn

-- | Steps away from the first onset
position :: (Eq a, Monoid a) => [a] -> Int
position xs
  | null xs = 0
  | let [x] = take 1 xs in x /= mempty = 0
  | otherwise = 1 + position (drop 1 xs)

-- | Drops all identity elements from the beginning of the list
dropIdentities :: (Eq a, Monoid a) => [a] -> [a]
dropIdentities [] = []
dropIdentities pttrn@(x : xs) = if x == mempty then dropIdentities xs else pttrn
