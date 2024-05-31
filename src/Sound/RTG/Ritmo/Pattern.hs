-- | The pattern type and some basic operation
module Sound.RTG.Ritmo.Pattern where

import           Data.List                   (nub, sort)
import           Sound.RTG.Ritmo.RatioDecons (modOne)

-- | Se utiliza tiempo racional para aprovechar su correlación con el pensamiento musical y
-- para preservar la precisión, postergando los cálculos con flotantes.
type Time = Rational

type Pattern a = [a]

-- Pattern operations

rotateLeft :: Int -> Pattern a -> Pattern a
rotateLeft _ [] = []
rotateLeft n xs = zipWith const (drop n (cycle xs)) xs

rotateRight :: Int -> Pattern a -> Pattern a
rotateRight _ [] = []
rotateRight n xs = take size $ drop (size - (n `mod` size)) (cycle xs)
  where
    size = length xs

patternSum :: Num a => Pattern a -> Pattern a -> Pattern a
patternSum = zipWith (+)

diffPattern :: Pattern a -> Pattern a -> Pattern a
diffPattern xs ys
  | lx > ly = drop ly xs
  | otherwise = drop lx ys
  where
    lx = length xs
    ly = length ys

-- Auxiliary functions

-- | A pattern of time values in ascending order and without duplicates,
-- and wrapped inside the interval [0,1) with 1 is excluded.
-- In effect, this normalizes cyclic time.
stdForm :: Pattern Time -> Pattern Time
stdForm = sort . nub . map modOne

startPosition :: (Eq a, Monoid a) => Pattern a -> Pattern a
startPosition [] = []
startPosition pttrn@(x:xs)
  | null (reduceEmpty pttrn) = []
  | x == mempty = startPosition $ rotateLeft 1 pttrn
  | otherwise = pttrn

-- | Steps away from the first onset
position :: (Eq a, Monoid a) => Pattern a -> Int
position xs
  | null xs = 0
  | let [x] = take 1 xs in x /= mempty = 0
  | otherwise = 1 + position (drop 1 xs)

reduceEmpty :: (Eq a, Monoid a) => Pattern a -> Pattern a
reduceEmpty []           = []
reduceEmpty pttrn@(x:xs) = if x == mempty then reduceEmpty xs else pttrn
