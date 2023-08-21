module Sound.RTG.Geometria.Euclidean (Euclidean, (<+>)) where

type Euclidean =
  ( Int, -- event number
    Int, -- pulse granularity
    Int -- rotation
  )

-- | Produces an Euclidean representation such that
-- the Euclidean rhythms are the set
-- \(\{ (x,y,z) | x,y,z >= 0 , y >= z \}\).
reduce :: Euclidean -> Euclidean
reduce (x, y, z) = (x', y', z')
  where
    x'
      | x == 0 = 0
      | x `mod` y' == 0 = y'
      | otherwise = x `mod` y'
    y' = abs y
    z' = z `mod` y'

-- Isochronous rhythms have an infinite number
-- of different representations that sound the same:
-- (3,3), (3,6), (3,9), ...
-- (5,5), (5,10), (5,15), ...
-- This families are not reduced to a single element
-- to preserve granularity.

-- | Combines two euclidean rhythms using modular arithmetic
-- on the least common multiple of pulse granularity.
(<+>) :: Euclidean -> Euclidean -> Euclidean -- type declaration
r1 <+> r2
  | (y /= 0) && (b /= 0) = ((x + a) `mod` w, w, (z' + c') `mod` w)
  | otherwise = error "Zero pulse argument not specified"
  where
    (a, b, c) = reduce r1
    (x, y, z) = reduce r2
    w = lcm y b
    z' = z * div w y
    c' = c * div w b
