module Sound.RTG.Geometria.Euclidean (Euclidean, (<+>)) where

data Euclidean = Euclidean Int Int Int deriving (Ord)

-- | Implements an equivalence relation for euclidean rhythms
-- at the type level
instance Eq Euclidean where
  Euclidean k n p == Euclidean k' n' p' = stdForm (k, n, p) == stdForm (k', n', p')

instance Show Euclidean where
  show (Euclidean k n p) = show $ stdForm (k, n, p)

-- | Euclidean rhythms in "standard form" are elements of
-- \(\{ (x,y,z) | x,y,z >= 0 , y >= z \}\).
stdForm :: (Int, Int, Int) -> (Int, Int, Int)
stdForm (k, n, p) = (k', n', p')
  where
    k'
      | k == 0 = 0
      | k `mod` n' == 0 = n'
      | otherwise = k `mod` n'
    n'
      | k == 0 && p == 0 = 1
      | otherwise = abs n
    p' = p `mod` n'

-- Isochronous rhythms have an infinite number
-- of different representations that sound the same:
-- (3,3,0), (3,6,0), (3,9,0), ...
-- (5,5,1), (5,10,1), (5,15,1), ...
-- This elements are distinguished
-- to preserve granularity.
-- The exception is (0,n,0), which is the identity element
-- for every n.

-- | Combines two euclidean rhythms using modular arithmetic
-- on the least common multiple of pulse granularity.
(<+>) :: Euclidean -> Euclidean -> Euclidean
Euclidean k n p <+> Euclidean k' n' p'
  | (n /= 0) && (n' /= 0) = Euclidean ((k + k') `mod` m) m ((position + position') `mod` m)
  | otherwise = error "Zero pulse argument not specified"
  where
    m = lcm n n'
    position = p * div m n
    position' = p' * div m n'

infixl 5 <+>
