-- | Esta implementación obedece a la siguiente especificacion:
-- 1. (0,1,0) es la identidad
-- 2. (0,n,0) sólo módifica la granularidad al transformar a otro ritmo.
-- 3. El inverso de (k,n,p) es (-k,n,-p)
-- 4. Los valores negativos de k se traducen por aritmética modular sobre n.
-- 5. Los valores negativos de n significan un cambio de orientación en el ritmo,
--    de manera que (k,-n,p) == (k, n, -p)
module Sound.RTG.Geometria.Euclidean (Euclidean (..), e, invert) where

import Data.Group
import Sound.RTG.Ritmo.Pattern (rotateLeft)
import Sound.RTG.Ritmo.Bjorklund (euclideanPattern)

data Euclidean = Euclidean Onsets Pulses Position deriving (Ord)

type Onsets = Int

type Pulses = Int

type Position = Int

-- | Implements an equivalence relation for euclidean rhythms
-- at the type level, based on a representation function.
instance Eq Euclidean where
  Euclidean k n p == Euclidean k' n' p' =
    simpleForm (k, n, p) == simpleForm (k', n', p')

instance Show Euclidean where
  show (Euclidean k n p) = show $ rotateLeft p $ euclideanPattern k n

-- Euclidean rhythms in its stardard semantics are elements of
-- \(\{ (x,y,z) | x,z >= 0 , y > 0, y >= z \}\).
-- Analogous to the "prime form" in pitch-class theory.
-- The following functions are used to try different equivalence
-- relantions among euclidean rhythms in terms of form representation.

-- | A simple representation based on standard semantics and modular aritmetic.
-- It excludes full isocronous rhythms (FIR) where all pulses are onsets.
simpleForm :: (Int, Int, Int) -> (Int, Int, Int)
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
-- allow full isochronous rhythms(FIR).
altForm :: (Int, Int, Int) -> (Int, Int, Int)
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
(<+>) :: Euclidean -> Euclidean -> Euclidean
Euclidean k n p <+> Euclidean k' n' p'
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

infixl 5 <+>

instance Semigroup Euclidean where
  a <> b = a <+> b

instance Monoid Euclidean where
  mempty = Euclidean 0 1 0

instance Group Euclidean where
  invert (Euclidean a b c) = Euclidean (- a) b (- c)

-- | The interface function to construct euclidean rhythms
-- (the value constructor is not exported).
e :: (Int, Int,  Int) -> Euclidean
e (x,y,z) = Euclidean x y z

-- TODO: Aplicar patrón de "smart constructor" y utilizar newtype para evitar errores de argumentos
