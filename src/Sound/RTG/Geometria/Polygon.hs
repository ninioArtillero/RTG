{-|
Module      : Polygon
Description : Generation of irreducibly periodic and perfectly balanced rhythms
Copyright   : (c) Xavier Góngora, 2023
License     : GPL-3
Maintainer  : ixbalanque@protonmail.ch
Stability   : experimental

Generation of irreducibily periodic and perfectly balaced rhythms
using linear sums of polygons. In the simplest case they can be thought of as /displaced polyrhythms/: the combination
of isochronous beats of different and coprime interonset intervals displaced so that they never
coincide. None trivial structures araise when weigthed sums are allowed.

Based on the heuristics found in:
Milne, Andrew, David Bulger, Steffen Herff, y William Sethares. 2015.
“Perfect balance: A novel principle for the construction of musical scales and meters”.
In Mathematics and computation in music: 5th international conference, MCM 2015;
proceedings, 97–108. Lecture notes in computer science 9110. London, UK.
https://doi.org/10.1007/978-3-319-20603-5.
-}
module Sound.RTG.Geometria.Polygon {-(Polygon, polygonPattern, polygonPatternSum, rotateLeft, rotateRight)-} where

import Sound.RTG.Ritmo.Pattern ( Pattern, rotateLeft )
import Foreign.C (throwErrnoPath)

-- TODO:
-- Al definir un polígono hay que ver la manera de generalizar su posición
-- módulo rotaciones (para evitar que se superpongan).
-- Además, hay que cuidar cuando la suma de polígonos forman otro polígono regular
-- que no es coprimo con alguno de los considerados.
-- La generalización a sumas no disjuntas permite dar pesos a los polígonos, siempre que
-- la combinación lineal resultante sólo contenga 1 y 0.

-- | The Polygon data type is used to represent regular polygons on a discrete space in the circle.
data Polygon = Polygon Pulses Onsets Position

type Pulses = Int
type Onsets = Int
type Position = Int

instance Show Polygon where
  show = show . polygonPattern

-- TODO: Comparar con factors de Data.Numbers
divisors :: Int -> [Int]
divisors n = [k | k <- [2 .. (n - 1)], n `rem` k == 0]

-- Coprime Disjoint Regular Polygons

-- | Produces a list of @1@ and @0@ representing a @k@-gon in circular @n@-space
-- It only has @0@ when @k@ is less than @2@ or doesn't divide @n@.
-- Empty for @n <= 0@.
polygonPattern :: Polygon -> Pattern Int
polygonPattern (Polygon n k p)
  | k >= 2 && n `rem` k == 0 = rotateLeft p . concat . replicate k $ side
  | otherwise = replicate n 0
  where
    subperiod = n `quot` k
    side = 1 : replicate (subperiod - 1) 0

-- | The list obtained by adding two polygons pointwise when in the same @n@-space.
polygonPatternSum :: Polygon -> Polygon -> Maybe (Pattern Int)
polygonPatternSum p1@(Polygon n k p) p2@(Polygon n' k' p') =
  if n == n'
    then Just $ patternSum pttrn1 pttrn2
    else Nothing
  where
    pttrn1 = polygonPattern p1
    pttrn2 = polygonPattern p2
    patternSum = zipWith (+)

-- | Polygon sum restricted to disjoint polygons in the same @n@-space.
polygonPatternSumRestricted :: Polygon -> Polygon -> Maybe (Pattern Int)
polygonPatternSumRestricted p1@(Polygon n k p) p2@(Polygon n' k' p') =
  if compatiblePatterns pttrn1 pttrn2
    then Just $ patternSum pttrn1 pttrn2
    else Nothing
  where
    pttrn1 = polygonPattern p1
    pttrn2 = polygonPattern p2
    patternSum = zipWith (+)
    compatiblePatterns xs ys =
      n == n' &&
      2 `notElem` patternSum xs ys
