-- | This module implements linear sums of polygons
-- to generate perfectly balaced rhythms.
--
-- Based on the heuristics found in:
-- Milne, Andrew, David Bulger, Steffen Herff, y William Sethares. 2015.
-- “Perfect balance: A novel principle for the construction of musical scales and meters”.
-- In Mathematics and computation in music: 5th international conference, MCM 2015;
-- proceedings, 97–108. Lecture notes in computer science 9110. London, UK.
-- https://doi.org/10.1007/978-3-319-20603-5.


module Sound.RTG.Geometria.Polygon {-(Polygon, polygonPattern, polygonPatternSum, rotateLeft, rotateRight)-} where

import Sound.RTG.Ritmo.Pattern ( Pattern, rotateLeft )

-- TODO: Combinaciones lineales de polígonos que sean
-- (1) disjuntos para preservar el balance
-- (2) Coprimos para evitar subperiodos.
-- Al definir un polígono hay que ver la manera de generalizar su posición
-- módulo rotaciones (para evitar que se superpongan).
-- Además, hay que cuidar cuando la suma de polígonos forman otro polígono regular
-- que no es coprimo con alguno de los considerados.
-- La generalización en Milne et. al.  artículo permite dar pesos a los polígonos, siempre que
-- la combinación lineal resultante sólo contenga 1 y 0.

data Polygon = Polygon Pulses Onsets Position

type Pulses = Int
type Onsets = Int
type Position = Int

instance Show Polygon where
  show = show . polygonPattern

-- TODO: Comparar con factors de Data.Numbers
divisors :: Int -> [Int]
divisors n = [k | k <- [2 .. (n - 1)], n `rem` k == 0]
--Copŕime Disjoint Regular Polygons

-- A k-gon in a n-space
polygonPattern :: Polygon -> Pattern Int
polygonPattern (Polygon n k p)
  | k > n = []
  | n `rem` k == 0 = rotateLeft p . concat . replicate k $ side
  | otherwise = []
  where
    subperiod = n `quot` k
    side = 1 : replicate (subperiod - 1) 0



-- | Suma de polygonos en el mismo espacio discreto
polygonPatternSum :: Polygon -> Polygon -> Maybe (Pattern Int)
polygonPatternSum p1@(Polygon n k p) p2@(Polygon n' k' p') =
  if n == n
    then Just $ patternSum pttrn1 pttrn2
    else Nothing
  where
    pttrn1 = polygonPattern p1
    pttrn2 = polygonPattern p2
    patternSum = zipWith (+)

-- | Suma de polígonos, restringida a figuras disjuntas
polygonPatternSumRestricted :: Polygon -> Polygon -> Maybe (Pattern Int)
polygonPatternSumRestricted p1 p2 =
  if compatiblePatterns pttrn1 pttrn2
    then Just $ patternSum pttrn1 pttrn2
    else Nothing
  where
    pttrn1 = polygonPattern p1
    pttrn2 = polygonPattern p2
    patternSum = zipWith (+)
    compatiblePatterns xs ys =
      length xs == length ys &&
      2 `notElem` patternSum xs ys
