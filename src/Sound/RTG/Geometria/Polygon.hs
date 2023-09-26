-- | This module implements linear sums of polygons
-- to generate perfectly balaced rhythms.

module Sound.RTG.Geometria.Polygon {-(Polygon, polygonPattern, polygonPatternSum, rotateLeft, rotateRight)-} where

import Sound.RTG.Ritmo.Pattern (Pattern)

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

-- Un polígono de k vértices en un universo discreto de dimensión n.
polygonPattern :: Polygon -> Pattern Int
polygonPattern (Polygon n k p)
  | k > n = []
  | n `rem` k == 0 = rotateLeft p . concat . replicate k $ side
  | otherwise = []
  where
    subperiod = n `quot` k
    side = 1 : replicate (subperiod - 1) 0


rotateLeft :: Int -> Pattern a -> Pattern a
rotateLeft _ [] = []
rotateLeft n xs = zipWith const (drop n (cycle xs)) xs

rotateRight :: Int -> Pattern a -> Pattern a
rotateRight _ [] = []
rotateRight n xs = take size $ drop (size - (n `mod` size)) (cycle xs)
  where
    size = length xs

-- TODO: Comparar con factors de Data.Numbers
divisors :: Int -> [Int]
divisors n = [k | k <- [2 .. (n - 1)], n `rem` k == 0]

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
