-- | This module implements linear sums of polygons
-- to generate perfectly balaced rhythms.

module Sound.RTG.Geometria.Polygon (polygon, rotateLeft, rotateRight, polygonSum) where

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

type Pulses = Int
type Onsets = Int
type Position = Int

-- Un polígono de k vértices en un universo discreto de dimensión n.
polygon :: Pulses -> Onsets -> Position -> Pattern Int
polygon n k p
  | k > n = []
  | n `rem` k == 0 = rotateLeft p . concat . replicate k $ side
  | otherwise = []
  where
    subperiod = n `quot` k
    side = 1 : replicate (subperiod - 1) 0

rotateLeft :: Int -> [a] -> [a]
rotateLeft _ [] = []
rotateLeft n xs = zipWith const (drop n (cycle xs)) xs

rotateRight :: Int -> [a] -> [a]
rotateRight _ [] = []
rotateRight n xs = take size $ drop (size - (n `mod` size)) (cycle xs)
  where
    size = length xs

-- TODO: Comparar con factors de Data.Numbers
divisors :: Int -> [Int]
divisors n = [k | k <- [2 .. (n - 1)], n `rem` k == 0]

polygonSum :: Pulses -> Onsets -> Onsets -> Pattern Int
polygonSum pulses n m =
  if compatibleIndicators polygon1 polygon2
    then sumIndicators polygon1 polygon2
    else polygon1
  where
    polygon1 = polygon pulses n 0
    polygon2 = polygon pulses m 0

sumIndicators :: [Int] -> [Int] -> [Int]
sumIndicators = zipWith (+)

compatibleIndicators :: [Int] -> [Int] -> Bool
compatibleIndicators xs ys = 2 `notElem` sumIndicators xs ys
