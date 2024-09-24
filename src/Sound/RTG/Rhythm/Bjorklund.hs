module Sound.RTG.Rhythm.Bjorklund (euclideanPattern) where

euclideanPattern :: Int -> Int -> [Int]
euclideanPattern onsets pulses = bjorklund front back
  where
    front = replicate onsets [1]
    back = replicate (pulses - onsets) [0]

-- Las siguientes implementaciones difieren en
-- su tratamiento de valores negativos.

euclideanPattern' :: Int -> Int -> [Int]
euclideanPattern' onsets pulses =
  case (compare onsets pulses) of
    LT -> bjorklund front back
    GT -> replicate pulses 1
    EQ -> replicate onsets 1
  where
    front = replicate onsets [1]
    back = replicate (pulses - onsets) [0]

euclideanPattern'' :: Int -> Int -> [Int]
euclideanPattern'' onsets pulses =
  if orientation > 0
    then bjorklund front back
    else reverse $ bjorklund front back
  where
    orientation = signum pulses
    onsets' = if onsets /= pulses then onsets `rem` pulses else abs onsets
    front = replicate onsets' [1]
    back = replicate (abs $ pulses - onsets') [0]

bjorklund :: [[Int]] -> [[Int]] -> [Int]
bjorklund front back
  | (not . null) front && length back > 1 = bjorklund newFront newBack
  | otherwise = concat (front ++ back)
  where
    newFront = zipWith (++) front back
    newBack = diffList front back

-- Versión previa, sin concat
bjorklund' :: [[Int]] -> [[Int]] -> [[Int]]
bjorklund' front back =
  if (length back) > 1
    then bjorklund' newFront newBack
    else front ++ back
  where
    newFront = zipWith (++) front back
    newBack = diffList front back

-- Función auxiliar para bjorklund
diffList :: [a] -> [a] -> [a]
diffList xs ys
  | lx > ly = drop ly xs
  | otherwise = drop lx ys
  where
    lx = length xs
    ly = length ys
