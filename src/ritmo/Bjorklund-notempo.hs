import           System.Environment (getArgs)

-- Este programa ejecuta con dos argumentos para generar un ritmo euclidiano
-- secuenciado cíclicamente.

-- Partir de aquí para ir generando un patrón cíclico con otro programa que llama
-- a este para generar sus valores bajo demanda.
main :: IO ()
main = do
  (k:n:_) <- getArgs
  let onsets = read k :: Int
      pulses = read n :: Int
      pttrn = {-cycle $-} euclideanPattern onsets pulses
  sequence_ (map (putStr.show) pttrn)


euclideanPattern :: Int -> Int -> [Int]
euclideanPattern onsets pulses = concat $ bjorklund front back
  where front = take onsets $ repeat [1]
        back = take (pulses - onsets) $ repeat [0]

bjorklund :: [[Int]] -> [[Int]] -> [[Int]]
bjorklund front back
  | (length back) > 1 = bjorklund newFront newBack
  | otherwise = front ++ back
    where
      newFront = zipWith (++) front back
      newBack = unPaired front back

-- función auxiliar para bjorklund
unPaired :: [a] -> [a] -> [a]
unPaired xs ys
  | lx > ly  = drop ly xs
  | otherwise = drop lx ys
    where lx = length xs
          ly = length ys
