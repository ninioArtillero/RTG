import           Control.Concurrent
import           Control.Monad      (forever, forM_)
import           System.Environment (getArgs)

-- Este programa ejecuta con tres argumentos:
-- cps :: Float, onsets :: Int, pulsos :: Int

-- Utiliza MVar, una implementación de variables mutables que
-- permite sincronizar procesos concurrentes.

main :: IO ()
main = do
  (c:k:n:_) <- getArgs
  -- argument parsing
  let cps = read c :: Float
      onsets = read k :: Int
      pulses = read n :: Int
      dur = eventDurationMs cps pulses
      pttrn = cycle ( euclideanPattern onsets pulses )
  -- initialize variables
  container <- newEmptyMVar
  index <- newMVar 0
  -- write loop
  forkIO . forever $ do
    i <- takeMVar index
    putMVar container ( pttrn !! i )
    putMVar index ( i + 1 )
    threadDelay dur
  -- output loop
  forever $ do
    x <- takeMVar container
    let message = messageTrigger x
    forM_ message print

messageTrigger :: Int -> Maybe Bool
messageTrigger x
  | x == 1 = Just True
  | x == 0 = Just False
  | otherwise = Nothing


-- Calcula la duración de cada pulso del ritmo euclidiano
eventDurationMs :: Float -> Int -> Int
eventDurationMs cps pulses = round ( milliSecondsPerCycle / cyclePartition )
      where milliSecondsPerCycle = (1/cps) * 10^6
            cyclePartition = fromIntegral pulses

euclideanPattern :: Int -> Int -> [Int]
euclideanPattern onsets pulses = concat $ bjorklund front back
  where front = replicate onsets [1]
        back = replicate (pulses - onsets) [0]

bjorklund :: [[Int]] -> [[Int]] -> [[Int]]
bjorklund front back
  | (length back) > 1 = bjorklund newFront newBack
  | otherwise = front ++ back
    where
      newFront = zipWith (++) front back
      newBack = diffList front back

-- función auxiliar para bjorklund
diffList :: [a] -> [a] -> [a]
diffList xs ys
  | lx > ly  = drop ly xs
  | otherwise = drop lx ys
    where lx = length xs
          ly = length ys
