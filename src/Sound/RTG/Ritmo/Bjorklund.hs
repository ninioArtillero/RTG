import           Control.Concurrent
import           Control.Monad      (forever)
import           System.Environment (getArgs)

-- Este programa ejecuta con tres argumentos:
-- cps :: Float, onsets :: Int, pulsos :: Int

-- Utiliza MVar, una implementación de variables mutables que
-- permite sincronizar procesos concurrentes.

main :: IO ()
main = do
  (c:k:n:_) <- getArgs
  let cps = read c :: Float
      onsets = read k :: Int
      pulses = read n :: Int
      dur = eventDurationMs cps pulses
      pttrn = cycle ( euclideanPattern onsets pulses )
  container <- newEmptyMVar
  index <- newMVar 0
  forkIO . forever $ do
    i <- takeMVar index
    putMVar container ( pttrn !! i )
    putMVar index ( i + 1 )
    threadDelay dur
  forever $ do
    val <- takeMVar container
    print val


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
bjorklund front back =
  if (length back) > 1
    then bjorklund newFront newBack
    else front ++ back
  where newFront = zipWith (++) front back
        newBack = diffList front back

-- función auxiliar para bjorklund
diffList :: [a] -> [a] -> [a]
diffList xs ys =
  if lx > ly
    then drop ly xs
    else drop lx ys
  where lx = length xs
        ly = length ys
