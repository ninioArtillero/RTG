import           Control.Concurrent
import           Control.Monad      (forever, liftM2, replicateM, replicateM_)
import           System.IO




main = do
  -- Pasar el resultado directo a stdout
  hSetBuffering stdout NoBuffering

  -- Canal para almacenar las salidas de los threads
  chan <- newChan :: IO (Chan String)

  let cyc = 10
  let numbers = [5,7]

  mapM_ (numberStream chan cyc) numbers

{-
  forever $ do
    val <- readChan chan
    putStr val
-}

------------------------
{-

-- Explorando una alternativa

  chan1 <- newChan
  chan2 <- newChan

  let cyc = 10

  _ <- numberStream chan1 cyc 5
  _ <- numberStream chan2 cyc 7

  forever $ do
    val1 <- readChan chan1
    val2 <- readChan chan2
    print (val1 ++ " - " ++ val2)

-}

-- forever $ liftM2 (++) (readChan chan1) (readChan chan2) >>= print
  forever $ do
    values <- replicateM 2 (readChan chan)
    let joinedValues = foldr1 joinValues values
    print joinedValues

joinValues :: String -> String -> String
joinValues x y = x ++ " - " ++ y

numberStream :: Chan String -> Float -> Int -> IO ThreadId
numberStream chan cyc num =
  forkIO $ forever $ do
  writeChan chan (show num)
  threadDelay $ eventDuration cyc num

eventDuration :: Float -> Int -> Int
eventDuration cyc num = round ( milliSecondsPerCycle / cyclePartition )
      where milliSecondsPerCycle = cyc * 1000000
            cyclePartition = fromIntegral num -- hacerlo Float para usarlo en la división
