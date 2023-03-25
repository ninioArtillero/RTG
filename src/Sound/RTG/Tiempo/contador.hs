import           Control.Concurrent
import           Control.Monad      (forever)

-- Ejemplo: Utilizar un índice contador como entrada de una función

main = do
  -- inicializar variables
  cajita <- newEmptyMVar
  index <- newMVar 0
  -- loop infinito
  -- poner ternurita en cajita
  forkIO . forever $ do
    i <- takeMVar index
    putMVar cajita ( ternuritas i )
    putMVar index ( i + 1 )
    threadDelay $ 1000000 -- espera 1 segundo
  -- sacar ternurita a terminal
  forever $ do
    ternurita <- takeMVar cajita
    putStrLn ternurita

-- Mi mascota par e impar
ternuritas :: Int  -> String
ternuritas i =
  if (i `mod` 2 == 0)
  then "Memuffin"
  else "Kimbola"
