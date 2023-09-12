import Control.Concurrent
import Control.Monad (forever)

-- Ejemplo: Utilizar un índice contador para variar la entrada de una función
--
-- Es algo habitual en lenguajes imperativos. Sin embargo en Haskell,
-- contenerlo en la mónada IO creando variables mutables (MVar) con la librería
-- Control. Concurrent
-- Una MVar bloquea la escritura mientras su valor no haya sido extraído. Esto
-- permite la coordinación de hilos concurrentes (creados por la función forkIO).

main = do
  -- inicializar variables
  cajita <- newEmptyMVar
  index <- newMVar 0
  -- loop infinito:
  -- poner ternurita en cajita
  forkIO . forever $ do
    i <- takeMVar index
    putMVar cajita (ternurita i)
    putMVar index (i + 1)
    threadDelay $ 1000000 -- espera 1 segundo
    -- para sacar ternurita a stdout
  forever $ do
    ternurita <- takeMVar cajita
    putStrLn ternurita

-- Mi mascota par e impar
ternurita :: Int -> String
ternurita i =
  if (i `mod` 2 == 0)
    then "Memuffin"
    else "Kimbola"
