import           Control.Concurrent
import           Control.Monad      (forM_, forever)
import           Sound.OSC.FD
import           System.Environment (getArgs)
import           Bjorklund          (euclideanPattern)

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
      dur = eventDurationS cps pulses
      pttrn = cycle ( euclideanPattern onsets pulses )
  -- initialize variables
  container <- newEmptyMVar
  index <- newMVar 0
  -- write loop
  forkIO . forever $ do
    i <- takeMVar index
    putMVar container ( pttrn !! i )
    putMVar index ( i + 1 )
    --threadDelay dur
  -- open connection
  port <- openUDP "127.0.0.1" 57120 :: IO UDP
  -- output loop
  forever $ do
    x <- takeMVar container
    let send = sendMessage port
        message = messageGen x
    send message
    pauseThread dur
    --tcp_send_packet t message


{- Versión para enviar a terminal
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
-}

-- Usé OSCFunc.trace(true) en SuperCollider para ver la estructura
-- del mensaje OSC generado en Tidal Cycles por: once $ s "sn"
-- Esta estructura esta definida en el módulo Sound.Tidal.Stream
-- De esta manera, tengo un mensaje que SuperDirt entiende para producir sonido.
messageGen :: Integral a => a -> Message
messageGen x =
  if x == 1
    then message "/dirt/play" [ASCII_String $ ascii "cps",
                               Float 0.5,
                               ASCII_String $ ascii "cycle",
                               Float 0.0,
                               ASCII_String $ ascii "delta",
                               Float 1.7777760028839,
                               ASCII_String $ ascii "s",
                               ASCII_String $ ascii "sn"]
    else message "/dirt/play" [ASCII_String $ ascii "cps",
                               Float 0.5,
                               ASCII_String $ ascii "cycle",
                               Float 0.0,
                               ASCII_String $ ascii "delta",
                               Float 1.7777760028839,
                               ASCII_String $ ascii "s",
                               ASCII_String $ ascii "tok"]


eventDurationMs :: Float -> Int -> Int
eventDurationMs cps pulses = round ( microSecondsPerCycle / cyclePartition )
      where microSecondsPerCycle = (1/cps) * 10^6
            cyclePartition = fromIntegral pulses

eventDurationS :: Float -> Int -> Float
eventDurationS cps pulses = secondsPerCycle / cyclePartition
      where secondsPerCycle = 1 / cps
            cyclePartition = fromIntegral pulses


eUnitTest :: (Int -> Int -> [Int]) -> String
eUnitTest f =
  if and [
    f 8 8 == [1,1,1,1,1,1,1,1],
    f 3 8 == [1,0,0,1,0,0,1,0],
    f 3 (-8) == [0,1,0,0,1,0,0,1],
    f (-3) 8 == [],
    f (-3) (-8) == [],
    f 11 8 == [1,1,1,1,1,1,1,1],
    f 11 (-8) == [],
    f (-11) 8 == [],
    f (-11) (-8) == []
    ]
     then "Rifo"
     else "Chafio"
