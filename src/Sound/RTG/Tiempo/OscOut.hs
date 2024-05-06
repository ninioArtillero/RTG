module Sound.RTG.Tiempo.OscOut where

import           Control.Concurrent
import           Control.Monad                   (forM_, forever)
import           Sound.OSC.FD
import           Sound.RTG.Ritmo.Pattern         (Pattern)
import           Sound.RTG.Ritmo.RhythmicPattern (Rhythmic (..), toInts)
import           System.Environment              (getArgs)

type CPS = Rational
type SampleName = String

-- Utiliza MVar, una implementación de variables mutables que
-- permite sincronizar procesos concurrentes.

play :: CPS -> SampleName -> Pattern Int -> IO ()
play cps sample pttrn = do
  let cyclicPattern = cycle pttrn
      dur = eventDurationS cps (length pttrn)
  -- initialize variables
  port <- openUDP "127.0.0.1" 57120 :: IO UDP
  container <- newEmptyMVar
  index <- newMVar 0
  -- write loop
  forkIO . forever $ do
    i <- takeMVar index
    putMVar container (cyclicPattern !! i)
    putMVar index (i + 1)
  -- open connection
  -- output loop
  forever $ do
    x <- takeMVar container
    let send = sendMessage port
        message = messageGen x sample
    send message
    pauseThread dur

-- Usé OSCFunc.trace(true) en SuperCollider para ver la estructura
-- del mensaje OSC generado en Tidal Cycles por: once $ s "sn"
-- Esta estructura esta definida en el módulo Sound.Tidal.Stream
-- De esta manera, tengo un mensaje que SuperDirt entiende para producir sonido.
messageGen :: Integral a => a -> SampleName -> Message
messageGen x sample =
  if x == 1
    then
      message
        "/dirt/play"
        [ ASCII_String $ ascii "cps",
          Float 0.5,
          ASCII_String $ ascii "cycle",
          Float 0.0,
          ASCII_String $ ascii "delta",
          Float 1.7777760028839,
          ASCII_String $ ascii "s",
          ASCII_String $ ascii sample
        ]
    else
      message
        "/dirt/play"
        [ ASCII_String $ ascii "cps",
          Float 0.5,
          ASCII_String $ ascii "cycle",
          Float 0.0,
          ASCII_String $ ascii "delta",
          Float 1.7777760028839,
          ASCII_String $ ascii "s",
          ASCII_String $ ascii "~"
        ]

eventDurationMs :: Rational -> Int -> Int
eventDurationMs cps pulses = round (microSecondsPerCycle / cyclePartition)
  where
    microSecondsPerCycle = (1 / cps) * 10 ^ 6
    cyclePartition = fromIntegral pulses

eventDurationS :: Rational -> Int -> Rational
eventDurationS cps pulses = secondsPerCycle / cyclePartition
  where
    secondsPerCycle = 1 / cps
    cyclePartition = fromIntegral pulses

-- | Provisional to play Rhythmic values fast and easy.
playR :: Rhythmic -> IO()
playR = play 0.4 "cp" . toInts . pttrn
