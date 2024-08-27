module Sound.RTG.Tiempo.OscOut where

import           Control.Concurrent
import           Control.Monad                   (forM_, forever)
import           GHC.IO                          (unsafePerformIO)
import           Sound.OSC.FD
import           Sound.RTG.Ritmo.RhythmicPattern (Pattern, Rhythm (..),
                                                  Rhythmic (..), Binary(..), RhythmicPattern)
import           System.Environment              (getArgs)

type CPS = Rational
type SampleName = String

-- Utiliza MVar, una implementación de variables mutables que
-- permite sincronizar procesos concurrentes.

globalCPS :: MVar CPS
{-# NOINLINE globalCPS #-}
globalCPS = unsafePerformIO $ newMVar 0.4

setcps :: CPS -> IO ()
setcps newcps = do
  swapMVar globalCPS newcps
  return ()

patternStream :: SampleName -> Pattern Binary -> IO ThreadId
patternStream sample pttrn = forkIO $ do
  let cyclicPattern = cycle pttrn
  -- initialize variables
  port <- openUDP "127.0.0.1" 57120 :: IO UDP
  onsetContainer <- newEmptyMVar
  index <- newMVar 0
  -- pattern query
  forkIO . forever $ do
    i <- takeMVar index
    putMVar onsetContainer (cyclicPattern !! i)
    putMVar index (i + 1)
  -- output OSC event messages
  forever $ do
    onset <- takeMVar onsetContainer
    cps <- readMVar globalCPS
    let send = sendMessage port
        event = messageGen onset sample
        dur = eventDuration cps (length pttrn)
    send event
    pauseThread dur

play :: Rhythmic a => SampleName -> a -> IO ()
play sample pttrn = do
  threadId <- patternStream sample . getRhythm . toRhythm $ pttrn
  print threadId

-- Usé OSCFunc.trace(true) en SuperCollider para ver la estructura
-- del mensaje OSC generado en Tidal Cycles por: once $ s "sn"
-- Esta estructura esta definida en el módulo Sound.Tidal.Stream
-- De esta manera, tengo un mensaje que SuperDirt entiende para producir sonido.
messageGen :: Binary -> SampleName -> Message
messageGen Zero _ = message "/dirt/play" []
messageGen One sample = message "/dirt/play"
 [ -- ASCII_String $ ascii "cps",
   -- Float 0.5,
   -- ASCII_String $ ascii "cycle",
   -- Float 0.0,
   -- ASCII_String $ ascii "delta",
   -- Float 1.7777760028839,
   ASCII_String $ ascii "s",
   ASCII_String $ ascii sample
  ]

eventDuration :: Rational -> Int -> Rational
eventDuration cps pulses = secondsPerCycle / eventsPerCycle
  where
    secondsPerCycle = 1 / cps
    eventsPerCycle = fromIntegral pulses
