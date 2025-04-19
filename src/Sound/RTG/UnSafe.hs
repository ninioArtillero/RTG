-- |
-- Module      : UnSafe
-- Description : Operations manipulating global mutable state in non-functional style using 'unsafePerformIO'.
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.UnSafe (playU, stop, setcps, setbpm, readcps) where

import Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    killThread,
    -- myThreadId,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    swapMVar,
    takeMVar,
  )
-- import GHC.Conc (listThreads, threadStatus) -- for stopAll since base-4.18.0.0
import Control.Monad (forever)
import GHC.IO (unsafePerformIO)
import Sound.Osc.Fd
  ( Datum (AsciiString),
    Message,
    ascii,
    message,
    openUdp,
    pauseThread,
    sendMessage,
  )
import Sound.RTG.RhythmicPattern
  ( Event (..),
    Rhythm (..),
    Rhythmic (..),
  )
import Sound.RTG.Utils ()

type CPS = Rational

type SampleName = String

type Pattern a = [a]

type BPM = Int

-- | Beats per cycle
type BPC = Int

-- TODO: Clean up pending... many functions not in used and
-- repeated functionality.
-- Utiliza MVar, una implementación de variables mutables que
-- permite sincronizar procesos concurrentes.

globalCPS :: MVar CPS
{-# NOINLINE globalCPS #-}
globalCPS = unsafePerformIO $ newMVar 0.4

readcps :: IO CPS
readcps = readMVar globalCPS

setcps :: CPS -> IO ()
setcps newcps = do
  swapMVar globalCPS newcps
  return ()

setbpm :: BPC -> BPM -> IO ()
setbpm bpc bpm = do
  let newcps = (fromIntegral bpm / 60) / fromIntegral bpc
  swapMVar globalCPS newcps
  return ()

patternStream :: SampleName -> Pattern Event -> IO ThreadId
patternStream sample pttrn = forkIO $ do
  let cyclicPattern = cycle pttrn
  -- initialize variables
  port <- openUdp "127.0.0.1" 57120
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

playU :: (Rhythmic a) => SampleName -> a -> IO ThreadId
playU sample pttrn = do
  threadId <- patternStream sample . getRhythm . toRhythm $ pttrn
  putStrLn $ "New pattern running at " ++ show threadId
  return threadId

stop :: ThreadId -> IO ()
stop = killThread

-- Usé OSCFunc.trace(true) en SuperCollider para ver la estructura
-- del mensaje OSC generado en Tidal Cycles por: once $ s "sn"
-- Esta estructura esta definida en el módulo Sound.Tidal.Stream
-- De esta manera, tengo un mensaje que SuperDirt entiende para producir sonido.
messageGen :: Event -> SampleName -> Message
messageGen Rest _ = message "/dirt/play" []
messageGen Onset sample =
  message
    "/dirt/play"
    [ -- ASCII_String $ ascii "cps",
      -- Float 0.5,
      -- ASCII_String $ ascii "cycle",
      -- Float 0.0,
      -- ASCII_String $ ascii "delta",
      -- Float 1.7777760028839,
      AsciiString $ ascii "s",
      AsciiString $ ascii sample
    ]

eventDuration :: Rational -> Int -> Rational
eventDuration cps pulses = secondsPerCycle / eventsPerCycle
  where
    secondsPerCycle = 1 / cps
    eventsPerCycle = fromIntegral pulses

-- Requires GHC.Conc.listThreads available since base-4.18.0.0
-- Still not working as expected and imposes a conservative upperbound base-4.21.0.0)
-- due the instability of the module (as mentioned in the documentation).
-- NOTE: Not working for unknown reason. Modify base dependency bounds as mentioned before
-- for testing.
-- stopAll :: IO ()
-- stopAll = listThreads >>= mapM_ killThread
