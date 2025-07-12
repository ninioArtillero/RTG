-- |
-- Module      : UnSafe
-- Description : Operations manipulating global mutable state in non-functional style using 'unsafePerformIO'.
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.UnSafe (setcps, setbpm, readcps) where

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
import Sound.Osc.Fd
  ( Message,
    message,
    pauseThread,
    sendMessage,
  )
import Sound.RTG.Event (Event, isOnset)
import Sound.RTG.OscMessages (superDirtMessage, superDirtPort)
import Sound.RTG.RhythmicPattern
  ( Rhythmic,
    rhythm,
  )
import Sound.RTG.Utils (patternEventDurationSec)
import System.IO.Unsafe (unsafePerformIO)

type CPS = Rational

type SampleName = String

-- | Beats Per Minute.
type BPM = Int

-- | Beats Per Cycle.
type BPC = Int

-- TODO: Change to global state module?

-- TODO: Clean up pending... many functions not in used and
-- repeated functionality.

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

patternStream :: SampleName -> [Event] -> IO ThreadId
patternStream sample pttrn = forkIO $ do
  let cyclicPattern = cycle pttrn
  -- initialize variables
  port <- superDirtPort
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
        dur = patternEventDurationSec cps (length pttrn)
    send event
    pauseThread dur

-- | Plays a sample in the given pattern. The returned 'ThreadId' must be bound
-- to a variable to be able to stop the thread. These binding are lost when @ghci@ is reloaded.
playU :: (Rhythmic a) => SampleName -> a -> IO ThreadId
playU sample pttrn = do
  threadId <- patternStream sample . rhythm $ pttrn
  putStrLn $ "New pattern running at " ++ show threadId
  return threadId

stop :: ThreadId -> IO ()
stop = killThread

-- Usé OSCFunc.trace(true) en SuperCollider para ver la estructura
-- del mensaje OSC generado en Tidal Cycles por: once $ s "sn"
-- Esta estructura esta definida en el módulo Sound.Tidal.Stream
-- De esta manera, tengo un mensaje que SuperDirt entiende para producir sonido.
messageGen :: Event -> SampleName -> Message
messageGen event sample =
  if isOnset event
    then superDirtMessage sample
    else message "/dirt/play" []

-- Requires GHC.Conc.listThreads available since base-4.18.0.0
-- Still not working as expected and imposes a conservative upperbound base-4.21.0.0)
-- due the instability of the module (as mentioned in the documentation).
-- NOTE: Not working for unknown reason. Modify base dependency bounds as mentioned before
-- for testing.
-- stopAll :: IO ()
-- stopAll = listThreads >>= mapM_ killThread
