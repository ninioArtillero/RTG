{-# LANGUAGE Arrows #-}
-- | FRP pattern implementation for on-the-fly pattern transformation
module Sound.RTG.ReactivePattern where

import Control.Concurrent (threadDelay)
import FRP.Yampa
import qualified Sound.Osc as Osc hiding (Time)
import Sound.RTG.Time.Messages
import Sound.RTG.Rhythm.RhythmicPattern

-- Signal Function

type Pattern = [Int]      -- Binary list of 1s (onset) and 0s (rests)
type PatternSF = SF Time [Osc.Message]

-- Function to convert a binary pattern into a repeating stream of OSC messages
patternToSF :: Rhythmic a => a -> Double -> PatternSF
patternToSF pttrn bpm = proc time -> do
  let onsetPattern = getRhythm . toRhythm $ pttrn
      patternLength = length onsetPattern
      interval = 60.0 / bpm
  -- Compute the index within the pattern based on the current time
  index <- arr (\(t, i, l) -> floor (t / i) `mod` l) -< (time, interval, patternLength)
  -- Determine if the current index corresponds to an onset
  isOnset <- arr (\(i, onsets) -> onsets !! i == One) -< (index, onsetPattern)
  -- Generate an OSC message if there's an onset
  messages <- arr (\(t, onset) -> [oscMessage t | onset]) -< (time, isOnset)
  returnA -< messages


-- TODO: Find a logic to generate event streams to trigger
-- the transformation of patterns and cps
{-
p bpm pttrn = (patternToSF pttrn bpm &&& (holdl pttrn >>> updatePattern)) `switch` \e -> patternToSF e bpm

updatePattern :: Rhythmic a => SF a (Event a)
updatePattern = proc inp -> do
                  pttrn <- constant <- inp
                  returnA <- pttrn
-}


-- Function to generate an OSC message
oscMessage :: Time -> Osc.Message
oscMessage time = Osc.message "/playOnset" [Osc.float time]


-- Reactimate setup

-- Initialization function: just returns initial time
initSense :: IO Time
initSense = return 0

-- Sense function: calculates time delta between updates
sense :: Bool -> IO (DTime, Maybe Time)
sense _ = do
    threadDelay (1000000 `div` 20)  -- Update about 20 times per second
    let deltaTime = 1 / 20
    return (deltaTime, Just deltaTime)

-- Actuate function: here we output the OSC messages to the console
actuate :: Bool -> [Osc.Message] -> IO Bool
actuate _ messages = do
    mapM_ print messages  -- Print each message for now
    return False

-- Run a pattern with a given BPM
runPattern :: Rhythmic a => a -> Double -> IO ()
runPattern pat bpm = reactimate initSense sense actuate (patternToSF pat bpm)
