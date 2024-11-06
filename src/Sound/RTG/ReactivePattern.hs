-- | FRP pattern implementation for on-the-fly pattern transformation

module Sound.RTG.ReactivePattern where

import Control.Concurrent (threadDelay)
import FRP.Yampa
import Sound.OSC

-- Signal Function

type Pattern = [Int]      -- Binary list of 1s (onset) and 0s (rests)
type PatternSF = SF Time [Message]

-- Function to convert a binary pattern into a repeating stream of OSC messages
patternToSF :: Pattern -> Double -> PatternSF
patternToSF pat bpm = proc time -> do
    let cycleLen = length pat
        interval = 60.0 / bpm             -- Convert BPM to seconds per beat
        index = floor (time / interval) `mod` cycleLen
        isOnset = pat !! index == 1
        messages = if isOnset then [oscMessage time] else []
    returnA -< messages

-- Function to generate an OSC message
oscMessage :: Time -> Message
oscMessage time = message "/playOnset" [float time]

-- Reactimate setup

-- Initialization function: just returns initial time
initSense :: IO Time
initSense = return 0

-- Sense function: calculates time delta between updates
sense :: Bool -> IO (DTime, Maybe Time)
sense _ = do
    threadDelay (1000000 `div` 20)  -- Update about 20 times per second
    deltaTime <- return (1 / 20)
    return (deltaTime, Just deltaTime)

-- Actuate function: here we output the OSC messages to the console
actuate :: Bool -> [Message] -> IO Bool
actuate _ messages = do
    mapM_ print messages  -- Print each message for now
    return False

-- Run a pattern with a given BPM
runPattern :: Pattern -> Double -> IO ()
runPattern pat bpm = reactimate initSense sense actuate (patternToSF pat bpm)
