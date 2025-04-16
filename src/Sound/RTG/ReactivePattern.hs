{-# LANGUAGE Arrows #-}
-- | FRP pattern implementation for on-the-fly pattern transformation
module Sound.RTG.ReactivePattern where

import           Control.Concurrent               (forkIO, threadDelay)
import           Euterpea.IO.MIDI                 (play)
import           Euterpea.Music
import           FRP.Yampa
import qualified Sound.Osc                        as Osc hiding (Time)
import           Sound.Osc.Fd                     (sendMessage)
import           Sound.RTG.RhythmicPattern
import           Sound.RTG.OscMessages

-- Signal Function

type PatternSF = SF Time [Osc.Message]
type Beat = Double
type BPM = Double


-- Function to convert a binary pattern into a repeating stream of OSC messages
patternToSF :: Rhythmic a => SampleName -> Beat -> BPM -> a -> SF Time [Osc.Message]
patternToSF sample beat bpm pttrn = proc time -> do
  let onsetPattern = getRhythm . toRhythm $ pttrn
      patternLength = length onsetPattern
      bps = bpm / 60 -- beats per second
  index <- arr (\(t, i, l, b) -> floor (t * i / b) `mod` l) -< (time, bps, patternLength, beat)
  -- Determine if the current index corresponds to an onset
  isOnset <- arr (\(i, onsets) -> onsets !! i == Onset) -< (index, onsetPattern)
  -- Event stream
  event <- edge -< isOnset
  -- Generate an OSC message if there's an onset
  messages <- arr (\e -> [superDirtMessage sample | isEvent e]) -< event
  --messages <- arr (\(t, e) -> case e of NoEvent -> []; Event _ -> [oscMessage t]) -< (time,event)
  returnA -< messages

playNote :: Music Pitch -> SF [Osc.Message] (IO ())
playNote note = proc oscMessage -> do
  n <- arr (\m -> if null m then play (rest 0.01 :: Music Pitch) else play note) -< oscMessage
  returnA -< n


{-
silence :: PatternSF
silence = constant []

start = rSwitch silence

play :: PatternSF
play = (pttrnSignal &&& changePattern) `switch` (\p -> patternToSF p)


changePattern :: Rhythmic b => SF a (Event b)

-- TODO: Find a logic to generate event streams to trigger
-- the transformation of patterns and cps
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

-- Initialization value. Initial time in this case
initSense :: IO Time
initSense = return 0

-- Sense function: calculates time delta between updates
sense :: Bool -> IO (DTime, Maybe Time)
sense _ = do
    threadDelay (1000000 `div` 80)  -- Update about 80 times per second
    let deltaTime = 1 / 80
    return (deltaTime, Just deltaTime)

-- Actuate function: here we output the OSC messages to the console
actuate :: Bool -> [Osc.Message] -> IO Bool
actuate _ messages = do
    port <- superDirtPort
    mapM_ (sendMessage port) messages
    return False

-- Run a pattern with a given beat value and BPM (OSC to SuperDirt)
runPattern :: Rhythmic a => SampleName -> Beat -> BPM -> a -> IO ()
runPattern sample beat bpm pat = do
  forkIO $ reactimate initSense sense actuate (time >>> patternToSF sample beat bpm pat)
  return ()

actuate' :: Bool -> IO () -> IO Bool
actuate' _ _ = return False

runPattern' :: Rhythmic a => SampleName -> Beat -> BPM -> a -> IO ()
runPattern' sample beat bpm pat = do
  forkIO $ reactimate initSense sense actuate' (time >>> patternToSF sample beat bpm pat >>> playNote (c 4 qn))
  return ()
