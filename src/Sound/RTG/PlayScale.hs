-- |
-- Module      : PlayScale
-- Description : Use patterns as scales
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.PlayScale
  ( p,
    s,
    patternToMusic,
    scale,
    scalePitches,
    Root,
  )
where

import Control.Concurrent (ThreadId, forkIO, readMVar)
import Control.Monad (forever)
import Euterpea hiding (Rest, forever)
import Sound.RTG.Conversion (onsetCount)
import Sound.RTG.RhythmicPattern (Event (..), Rhythmic (..), rhythm)
import Sound.RTG.Structure (iois)
import Sound.RTG.UnSafe (readcps)

-- Use patterns simultaneaously as rhythms and scales
-- for Euterpea MIDI output

type CPS = Rational

type Root = Pitch

type Scale = [Pitch]

-- | Play r1 as scale over r2
p :: (Rhythmic a, Rhythmic b) => Root -> a -> b -> IO ThreadId
p root r1 r2 =
  forkIO . forever $ do
    cps <- readcps
    play $ patternToMusic cps root r1 r2

-- | Play as scale
s :: (Rhythmic a) => Root -> a -> IO ThreadId
s root rhythm =
  forkIO . forever $ do
    cps <- readcps
    play $ scale cps root rhythm

-- | Transforms the first rhythm into a scale to be played at the second.
-- Produces an infinite 'Music Pitch' value.
-- TODO: Take the CPS and root values into a State Monad to stop passing then arround
-- TODO: Look for time and timing issues (Euterpea management of duration)
patternToMusic :: (Rhythmic a, Rhythmic b) => CPS -> Root -> a -> b -> Music Pitch
patternToMusic cps root scalePttrn rhythmicPttrn =
  let binaryPttrn = rhythm $ rhythmicPttrn
      scale = scalePitches root scalePttrn
      -- TODO: aux function to count onsets (faster?)
      n = length scale
      m = onsetCount binaryPttrn
      l = length binaryPttrn
      sync = l * (lcm n m `div` m)
      eventDur = 1 / (fromIntegral l * cps)
   in line $ take sync $ matchEvents eventDur binaryPttrn scale

scale :: (Rhythmic a) => CPS -> Root -> a -> Music Pitch
scale cps root rhythmicPttrn = line . map (note dur) $ scalePttrn
  where
    scalePttrn = scalePitches root rhythmicPttrn
    dur = 1 / fromIntegral (length scalePttrn) * cps

matchEvents :: Dur -> [Event] -> Scale -> [Music Pitch]
matchEvents 0 _ _ = []
matchEvents _ [] _ = []
matchEvents _ _ [] = []
matchEvents duration pttrn scale =
  let (x : xs) = cycle pttrn
      (p : ps) = cycle scale
   in case x of
        Rest -> rest duration : matchEvents duration xs (p : ps)
        Onset -> note duration p : matchEvents duration xs ps

-- TODO: Allow microtonal scales

-- | Transforms a given rhythm into an scale begining at a root note
-- up to its octave.
scalePitches :: (Rhythmic a) => Root -> a -> Scale
scalePitches root = semitonesToScale root . timeToSemitoneIntervals

timeToSemitoneIntervals :: (Rhythmic a) => a -> [Int]
timeToSemitoneIntervals pttrn =
  let intervals = iois pttrn
   in reverse $ foldl (\acc x -> (head acc + x) : acc) [0] intervals

semitonesToScale :: Root -> [Int] -> Scale
semitonesToScale root = map (pitch . (+ absPitch root))
