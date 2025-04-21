-- |
-- Module      : Sequencer
-- Description : A sequencer using the Timed IO Monad.
-- Copyright   : (c) Xavier Góngora, 2025
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.Sequencer (globalPattern, updatePatternPool, removePattern, playSequencerPattern) where

import Control.Monad ( mapM_ )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef ( IORef, newIORef, modifyIORef', readIORef )
import Euterpea (Pitch, note, playS)
import qualified Sound.Osc.Fd as Osc
import Sound.RTG.OscMessages (superDirtMessage, superDirtPort)
import Sound.RTG.RhythmicPattern (Event (..))
import Sound.RTG.TimedMonad
  ( Micro (..),
    TIO,
    TimedMonad (delay, run),
    timedLift,
  )
import Sound.RTG.Utils (patternEventDurationSec)


-- | A map containing all currently running patterns.
type PatternPool = HashMap PatternId (SequencerPattern)

-- | Pattern keys in a 'PatternPool' are just Strings.
type PatternId = String

-- | Events with output data.
type SequencerPattern = [(Event, Maybe [Output])]

-- | Samples names from an existing sound library (in this case, DirtSamples).
type SampleName = String

-- | Output value types.
data Output
  = -- | Trigger samples in SuperDirt.
    Osc !SampleName
  | -- | Play a MIDI note using the Euterpea backend.
    Note !Pitch
  deriving (Show, Eq)

isOsc :: Output -> Bool
isOsc (Osc _) = True
isOsc _ = False

isNote :: Output -> Bool
isNote (Note _) = True
isNote _ = False

-- | Initialize an empty pattern pool.
patternPoolRef :: IO (IORef PatternPool)
patternPoolRef = newIORef (HM.empty)

-- | Add or update a pattern.
updatePatternPool :: PatternId -> SequencerPattern -> IO ()
updatePatternPool id pattern = do
  ref <- patternPoolRef
  modifyIORef' ref $ HM.insert id pattern

-- | Remove a pattern from the pool.
removePattern :: PatternId -> IO ()
removePattern id = do
  ref <- patternPoolRef
  modifyIORef' ref $ HM.delete id

-- | The global pattern obtained from merging all running patterns.
globalPattern :: IO (SequencerPattern)
globalPattern = do
  ref <- patternPoolRef
  patternPool <- readIORef ref
  let patternLengths = HM.map length patternPool
      leastCommonMultiple = HM.foldl' lcm 1 patternLengths
      adjustedPatterns = HM.map (refinePattern leastCommonMultiple) patternPool
      mergePattern = HM.foldl' (matchOutputEvents) [] adjustedPatterns
  pure mergePattern

{-@ refinePattern :: Integral a => n:a
                 -> {pttrn : SequencerPattern | n `mod` (length pttrn) == 0 }
                 -> {pttrn' : SequencerPattern | length pttrn' == n}@-}

-- | Adjust a pattern to a finer grain (/i.e./ to a bigger discrete chromatic universe).
refinePattern :: (Integral a) => a -> SequencerPattern -> SequencerPattern
refinePattern n pttrn =
  let factor = div (fromIntegral n) (length pttrn)
   in concat $ map (\x -> x : replicate (factor - 1) (Rest, Nothing)) pttrn

{-@ matchOutputEvents :: pttrn : [(Event, Maybe [a])]
                         -> {pttrn' : [(Event, Maybe [a])] | length pttrn == length pttrn'}
                         -> {pttrn'' : [(Event, Maybe [a])] | length pttrn == length pttrn''} @-}

-- | Join the outputs of matching events. Expects patterns of the same length.
matchOutputEvents :: SequencerPattern -> SequencerPattern -> SequencerPattern
matchOutputEvents pttrn pttrn' = zipWith f pttrn pttrn'
  where
    f (event, outputs) (event', outputs') = case (event, event') of
      (Onset, _) -> (Onset, (++) <$> outputs <*> outputs')
      (_, Onset) -> (Onset, (++) <$> outputs <*> outputs')
      (_, _) -> (Rest, Nothing)


-- Play functionality

-- Using the timedLift function we can to forget about
-- implementation details of the Timed IO Monad (TIO), which are quite messy.
-- Timing information is handled beneath the hood, except when we call delay.

-- | Play through the Timed IO Monad 'TIO'.
-- The 'Micro' parameter should be generated from the global cps value.
playSequencerPattern :: Micro -> SequencerPattern -> IO ()
playSequencerPattern dur sp = run $ mapM_ (eventOutput dur . snd) sp

eventOutput :: Micro -> Maybe [Output] -> TIO ()
eventOutput dur Nothing = delay dur
eventOutput dur (Just outputs) = mapM_ (timedOutput dur) outputs

timedOutput :: Micro -> Output -> TIO ()
timedOutput dur (Osc sample) = timedLift $ do
  port <- superDirtPort
  Osc.sendMessage port (superDirtMessage sample)
timedOutput dur (Note pitch) =
  let Micro m = dur
      microsecs = fromIntegral m
      secs = microsecs / 10 ^ 6
   in timedLift (playS $ note secs pitch)
