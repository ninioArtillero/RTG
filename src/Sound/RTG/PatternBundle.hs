{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : PatternBundle
-- Description : A pattern map and its elementary operations.
-- Copyright   : (c) Xavier Góngora, 2025
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- The 'PatternBundle' follows an analogy with a /fiber bundle/,
-- where the 'PatternBundle' is the /bundle/, the 'SequencerPattern'
-- is the /fiber/, and an 'OutputPattern' is the /base space/.
module Sound.RTG.PatternBundle where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.Maybe (fromMaybe)
import Euterpea (Pitch)
import Sound.RTG.Event (Event (..), isOnset, pairValuesWithOnsets)
import Sound.RTG.List (compact)
import Sound.RTG.RhythmicPattern

-- TODO: This module still depends on Event constructors.
-- Can it be decoupled?

-- | A map containing all currently running patterns.
-- Analogous to the /bundle/ of a /fiber bundle/.
-- TODO: Change to newtype to define custom show instance.
type PatternBundle = IntMap SequencerPattern

-- | Pattern keys in a 'PatternBundle' are 'Int'.
type PatternId = Int

-- | Events with output data.
-- Analogous to the /fiber/ of a /fiber bundle/.
data SequencerPattern = SequencerPattern
  { getOutputPattern :: !OutputPattern,
    getPatternStatus :: !PatternStatus
  }

-- | Patterns excecuted 'Sound.RTG.Sequencer.inSequencer'.
type OutputPattern = Rhythm (Event, Maybe [Output])

-- | Recover the event list using the rhythmic interface.
instance Rhythmic OutputPattern where
  toRhythm = liftR (map fst)

data PatternStatus = Idle | Running deriving (Eq)

-- | Samples names from an existing sound library (in this case, DirtSamples).
type SampleName = String

-- | Output values. 'OSC' message or midi 'Note'.
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

-- * PatternBundle Projection

-- | A projection of the bundle into a single pattern
-- obtained from merging all 'Running' patterns in the bundle.
-- Analogous to the /base space/ of a /fiber bundle/. The 'globalPattern' is the means
-- to play the patterns contained in the 'PatternBundle'. The /merging/ proceduce it
-- implements is analogous to a /projection/.
projection :: PatternBundle -> OutputPattern
projection patternBundle =
  let patternLengthsMap = Map.map (length . getOutputPattern) patternBundle
      leastCommonMultiple = Map.foldl' lcm 1 patternLengthsMap
      runningPatterns = Map.filter ((== Running) . getPatternStatus) patternBundle
      alignedOutputPatterns =
        Map.map (alignPattern leastCommonMultiple . getOutputPattern) runningPatterns
      -- NOTE: For some reason it is necessary to accumulate on the bare output list.
      -- Otherwise, the globalPattern at the sequencer turns reduces to one of is fibers.
      -- The other functionality seems to be unaffected.
      -- See BUG comment bellow. Intended version (which type checks):
      -- gp = Map.foldl' matchOutputEvents (mempty :: OutputPattern) alignedOutputPatterns
      gp = Map.foldl' (\acc x -> joinEventOutputs acc (getRhythm x)) [] alignedOutputPatterns
   in -- gp
      -- Compact the resulting pattern to purge unnecessary rest events.
      Rhythm $ compact gp

fiber :: PatternId -> PatternBundle -> Maybe OutputPattern
fiber id patternBundle =
  fmap (liftR compact . getOutputPattern) $
    Map.lookup id $
      patternBundle

{-@ alignPattern :: Integral a => n:a
                 -> {pttrn : OutputPattern | n `mod` (length pttrn) == 0 }
                 -> {pttrn' : OutputPattern | length pttrn' == n}@-}

-- | Align a 'OutputPattern' to a finer grain (/i.e./ to a bigger discrete chromatic universe).
-- TODO: Refactor projection logic in here
alignPattern :: (Integral a) => a -> OutputPattern -> OutputPattern
alignPattern grain pttrn =
  let factor = (fromIntegral grain) `div` (length pttrn)
   in liftR (concat . map (: replicate (factor - 1) (Rest, Nothing))) pttrn

-- TODO: Correct this type when running LH if BUG can't be resolved.
{-@ matchOutputEvents :: pttrn : OutputPattern
                      -> {pttrn' : OutputPattern | length pttrn == length pttrn'}
                      -> {pttrn'' : OutputPattern | length pttrn == length pttrn''} @-}

-- | Join the outputs of matching events. Expects patterns of the same length.
joinEventOutputs ::
  [(Event, Maybe [Output])] ->
  [(Event, Maybe [Output])] ->
  [(Event, Maybe [Output])]
joinEventOutputs [] pttrn' = pttrn'
joinEventOutputs pttrn [] = pttrn
-- Uses the Semigroup instace of (Event, Maybe [a]).
-- Previous implementation threw away outputs if both events where Rest.
-- So far they are note supposed to be able to do anything, but might prove
-- usefull to implement cool functionality for "ghost" outputs.
-- NOTE: Depends on the || semigroup instance of events.
joinEventOutputs pttrn pttrn' = zipWith (<>) pttrn pttrn'

{-
-- BUG: This bug was introduced when OutputPattern was refactored to use the
-- 'Rhythm' wrapper to derive its default Semigroup instance.
-- The compiler (currently tested with ghc-9.10.1) seems to be confused about
-- this version of the code which lifts the local function.
-- I'ld prefer this version as its type signature better documents its behavior.
matchOutputEvents :: OutputPattern -> OutputPattern -> OutputPattern
matchOutputEvents mempty pttrn' = pttrn' -- NOTE: Should empty cases be handled here?
matchOutputEvents pttrn mempty = pttrn -- The compiler throws a warning that this is redundant.
matchOutputEvents pttrn pttrn' = (liftR2 . zipWith) f pttrn pttrn'
  where
    f :: (Event, Maybe [Output]) -> (Event, Maybe [Output]) -> (Event, Maybe [Output])
    f (event, outputs) (event', outputs') =
      if isOnset event || isOnset event'
        then (Onset, Just $ (fromMaybe [] outputs) ++ (fromMaybe [] outputs'))
        else (Rest, Nothing)
-}

-- * Elementary Bundle Operations

bundleKeys :: PatternBundle -> [PatternId]
bundleKeys = Map.keys

runningPatternKeys :: PatternBundle -> [PatternId]
runningPatternKeys = Map.keys . Map.filter ((== Running) . getPatternStatus)

idlePatternKeys :: PatternBundle -> [PatternId]
idlePatternKeys = Map.keys . Map.filter ((== Idle) . getPatternStatus)

emptyBundle :: PatternBundle
emptyBundle = Map.empty

insert :: PatternId -> SequencerPattern -> PatternBundle -> PatternBundle
insert = Map.insert

remove :: PatternId -> PatternBundle -> PatternBundle
remove = Map.delete

disable :: PatternId -> PatternBundle -> PatternBundle
disable =
  Map.adjust
    ( \sequencerPattern ->
        sequencerPattern {getPatternStatus = Idle}
    )

enable :: PatternId -> PatternBundle -> PatternBundle
enable =
  Map.adjust
    ( \sequencerPattern ->
        sequencerPattern {getPatternStatus = Running}
    )

enableAll :: PatternBundle -> PatternBundle
enableAll =
  Map.map
    ( \sequencerPattern ->
        sequencerPattern {getPatternStatus = Running}
    )

disableAll :: PatternBundle -> PatternBundle
disableAll =
  Map.map
    ( \sequencerPattern ->
        sequencerPattern {getPatternStatus = Idle}
    )

-- * Conversion

-- | Convert a rhythmic pattern into an 'OutputPattern'
-- TODO: Add a switch for changing the event matching strategy.
toOutputPattern :: (Rhythmic a) => a -> [[Output]] -> OutputPattern
toOutputPattern pattern outputs =
  let eventPattern = rhythm pattern
   in Rhythm $ pairValuesWithOnsets eventPattern outputs
