module Sound.RTG.PatternPool where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.Maybe
import Euterpea hiding (Note, Rest)
import Sound.RTG.Event

-- TODO: This module still depends on Event constructors.
-- Can it be decoupled?

-- | A map containing all currently running patterns.
-- Analogous to the /bundle/ of a /fiber bundle/.
-- TODO: Change to newtype to define custom show instance.
type PatternPool = IntMap SequencerPattern

-- | Pattern keys in a 'PatternPool' are 'Int'.
type PatternId = Int

-- | Events with output data.
-- Analogous to the /fiber/ of a /fiber bundle/.
data SequencerPattern = SequencerPattern
  { getOutputPattern :: !OutputPattern,
    getPatternStatus :: !PatternStatus
  }

type OutputPattern = [(Event, Maybe [Output])]

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

-- * PatternPool Proyection

-- | The global pattern obtained from merging all running patterns in the pool.
-- Analogous to the /base space/ of a /fiber bundle/. The 'globalPattern' is the means
-- to play the patterns contained in the 'PatternPool'. The /merging/ proceduce it
-- implements is analogous to a /projection/.
proyection :: PatternPool -> OutputPattern
proyection patternPool =
  let patternLengthsMap = Map.map (length . getOutputPattern) patternPool
      leastCommonMultiple = Map.foldl' lcm 1 patternLengthsMap
      runningPatterns = Map.filter ((== Running) . getPatternStatus) patternPool
      alignedOutputPatterns =
        Map.map (alignPattern leastCommonMultiple . getOutputPattern) runningPatterns
      gp = Map.foldl' (matchOutputEvents) [] alignedOutputPatterns
   in gp

fiber :: PatternId -> PatternPool -> Maybe SequencerPattern
fiber id patternPool = Map.lookup id patternPool

{-@ alignPattern :: Integral a => n:a
                 -> {pttrn : OutputPattern | n `mod` (length pttrn) == 0 }
                 -> {pttrn' : OutputPattern | length pttrn' == n}@-}

-- | Align a 'OutputPattern' to a finer grain (/i.e./ to a bigger discrete chromatic universe).
alignPattern :: (Integral a) => a -> OutputPattern -> OutputPattern
alignPattern grain pttrn =
  let factor = (fromIntegral grain) `div` (length pttrn)
   in concat $ map (\x -> x : replicate (factor - 1) (Rest, Nothing)) pttrn

{-@ matchOutputEvents :: pttrn : OutputPattern
                      -> {pttrn' : OutputPattern | length pttrn == length pttrn'}
                      -> {pttrn'' : OutputPattern | length pttrn == length pttrn''} @-}

-- | Join the outputs of matching events. Expects patterns of the same length.
matchOutputEvents :: OutputPattern -> OutputPattern -> OutputPattern
matchOutputEvents [] pttrn' = pttrn' -- NOTE: Should empty cases be handled here?
matchOutputEvents pttrn [] = pttrn
matchOutputEvents pttrn pttrn' = zipWith f pttrn pttrn'
  where
    f (event, outputs) (event', outputs') =
      if isOnset event || isOnset event'
        then (Onset, Just $ (fromMaybe [] outputs) ++ (fromMaybe [] outputs'))
        else (Rest, Nothing)

-- * Modify the Pool

poolKeys :: PatternPool -> [PatternId]
poolKeys = Map.keys

runningPatternKeys :: PatternPool -> [PatternId]
runningPatternKeys = Map.keys . Map.filter ((== Running) . getPatternStatus)

idlePatternKeys :: PatternPool -> [PatternId]
idlePatternKeys = Map.keys . Map.filter ((== Idle) . getPatternStatus)

emptyPool :: PatternPool
emptyPool = Map.empty

insert :: PatternId -> SequencerPattern -> PatternPool -> PatternPool
insert = Map.insert

remove :: PatternId -> PatternPool -> PatternPool
remove = Map.delete

disable :: PatternId -> PatternPool -> PatternPool
disable =
  Map.adjust
    ( \sequencerPattern ->
        sequencerPattern {getPatternStatus = Idle}
    )

enable :: PatternId -> PatternPool -> PatternPool
enable =
  Map.adjust
    ( \sequencerPattern ->
        sequencerPattern {getPatternStatus = Running}
    )

enableAll :: PatternPool -> PatternPool
enableAll =
  Map.map
    ( \sequencerPattern ->
        sequencerPattern {getPatternStatus = Running}
    )

disableAll :: PatternPool -> PatternPool
disableAll =
  Map.map
    ( \sequencerPattern ->
        sequencerPattern {getPatternStatus = Idle}
    )
