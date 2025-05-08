module Sound.RTG.BundleTransformations where

import Sound.RTG.PatternBundle
import Sound.RTG.RhythmicPattern

liftB :: (OutputPattern -> OutputPattern) -> PatternBundle -> PatternBundle
liftB t =
  fmap
    ( \sequencerPattern ->
        sequencerPattern
          { getOutputPattern = t $ getOutputPattern sequencerPattern
          }
    )

handFan :: OutputPattern -> OutputPattern
handFan =
  liftR $
    concat
      . map
        ( \(event, maybeOutputs) ->
            case maybeOutputs of
              Nothing -> [(event, maybeOutputs)]
              Just outputList -> [(event, Just [output]) | output <- outputList]
        )

-- | Apply a rhythmic value to every pattern in the bundle using the 'Semigroup' operation.
-- Like a group action following the analogy with a /Principal Bundle/.
fibreProduct :: (Rhythmic a) => a -> PatternBundle -> PatternBundle
fibreProduct = liftB . (<>) . rhythmToOutputPattern

-- | Lifts a rhythmic type into a sequencer pattern.
rhythmToSequencerPattern :: (Rhythmic a) => a -> SequencerPattern
rhythmToSequencerPattern r = SequencerPattern (rhythmToOutputPattern r) Idle

-- | Pairs each rhythmic pattern event to a null output.
rhythmToOutputPattern :: (Rhythmic a) => a -> OutputPattern
rhythmToOutputPattern r = Rhythm [(e, Nothing) | e <- rhythm r]
