module Sound.RTG.BundleTransformations where

import Sound.RTG.PatternBundle
import Sound.RTG.Sequencer (updateSequencerCPS)

lift :: (OutputPattern -> OutputPattern) -> PatternBundle -> PatternBundle
lift t =
  fmap
    ( \sequencerPattern ->
        sequencerPattern
          { getOutputPattern = t $ getOutputPattern sequencerPattern
          }
    )

-- | Expand the sequencer time.
expandCycle :: Integral a => a -> OutputPattern -> IO ()
expandCycle eventDur outputPattern = undefined

handFan :: OutputPattern -> OutputPattern
handFan =
  concat
    . map
      ( \(event, maybeOutputs) ->
          case maybeOutputs of
            Nothing -> [(event, maybeOutputs)]
            Just outputList -> [(event, Just [output]) | output <- outputList]
      )
