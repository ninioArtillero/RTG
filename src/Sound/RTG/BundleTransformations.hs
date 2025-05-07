module Sound.RTG.BundleTransformations where

import Sound.RTG.PatternBundle

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
  concat
    . map
      ( \(event, maybeOutputs) ->
          case maybeOutputs of
            Nothing -> [(event, maybeOutputs)]
            Just outputList -> [(event, Just [output]) | output <- outputList]
      )
