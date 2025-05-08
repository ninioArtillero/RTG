-- | Main API
module Sound.RTG
  ( module Euterpea.Music, -- TODO: make this exports explicit
    module Sound.RTG.Euclidean,
    module Sound.RTG.RhythmicPattern,
    module Sound.RTG.Sequencer,
    module Sound.RTG.Structure,
    module Sound.RTG.TimePatterns,
    help,
  )
where

import Euterpea.Music hiding (Primitive (..))
import Sound.RTG.BundleTransformations
import Sound.RTG.Euclidean (e')
import Sound.RTG.RhythmicPattern (Rhythmic (..), rhythm)
import Sound.RTG.Sequencer
  ( active,
    action,
    clear,
    fanBundle,
    fanOutput,
    hush,
    idle,
    kill,
    p,
    querie,
    reset,
    resume,
    setbpm,
    setcps,
    solo,
    start,
    startAll,
    status,
    stop,
    stopAll,
    unsolo,
  )
import Sound.RTG.Structure
import Sound.RTG.TimePatterns

-- TODO: Update instructions to new API.
help :: IO ()
help =
  putStrLn $
    unlines
      [ "\nRTG has several funcions to generate rhythmic patterns",
        "that are interpreted as MIDI and OSC message streams.",
        "In the first case the default MIDI output device is used.",
        "The list of active MIDI devices can be displayed with:",
        "\nλ> devices\n",
        "For OSC message playback, an active SuperDirt instance,",
        "with its standard sample library loaded, is needed.",
        "To create a pattern use any of the following:\n",
        "λ> o <sample> <rhythm>          -- OSC  | play rhythm once",
        "λ> l <sample> <rhythm>          -- OSC  | play rhythm in loop",
        "λ> p <root> <rhythmA> <rhythmB> -- MIDI | play rhythmA as an scale over rhythmB (loop)",
        "λ> s <root> <rhythmA>           -- MIDI | play rhythmA as a scale (loop)",
        "\nThe playback speed of the patterns can be changed on the fly.\n",
        "λ> setcps <new-cps>             -- Change CPS value",
        "λ> readcps                      -- View current CPS value",
        "\nTo stop playback make sure to bind all patterns to a name",
        "with the following syntax:\n",
        "λ> name <- ...",
        "λ> stop name",
        "\nIf a name is given again to a new pattern, the former becomes headless",
        "and the ghci session needs to be ended to kill all active pattern. \n",
        "λ> :quit"
      ]
