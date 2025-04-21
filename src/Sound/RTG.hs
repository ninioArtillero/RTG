-- | Main API
module Sound.RTG
  ( module Euterpea.Music,
    module Sound.RTG.Euclidean,
    module Sound.RTG.Polygon,
    module Sound.RTG.ReactivePattern,
    module Sound.RTG.Bjorklund,
    module Sound.RTG.PerfectBalance,
    module Sound.RTG.Play,
    module Sound.RTG.RhythmicPattern,
    module Sound.RTG.TimePatterns,
    module Sound.RTG.Zip,
    module Sound.RTG.TemporalMonad,
    module Sound.RTG.UnSafe,
  )
where

import Euterpea.Music hiding (Primitive (..))
import Sound.RTG.Bjorklund
import Sound.RTG.Conversion
import Sound.RTG.Euclidean
import Sound.RTG.PerfectBalance
import Sound.RTG.Play
import Sound.RTG.PlayScale
import Sound.RTG.Polygon
import Sound.RTG.ReactivePattern
import Sound.RTG.RhythmicPattern
import Sound.RTG.Sequencer
import Sound.RTG.Structure
import Sound.RTG.TemporalMonad
import Sound.RTG.TimePatterns
import Sound.RTG.TimedMonad
import Sound.RTG.UnSafe
import Sound.RTG.Zip

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
