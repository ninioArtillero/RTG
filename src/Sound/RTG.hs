-- | Main API
module Sound.RTG
  (
    p
  , s
  , readcps
    -- * Re-exported modules
  , module Euterpea.IO.MIDI
  , module Euterpea.Music
  , module Sound.RTG.Euclidean
  , module Sound.RTG.Polygon
  , module Sound.RTG.ReactivePattern
  , module Sound.RTG.Bjorklund
  , module Sound.RTG.PerfectBalance
  , module Sound.RTG.Play
  , module Sound.RTG.RhythmicPattern
  , module Sound.RTG.TimePatterns
  , module Sound.RTG.Zip
  , module Sound.RTG.TiledMusic
  , module Sound.RTG.OscMessages
  , module Sound.RTG.TemporalMonad
  , module Sound.RTG.UnSafe
  )
where


import           Control.Concurrent        (ThreadId, forkIO, killThread,
                                            myThreadId, readMVar)
import           Control.Monad             (forever)
import           Euterpea.IO.MIDI
import           Euterpea.Music            hiding (Rest, forever, invert)
-- import           GHC.Conc                  (listThreads, threadStatus) -- since base-4.18.0.0
import           Sound.RTG.Euclidean
import           Sound.RTG.Polygon
import           Sound.RTG.ReactivePattern
import           Sound.RTG.Bjorklund
import           Sound.RTG.PerfectBalance
import Sound.RTG.Play
import           Sound.RTG.RhythmicPattern hiding (co, inv)
import           Sound.RTG.TimePatterns
import           Sound.RTG.Zip
import           Sound.RTG.TiledMusic
import           Sound.RTG.TiledStream
import           Sound.RTG.OscMessages   hiding (CPS, Dur)
import           Sound.RTG.TemporalMonad
import           Sound.RTG.UnSafe
import           Sound.RTG.Structure
import           Sound.RTG.Conversion
import           Sound.RTG.PlayScale

type CPS = Rational

-- | Play r1 as scale over r2
p :: (Rhythmic a, Rhythmic b) => Root -> a -> b -> IO ThreadId
p root r1 r2 =
  forkIO . forever $ do
    cps <- readMVar globalCPS
    play $ patternToMusic cps root r1 r2

-- | Play as scale
s :: Rhythmic a => Root -> a -> IO ThreadId
s root rhythm =
  forkIO . forever $ do
    cps <- readMVar globalCPS
    play $ scale cps root rhythm

readcps :: IO CPS
readcps = readMVar globalCPS

help :: IO ()
help = putStrLn $ unlines [
                          "\nRTG has several funcions to generate rhythmic patterns",
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

-- Requires GHC.Conc.listThreads available since base-4.18.0.0
-- Still not working as expected and imposes a conservative upperbound base-4.21.0.0)
-- due the instability of the module (as mentioned in the documentation).
-- NOTE: Not working for unknown reason. Modify base dependency bounds as mentioned before
-- for testing.
-- stopAll :: IO ()
-- stopAll = listThreads >>= mapM_ killThread
