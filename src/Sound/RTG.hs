-- | Main API
module Sound.RTG
  (
    p
  , s
  , readcps
    -- * Re-exported modules
  , module Euterpea.IO.MIDI
  , module Euterpea.Music
  , module Sound.RTG.Geometry
  , module Sound.RTG.ReactivePattern
  , module Sound.RTG.Rhythm
  , module Sound.RTG.TiledMusic
  , module Sound.RTG.Time
  )
where


import           Control.Concurrent        (ThreadId, forkIO, killThread,
                                            myThreadId, readMVar)
import           Control.Monad             (forever)
import           Euterpea.IO.MIDI
import           Euterpea.Music            hiding (forever, invert)
-- import           GHC.Conc                  (listThreads, threadStatus) -- since base-4.18.0.0
import           Sound.RTG.Geometry
import           Sound.RTG.ReactivePattern
import           Sound.RTG.Rhythm
import           Sound.RTG.TiledMusic
import           Sound.RTG.TiledStream
import           Sound.RTG.Time

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
