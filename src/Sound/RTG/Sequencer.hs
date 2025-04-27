-- |
-- Module      : Sequencer
-- Description : A sequencer using the Timed IO Monad.
-- Copyright   : (c) Xavier Góngora, 2025
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.Sequencer where

-- ( globalPattern,
--   updatePatternPool,
--   stopPattern,
--   playSequencerPattern,
--   refreshSequencer,
--   playPattern,
--   d1,
--   d2,
--   d3,
--   d4,
--   d5,
--   d6,
--   d7,
--   d8,
-- )

import Control.Monad (forever, mapM_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Euterpea (Pitch, PitchClass (..), note, play, playS)
import GHC.IO (unsafePerformIO)
import qualified Sound.Osc.Fd as Osc
import Sound.RTG.Async (cancel)
import Sound.RTG.Event (Event (..), isOnset, zipValuesWithOnsets)
import Sound.RTG.OscMessages (superDirtMessage, superDirtPort)
import Sound.RTG.RhythmicPattern (Rhythmic, rhythm)
import Sound.RTG.TimedMonad
  ( Micro (..),
    MonadRef (..),
    TIO,
    TimedMonad (..),
    timedLift,
  )
import Sound.RTG.UnSafe (readcps)
import Sound.RTG.Utils (patternEventDurationSec)

-- TODO: This module still depends on Event constructors.
-- Can it be decoupled?

-- | A map containing all currently running patterns.
type PatternPool = HashMap PatternId SequencerPattern

-- | Pattern keys in a 'PatternPool' are just Strings.
type PatternId = String

-- | Events with output data.
type SequencerPattern = [(Event, Maybe [Output])]

-- | Samples names from an existing sound library (in this case, DirtSamples).
type SampleName = String

-- | Output value
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

-- PatternPool and Global Pattern

-- | Initialize an empty pattern pool in a mutable variable.
-- It needs to be exposed outside of an action to be able to
-- update it within the ghci runtime.
patternPoolRef :: IORef PatternPool
patternPoolRef = unsafePerformIO $ newIORef (HM.empty)

-- | Add or update a pattern in the pattern pool.
updatePatternPool :: PatternId -> SequencerPattern -> IO ()
updatePatternPool id pattern = modifyIORef' patternPoolRef $ HM.insert id pattern

-- Test Examples:
-- updatePatternPool "1" [(Onset, Just [Osc "cp"])]
-- updatePatternPool "2" [(Onset, Just [Osc "bd", Osc "sn"]), (Onset, Just [Osc "blip"])]
-- updatePatternPool "3" [(Rest, Nothing), (Onset, Just [Osc "can"], (Rest, Nothing))]

-- | Remove a pattern from the pool.
removePattern :: PatternId -> IO ()
removePattern id = modifyIORef' patternPoolRef $ HM.delete id

-- | The global pattern obtained from merging all patterns in the pool.
globalPattern :: IO (SequencerPattern)
globalPattern = do
  patternPool <- readIORef patternPoolRef
  let patternLengths = HM.map (length) patternPool
      leastCommonMultiple = HM.foldl' lcm 1 patternLengths
      adjustedPatterns = HM.map (refinePattern leastCommonMultiple) patternPool
      mergePattern = HM.foldl' (matchOutputEvents) [] adjustedPatterns
  pure mergePattern

{-@ refinePattern :: Integral a => n:a
                 -> {pttrn : SequencerPattern | n `mod` (length pttrn) == 0 }
                 -> {pttrn' : SequencerPattern | length pttrn' == n}@-}

-- | Adjust a pattern to a finer grain (/i.e./ to a bigger discrete chromatic universe).
refinePattern :: (Integral a) => a -> SequencerPattern -> SequencerPattern
refinePattern grain pttrn =
  let factor = (fromIntegral grain) `div` (length pttrn)
   in concat $ map (\x -> x : replicate (factor - 1) (Rest, Nothing)) pttrn

{-@ matchOutputEvents :: pttrn : [(Event, Maybe [a])]
                         -> {pttrn' : [(Event, Maybe [a])] | length pttrn == length pttrn'}
                         -> {pttrn'' : [(Event, Maybe [a])] | length pttrn == length pttrn''} @-}

-- | Join the outputs of matching events. Expects patterns of the same length.
matchOutputEvents :: SequencerPattern -> SequencerPattern -> SequencerPattern
matchOutputEvents [] pttrn' = pttrn' -- Should empty cases be handled here?
matchOutputEvents pttrn [] = pttrn
matchOutputEvents pttrn pttrn' = zipWith f pttrn pttrn'
  where
    f (event, outputs) (event', outputs') =
      if isOnset event || isOnset event'
        then (Onset, Just $ (fromMaybe [] outputs) ++ (fromMaybe [] outputs'))
        else (Rest, Nothing)

-- Play functionality

-- By lifting actions into the Timed IO Monad (TIO) we can forget about
-- managing computaion time dritfs explicitly.

-- | Play through the Timed IO Monad 'TIO' in a loop.
-- The 'Micro' parameter should be generated from the global cps value;
-- it defines the duration of each event.
playSequencerPattern :: Micro -> SequencerPattern -> TIO ()
playSequencerPattern dur sp = do
  ref <- fork $ mapM_ (eventOutput dur . snd) $ cycle sp
  timedLift $ writeIORef playRef (Just ref)

-- | Current play status. 'Nothing' stands for no playback.
-- A value gives the current playback process reference.
playRef :: IORef (Maybe (Ref TIO ()))
playRef = unsafePerformIO $ newIORef Nothing

-- | Generates an event playback by the given duration inside the timed IO monad.
eventOutput :: Micro -> Maybe [Output] -> TIO ()
eventOutput dur Nothing = delay dur
-- NOTE: Added now >>= lift . print to show timestamps
-- but they are shown elsewhere (probably in the actual thread).
eventOutput dur (Just outputs) = mapM_ (\x -> instantOutput dur x >> delay dur >> now >>= lift . print) outputs

-- | TODO: output for Note is messing up time.
-- NOTE: Previously called timedOutput, now I changed 'timedLift' to 'lift'
-- as this events are supposed to be instantaneous.
instantOutput :: Micro -> Output -> TIO ()
instantOutput dur (Osc sample) = lift $ do
  port <- superDirtPort
  Osc.sendMessage port (superDirtMessage sample)
instantOutput dur (Note pitch) =
  let Micro m = dur
      microsecs = fromIntegral m
      secs = microsecs / 10 ^ 6
   in lift $ do
        play $ note secs pitch -- too slow to catch up
        -- fork $ play $ note secs pitch -- too slow to catch up
        -- return ()

-- Execution interface

-- | Play a pattern and update the pattern pool.
playPattern :: (Rhythmic a) => PatternId -> a -> IO ()
playPattern id pattern = do
  outputs <- readIORef defaultOutput -- just for testing?
  let newSequencerPattern = toSequencerPattern pattern outputs
  updatePatternPool id newSequencerPattern
  refreshSequencer

resume = refreshSequencer

stopPattern id = removePattern id >> refreshSequencer

clear :: IO ()
clear = run go
  where
    go :: TIO ()
    go = lift $ do
      modifyIORef' playRef $ const Nothing
      modifyIORef' patternPoolRef $ const HM.empty
      refreshSequencer

stopAll :: IO ()
stopAll = run go
  where
    go :: TIO ()
    go = do
      currentlyPlaying <- (timedLift $ readIORef playRef)
      freeze $ fromJust currentlyPlaying

refreshSequencer :: IO ()
refreshSequencer = do
  gp <- globalPattern
  cps <- readcps
  currentlyPlaying <- readIORef playRef
  let grain = length gp
      dur =
        if grain /= 0 -- We get this at the start of after clearing the pool
          then cpsToTimeStamp cps grain
          else 0
  if isNothing currentlyPlaying
    then run $ playSequencerPattern dur gp
    else run $ do
      freeze (fromJust currentlyPlaying)
      delay dur
      playSequencerPattern dur gp

toSequencerPattern :: (Rhythmic a) => a -> [[Output]] -> SequencerPattern
toSequencerPattern pattern outputs =
  let eventPattern = rhythm pattern
   in zipValuesWithOnsets eventPattern outputs

defaultOutput :: IORef [[Output]]
defaultOutput = unsafePerformIO $ newIORef [[Osc "blip"]]
-- defaultOutput = unsafePerformIO $ newIORef [[Note (C, 5)]]

changeDefOutput = do
  modifyIORef' defaultOutput (const [[Osc "bd"]])

cpsToTimeStamp :: (RealFrac a, Integral b) => a -> b -> Micro
cpsToTimeStamp cps pttrnLen = Micro . round $ 1 / (toRational cps * fromIntegral pttrnLen) * 10 ^ 6

-- | Default pattern ID following Tidal Cycles convention.
d1, d2, d3, d4, d5, d6, d7, d8 :: (Rhythmic a) => a -> IO ()
d1 = playPattern "1"
d2 = playPattern "2"
d3 = playPattern "3"
d4 = playPattern "4"
d5 = playPattern "5"
d6 = playPattern "6"
d7 = playPattern "7"
d8 = playPattern "8"

{-
playPattern :: Playback -> [Event] -> [Output] -> IO ()
playPattern playback events outputs = do
  cps <- readcps
  let outputPattern = matchValuesWithOnsets eventPattern outputs
      eventPattern = case playback of
        Once -> events
        Loop -> cycle events
      dur =
        let len = length events
            microSecs = (patternEventDurationSec cps len) * 10 ^ 6
         in Micro $ round microSecs
  run $ mapM_ (playEvent dur) outputPattern
-}
