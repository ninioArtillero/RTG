-- |
-- Module      : Sequencer
-- Description : A sequencer using the Timed IO Monad.
-- Copyright   : (c) Xavier Góngora, 2025
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.Sequencer
  ( globalPattern,
    updatePatternPool,
    removePattern,
    playSequencerPattern,
    pp,
    d1,
    d2,
    d3,
    d4,
    d5,
    d6,
    d7,
    d8,
  )
where

import Control.Monad (forever, mapM_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Euterpea (Pitch, PitchClass (..), note, play, playS)
import GHC.IO (unsafePerformIO)
import qualified Sound.Osc.Fd as Osc
import Sound.RTG.Conversion (zipValuesWithOnsets)
import Sound.RTG.OscMessages (superDirtMessage, superDirtPort)
import Sound.RTG.RhythmicPattern (Event (..), Rhythmic, rhythm)
import Sound.RTG.TimedMonad
  ( Micro (..),
    MonadRef (..),
    TIO,
    TimedMonad (..),
    timedLift,
  )
import Sound.RTG.Async (cancel)
import Sound.RTG.UnSafe (readcps)
import Sound.RTG.Utils (patternEventDurationSec)

-- | A map containing all currently running patterns.
type PatternPool = HashMap PatternId SequencerPattern

-- | Pattern keys in a 'PatternPool' are just Strings.
type PatternId = String

-- | Events with output data.
type SequencerPattern = [(Event, Maybe [Output])]

-- | Samples names from an existing sound library (in this case, DirtSamples).
type SampleName = String

-- | Output value types.
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

-- | Initialize an empty pattern pool.
patternPoolRef :: IORef PatternPool
patternPoolRef = unsafePerformIO $ newIORef (HM.empty)

-- | Add or update a pattern.
updatePatternPool :: PatternId -> SequencerPattern -> IO ()
updatePatternPool id pattern = modifyIORef' patternPoolRef $ HM.insert id pattern

-- | Remove a pattern from the pool.
removePattern :: PatternId -> IO ()
removePattern id = modifyIORef' patternPoolRef $ HM.delete id

-- | The global pattern obtained from merging all running patterns.
globalPattern :: IO (SequencerPattern)
globalPattern = do
  patternPool <- readIORef patternPoolRef
  let patternLengths = HM.map (length) patternPool
      leastCommonMultiple = HM.foldl' lcm 1 patternLengths
      adjustedPatterns = HM.map (refinePattern leastCommonMultiple) patternPool
      mergePattern = HM.foldl' (matchOutputEvents) [] adjustedPatterns
  pure mergePattern

-- updatePatternPool "1" [(Onset, Just [Osc "cp"])]
-- updatePatternPool "2" [(Onset, Just [Osc "bd", Osc "sn"])]

{-@ refinePattern :: Integral a => n:a
                 -> {pttrn : SequencerPattern | n `mod` (length pttrn) == 0 }
                 -> {pttrn' : SequencerPattern | length pttrn' == n}@-}

-- | Adjust a pattern to a finer grain (/i.e./ to a bigger discrete chromatic universe).
refinePattern :: (Integral a) => a -> SequencerPattern -> SequencerPattern
refinePattern n pttrn =
  let factor = div (fromIntegral n) (length pttrn)
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
      if event == Onset || event' == Onset
        then (Onset, Just $ (fromMaybe [] outputs) ++ (fromMaybe [] outputs'))
        else (Rest, Nothing)

-- Play functionality

-- Using the timedLift function we can to forget about
-- implementation details of the Timed IO Monad (TIO), which are quite messy.
-- Timing information is handled beneath the hood, except when we call delay.

-- | Play through the Timed IO Monad 'TIO'.
-- The 'Micro' parameter should be generated from the global cps value;
-- it defines the duration of each event.
playSequencerPattern :: Micro -> SequencerPattern -> TIO ()
playSequencerPattern dur sp = do
  ref <- fork $ mapM_ (eventOutput dur . snd) $ cycle sp
  timedLift $ writeIORef playRef (Just ref)

eventOutput :: Micro -> Maybe [Output] -> TIO ()
eventOutput dur Nothing = delay dur
eventOutput dur (Just outputs) = mapM_ (\x -> timedOutput dur x >> delay dur) outputs

-- TODO: output for Note is messing up time.
timedOutput :: Micro -> Output -> TIO ()
timedOutput dur (Osc sample) = timedLift $ do
  port <- superDirtPort
  Osc.sendMessage port (superDirtMessage sample)
timedOutput dur (Note pitch) =
  let Micro m = dur
      microsecs = fromIntegral m
      secs = microsecs / 10 ^ 6
   in timedLift $ do
        fork $ playS $ note secs pitch -- Esta función es tan lenta que
        return ()

-- Execution interface

-- | Play a pattern and update the pattern pool.
pp :: (Rhythmic a) => PatternId -> a -> IO ()
pp id pattern = do
  outputs <- readIORef defaultOutput
  cps <- readcps
  let newSequencerPattern = toSequencerPattern pattern outputs
  updatePatternPool id newSequencerPattern
  gp <- globalPattern
  let grain = length gp
      dur = cpsToTimeStamp cps grain
  run $ do
    playing <- timedLift $ readIORef playRef
    if isNothing playing
        then playSequencerPattern dur gp
        else freeze (fromJust playing) >> delay dur >> playSequencerPattern dur gp

playRef :: IORef (Maybe (Ref TIO ()))
playRef = unsafePerformIO $ newIORef Nothing

toSequencerPattern :: (Rhythmic a) => a -> [[Output]] -> SequencerPattern
toSequencerPattern pattern outputs =
  let eventPattern = rhythm pattern
   in zipValuesWithOnsets eventPattern outputs

defaultOutput :: IORef [[Output]]
-- defaultOutput = unsafePerformIO $ newIORef [[Note (C,5)]]
defaultOutput = unsafePerformIO $ newIORef [[Osc "cp"]]

cpsToTimeStamp :: (RealFrac a, Integral b) => a -> b -> Micro
cpsToTimeStamp cps pttrnLen = Micro . round $ 1 / (toRational cps * fromIntegral pttrnLen) * 10 ^ 6

-- Default pattern IDs following Tidal Cycles convention.

d1, d2, d3, d4, d5, d6, d7, d8 :: (Rhythmic a) => a -> IO ()
d1 = pp "1"
d2 = pp "2"
d3 = pp "3"
d4 = pp "4"
d5 = pp "5"
d6 = pp "6"
d7 = pp "7"
d8 = pp "8"

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
