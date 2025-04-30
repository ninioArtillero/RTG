-- |
-- Module      : Sequencer
-- Description : A sequencer using the Timed IO Monad.
-- Copyright   : (c) Xavier Góngora, 2025
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- The execution model of the sequencer follows the form of a /fiber bundle/,
-- where the 'PatternPool' is the /bundle/, the 'SequencerPattern'
-- is the /fiber/, and the 'globalPattern' the /base space/.
-- So that transformations on the bundle are projected into base, which
-- is the pattern to be executed.
--
-- Some sections of this module are (very) unsafe due to the use of 'Foreign.Store',
-- whose documentation is rather opaque about its implementation quirks.
-- Modify with care, as a 'Store' is not thread safe and its values not typechecked,
-- which can easily make the process blow up.
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
import qualified Data.HashMap.Strict as Map
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Euterpea (Pitch, PitchClass (..), note, play, playS)
import Foreign.Store
import qualified Sound.Osc.Fd as Osc
import Sound.RTG.Event (Event (..), isOnset, zipValuesWithOnsets)
import Sound.RTG.OscMessages (superDirtMessage, superDirtPort)
import Sound.RTG.RhythmicPattern (Rhythmic, rhythm)
import Sound.RTG.TimedMonad
  ( Micro (..),
    MonadRef (..),
    TIO,
    TimedMonad (..),
  )
import Sound.RTG.UnSafe (readcps)
import Sound.RTG.Utils (patternEventDurationSec)
import System.IO.Unsafe (unsafePerformIO)

-- TODO: This module still depends on Event constructors.
-- Can it be decoupled?

-- * Data Types

-- | A map containing all currently running patterns.
-- Analogous to the /bundle/ of a /fiber bundle/.
type PatternPool = HashMap PatternId SequencerPattern

-- | Pattern keys in a 'PatternPool' are 'Int'.
type PatternId = Int

-- | Events with output data.
-- Analogous to the /fiber/ of a /fiber bundle/.
type SequencerPattern = [(Event, Maybe [Output])]

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

-- | TODO: Add a switch for changing the event matching strategy.
toSequencerPattern :: (Rhythmic a) => a -> [[Output]] -> SequencerPattern
toSequencerPattern pattern outputs =
  let eventPattern = rhythm pattern
   in zipValuesWithOnsets eventPattern outputs

-- * PatternPool State in Stored References.

-- | Initialize an empty pattern pool in a mutable variable.
-- It is exposed outside of 'IO' as a mutable shared state
-- to be able to update it within the ghci runtime.
patternPoolRef :: IORef PatternPool
patternPoolRef = unsafePerformIO $ newIORef (Map.empty)

storePatternPool :: IO ()
storePatternPool = do
  pool <- readIORef patternPoolRef
  writeStore (Store 0) pool

queriePatterns :: IO ()
queriePatterns = readIORef patternPoolRef >>= print . Map.keys

-- | Add or update a pattern in the pattern pool.
updatePatternPool :: PatternId -> SequencerPattern -> IO ()
updatePatternPool id pattern = modifyIORef' patternPoolRef $ Map.insert id pattern

-- Test Examples:
-- updatePatternPool 1 [(Onset, Just [Osc "cp"])]
-- updatePatternPool 2 [(Onset, Just [Osc "bd", Osc "sn"]), (Onset, Just [Osc "blip"])]
-- updatePatternPool 3 [(Rest, Nothing), (Onset, Just [Osc "can"], (Rest, Nothing))]

-- | Remove a pattern from the pool.
removePattern :: PatternId -> IO ()
removePattern id = modifyIORef' patternPoolRef $ Map.delete id

-- * Global Pattern. Analogous to the /base space/ of a /fiber bundle/.

-- | The global pattern obtained from merging all patterns in the pool.
globalPattern :: IO (SequencerPattern)
globalPattern = do
  patternPool <- readIORef patternPoolRef
  let patternLengthsMap = Map.map (length) patternPool
      leastCommonMultiple = Map.foldl' lcm 1 patternLengthsMap
      adjustedPatterns = Map.map (alignPattern leastCommonMultiple) patternPool
      gp = Map.foldl' (matchOutputEvents) [] adjustedPatterns
  pure gp

{-@ alignPattern :: Integral a => n:a
                 -> {pttrn : SequencerPattern | n `mod` (length pttrn) == 0 }
                 -> {pttrn' : SequencerPattern | length pttrn' == n}@-}

-- | Align a 'SequencerPattern' to a finer grain (/i.e./ to a bigger discrete chromatic universe).
alignPattern :: (Integral a) => a -> SequencerPattern -> SequencerPattern
alignPattern grain pttrn =
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

-- * Play state

-- | Current play status. 'Nothing' stands for no playback.
-- A value gives the current playback process reference.
playRef :: IORef (Maybe (Ref TIO ()))
playRef = unsafePerformIO $ newIORef Nothing

storePlayState :: IO ()
storePlayState = do
  status <- readIORef playRef
  writeStore (Store 1) status

playStatus :: IO String
playStatus = do
  currentlyPlaying <- readIORef playRef
  if isNothing currentlyPlaying
    then pure "Nothing is playing"
    else pure "Just playing"

-- * Play Functionality

-- $playFuncionality
-- By lifting actions into the Timed IO Monad (TIO) we can forget about
-- managing computation time drifts explicitly and get (approximately)
-- correct timing.

-- | Play through the Timed IO Monad 'TIO' in a loop.
-- The 'Micro' parameter should be generated from the global cps value;
-- it defines the duration of each event.
playSequencerPattern :: Micro -> SequencerPattern -> TIO ()
playSequencerPattern dur sp = do
  ref <- fork $ mapM_ (eventOutput dur . snd) $ cycle sp
  lift $ writeIORef playRef (Just ref)

-- | Generates an event playback by the given duration inside the timed IO monad.
eventOutput :: Micro -> Maybe [Output] -> TIO ()
eventOutput dur Nothing = delay dur
eventOutput dur (Just outputs) = mapM_ (\x -> instantOut dur x >> delay dur) outputs

-- | TODO: output for Note is messing up time. For the mean time I'll forget about it.
instantOut :: Micro -> Output -> TIO ()
instantOut dur (Osc sample) = lift $ do
  port <- superDirtPort
  Osc.sendMessage port (superDirtMessage sample)
instantOut dur (Note pitch) =
  let Micro m = dur
      microsecs = fromIntegral m
      secs = microsecs / 10 ^ 6
   in lift $ do
        play $ note secs pitch -- too slow to catch up?

-- * Managing the Sequencer State

inSequencer :: IO a -> IO a
inSequencer action = do
  returnValue <- action
  refreshSequencer
  storePatternPool
  storePlayState
  pure returnValue

-- | Querie the environment and re-trigger excecution.
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
    then run $ playSequencerPattern dur gp -- resume
    else run $ do
      -- kill running sequencer pattern and resume
      freeze (fromJust currentlyPlaying)
      delay dur
      playSequencerPattern dur gp

cpsToTimeStamp :: (RealFrac a, Integral b) => a -> b -> Micro
cpsToTimeStamp cps pttrnLen = Micro . round $ 1 / (toRational cps * fromIntegral pttrnLen) * 10 ^ 6

-- * Execution Interface

-- | Default pattern ID following the Tidal Cycles idiom.
d1, d2, d3, d4, d5, d6, d7, d8 :: (Rhythmic a) => [SampleName] -> a -> IO ()
d1 samples = playPattern 1 (map (\s -> [Osc s]) samples)
d2 samples = playPattern 2 (map (\s -> [Osc s]) samples)
d3 samples = playPattern 3 (map (\s -> [Osc s]) samples)
d4 samples = playPattern 4 (map (\s -> [Osc s]) samples)
d5 samples = playPattern 5 (map (\s -> [Osc s]) samples)
d6 samples = playPattern 6 (map (\s -> [Osc s]) samples)
d7 samples = playPattern 7 (map (\s -> [Osc s]) samples)
d8 samples = playPattern 8 (map (\s -> [Osc s]) samples)

-- | Entry point for a pattern execution. It add the pattern to the pool
-- and updates the sequencer.
-- TODO: Add argument to modify event matching strategy for output values.
playPattern :: (Rhythmic a) => PatternId -> [[Output]] -> a -> IO ()
playPattern id outputs pattern = inSequencer $ do
  let newSequencerPattern = toSequencerPattern pattern outputs
  updatePatternPool id newSequencerPattern

-- | Convinience alias. Used to resume play after 'stopAll'.
resume :: IO ()
resume = refreshSequencer

-- | Removes a pattern both from the pool and playback.
kill :: PatternId -> IO ()
kill id = inSequencer $ do
  removePattern id
  pool <- readIORef patternPoolRef
  if pool == Map.empty
    then modifyIORef' playRef $ const Nothing
    else pure ()

-- | Clear the pool and stop playing.
clear :: IO ()
clear = inSequencer $ do
  stopAll
  modifyIORef' playRef $ const Nothing
  modifyIORef' patternPoolRef $ const Map.empty

-- | Stop playing, but keep patterns in the pool.
stopAll :: IO ()
stopAll = inSequencer $ run go
  where
    go :: TIO ()
    go = do
      currentlyPlaying <- lift $ readIORef playRef
      freeze $ fromJust currentlyPlaying
      lift $ modifyIORef' playRef $ const Nothing
