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

import Control.Monad (mapM_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Euterpea (Pitch, PitchClass (..), note, play)
import Foreign.Store
  ( Store (..),
    lookupStore,
    readStore,
    storeAction,
    withStore,
    writeStore,
  )
import qualified Sound.Osc.Fd as Osc
import Sound.RTG.Event (Event (..), isOnset, zipValuesWithOnsets)
import Sound.RTG.OscMessages (sendSuperDirtSample)
import Sound.RTG.RhythmicPattern (Rhythmic, rhythm)
import Sound.RTG.TimedMonad
  ( Micro (..),
    MonadRef (..),
    TIO,
    TimedMonad (..),
  )
import Sound.RTG.UnSafe (readcps)
import Sound.RTG.Utils (patternEventDurationSec)

-- TODO: This module still depends on Event constructors.
-- Can it be decoupled?

-- * Data Types

-- | A map containing all currently running patterns.
-- Analogous to the /bundle/ of a /fiber bundle/.
-- TODO: Change to newtype to define custom show instance.
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

data SequencerMode = Solo | Global

-- | TODO: Add a switch for changing the event matching strategy.
toSequencerPattern :: (Rhythmic a) => a -> [[Output]] -> SequencerPattern
toSequencerPattern pattern outputs =
  let eventPattern = rhythm pattern
   in zipValuesWithOnsets eventPattern outputs

-- * PatternPool State

-- $patternpoolstate
-- The 'PatternPool' is stored in the 'patternPoolStore', so here whe define custom
-- 'Store' operations to manipulate it.
-- 'Store's are used for the sequencer state to persist @ghci@ reloads.

-- | The 'Store' index for the'PatternPool'.
patternPoolStore :: Store PatternPool
patternPoolStore = Store 0

getPatternPool :: IO PatternPool
getPatternPool = readStore patternPoolStore

queriePatterns :: IO [PatternId]
queriePatterns = withStore patternPoolStore $ pure . Map.keys

-- | Store and return the result of an update operation on the stored value.
updateStore :: Store a -> (a -> IO a) -> IO a
updateStore store update = storeAction store . withStore store $ update

-- | Store an empty 'PatternPool' in the 'patternPoolStore'.
resetPatternPool :: IO ()
resetPatternPool = writeStore patternPoolStore $ Map.empty

-- | Add or update a pattern in the pattern pool.
addPatternToPool :: PatternId -> SequencerPattern -> IO PatternPool
addPatternToPool id pattern =
  updateStore patternPoolStore $ pure . Map.insert id pattern

-- | Remove a pattern from the pool.
removePattern :: PatternId -> IO PatternPool
removePattern id = updateStore patternPoolStore $ pure . Map.delete id

-- * Global Pattern

-- $globalpattern
-- Analogous to the /base space/ of a /fiber bundle/. The 'globalPattern' is the means
-- to play the patterns contained in the 'PatternPool'. The /merging/ proceduce it
-- implements is analogous to a /projection/.

-- | The global pattern obtained from merging all patterns in the pool.
globalPattern :: IO SequencerPattern
globalPattern = withStore patternPoolStore $
  \patternPool -> do
    let patternLengthsMap = Map.map (length) patternPool
        leastCommonMultiple = Map.foldl' lcm 1 patternLengthsMap
        adjustedPatterns = Map.map (alignPattern leastCommonMultiple) patternPool
        gp = Map.foldl' (matchOutputEvents) [] adjustedPatterns
    pure gp

soloPattern :: PatternId -> IO (Maybe SequencerPattern)
soloPattern id = withStore patternPoolStore $
  \patternPool -> pure $ Map.lookup id patternPool

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

-- * Play Status

-- $playstatus
-- The 'playStatus' is inferred from a 'Maybe (Ref TIO ())' (a 'TimedMonad' reference)
-- value inside a 'Store'.
-- Under the hood, the reference carries a 'Control.Concurrency.threadId'
-- as a process handle. 'Nothing' stands for no-playback.

-- | Current play thread reference store. 'Nothing' stands for no-playback.
playThreadStore :: Store (Maybe (Ref TIO ()))
-- playThreadStore = unsafePerformIO $ newStore $ Nothing
playThreadStore = Store 1

-- | Updates the 'playThreadStore' with the given reference and
-- returns both the old and new playing thread references.
updatePlayThread :: Maybe (Ref TIO ()) -> IO (Maybe (Ref TIO ()), Maybe (Ref TIO ()))
updatePlayThread currentlyPlaying = do
  wasPlaying <- readStore playThreadStore
  writeStore playThreadStore currentlyPlaying
  pure (wasPlaying, currentlyPlaying)

getPlayThread :: IO (Maybe (Ref TIO ()))
getPlayThread = do
  currentlyPlaying <- readStore playThreadStore
  pure currentlyPlaying

playStatus :: IO String
playStatus = do
  withStore playThreadStore $ \currentlyPlaying -> do
    case currentlyPlaying of
      Nothing -> pure "Nothing is playing"
      Just _ -> pure "Just playing"

-- * Play Functionality

-- $playFuncionality
-- By lifting actions into the Timed IO Monad (TIO) we can forget about
-- managing computation time drifts explicitly and get (approximately)
-- correct timing.

-- | Play a pattern in the Timed IO Monad 'TIO'.
-- The playback is in a loop and runs on a forked thread.
-- This functions is called at 'refreshSequencerPattern'
-- to play the current 'globalPattern'.
-- There the 'Micro' parameter corresponds to the duration of each event,
-- and is derived from the global cps value and the pattern length.
playSequencerPattern :: Micro -> SequencerPattern -> TIO ()
playSequencerPattern dur sp =
  if dur == 0 || null sp
    then do
      (wasPlaying, _) <- lift $ updatePlayThread Nothing
      case wasPlaying of
        Nothing -> pure ()
        Just thread -> freeze thread
    else do
      wasPlaying <- lift $ getPlayThread
      case wasPlaying of
        Nothing -> do
          thread <- fork $ patternOutput dur sp
          _ <- lift $ updatePlayThread (Just thread)
          pure ()
        Just oldThread -> do
          freeze oldThread
          delay dur
          newThread <- fork (patternOutput dur sp)
          _ <- lift $ updatePlayThread (Just newThread)
          pure ()

patternOutput :: Micro -> [(a, Maybe [Output])] -> TIO ()
patternOutput dur sp = mapM_ (eventOutput dur . snd) $ cycle sp

-- | Generates an event playback by the given duration inside the timed IO monad.
eventOutput :: Micro -> Maybe [Output] -> TIO ()
eventOutput dur Nothing = delay dur
eventOutput dur (Just outputs) = mapM_ (\x -> instantOut dur x >> delay dur) outputs

-- | TODO: output for Note is messing up time. For the mean time I'll forget about it.
instantOut :: Micro -> Output -> TIO ()
instantOut dur (Osc sample) = lift $ sendSuperDirtSample sample
instantOut dur (Note pitch) =
  let Micro m = dur
      microsecs = fromIntegral m
      secs = microsecs / 10 ^ 6
   in lift $ do
        play $ note secs pitch -- too slow to catch up?

-- * Sequencer State

-- | Wrapper for operations that depend on the sequencer state.
inSequencer :: SequencerMode -> PatternId -> IO a -> IO a
inSequencer mode id action = do
  storeInit
  returnValue <- action
  runSequencer mode id
  pure returnValue

-- | Initializes the 'patternPoolStore' and the 'playThreadStore'.
storeInit :: IO ()
storeInit = do
  [maybePoolStore, maybeThreadStore] <- mapM lookupStore [0, 1]
  case (maybePoolStore, maybeThreadStore) of
    (Nothing, Nothing) -> resetPatternPool >> writeStore playThreadStore Nothing
    (Nothing, _) -> resetPatternPool
    (_, Nothing) -> writeStore playThreadStore Nothing
    (_, _) -> pure ()

-- | Querie the environment and (re-)trigger execution.
runSequencer :: SequencerMode -> PatternId -> IO ()
runSequencer mode id = do
  gp <- globalPattern
  cps <- readcps
  let len = length gp
      dur = if len /= 0 then cpsToTimeStamp cps len else 0
  case mode of
    Global -> run $ playSequencerPattern dur gp
    Solo -> do
      sp <- soloPattern id
      case sp of
        Nothing ->
          error $
            unlines
              [ "runSequencer:",
                "\nPattern " ++ show id ++ "is not in the pool!",
                "\nUse 'queriePatterns' to show available patterns."
              ]
        Just pttrn -> run $ playSequencerPattern dur $
          alignPattern (lcm len $ length pttrn) pttrn

cpsToTimeStamp :: (RealFrac a, Integral b) => a -> b -> Micro
cpsToTimeStamp cps pttrnLen = Micro . round $ 1 / (toRational cps * fromIntegral pttrnLen) * 10 ^ 6

-- * Execution Interface

-- | Default pattern ID following the Tidal Cycles idiom.
d1, d2, d3, d4, d5, d6, d7, d8 :: (Rhythmic a) => [SampleName] -> a -> IO PatternPool
d1 samples = playPattern Global 1 (map (\s -> [Osc s]) samples)
d2 samples = playPattern Global 2 (map (\s -> [Osc s]) samples)
d3 samples = playPattern Global 3 (map (\s -> [Osc s]) samples)
d4 samples = playPattern Global 4 (map (\s -> [Osc s]) samples)
d5 samples = playPattern Global 5 (map (\s -> [Osc s]) samples)
d6 samples = playPattern Global 6 (map (\s -> [Osc s]) samples)
d7 samples = playPattern Global 7 (map (\s -> [Osc s]) samples)
d8 samples = playPattern Global 8 (map (\s -> [Osc s]) samples)

-- | Entry point for a pattern execution. It add the pattern to the pool
-- and updates the sequencer.
-- TODO: Add argument to modify event matching strategy for output values.
playPattern :: (Rhythmic a) => SequencerMode -> PatternId -> [[Output]] -> a -> IO PatternPool
playPattern mode id outputs pttrn = inSequencer mode id $ do
  let newSequencerPattern = toSequencerPattern pttrn outputs
  addPatternToPool id newSequencerPattern

s1, s2, s3, s4, s5, s6, s7, s8 :: IO ()
s1 = solo 1
s2 = solo 2
s3 = solo 3
s4 = solo 1
s5 = solo 5
s6 = solo 6
s7 = solo 7
s8 = solo 8

solo :: PatternId -> IO ()
solo id = inSequencer Solo id $ pure ()

-- | Resume play of the 'globalPattern'.
resume :: IO ()
resume = inSequencer Global 0 $ pure ()

-- | Removes a pattern both from the pool and playback.
kill :: PatternId -> IO PatternPool
kill id = inSequencer Global id $ removePattern id

-- | Clear the pool and stop playing.
-- REVIEW: why not "inSequencer"?
clear :: IO ()
clear = do
  stopAll
  resetPatternPool

-- | Stop playing, but keep patterns in the pool.
-- TODO: bug when "inSequencer"
stopAll :: IO ()
stopAll = run go
  where
    go :: TIO ()
    go = do
      currentlyPlaying <- lift $ getPlayThread
      case currentlyPlaying of
        Nothing -> lift $ putStrLn "Nothing is playing in silence loudly, listen..."
        Just thread -> do
          freeze thread
          _ <- lift $ updatePlayThread Nothing
          pure ()
