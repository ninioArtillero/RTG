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
data SequencerPattern = SequencerPattern
  { getOutputPattern :: !OutputPattern,
    status :: !PatternStatus
  }

type OutputPattern = [(Event, Maybe [Output])]

data PatternStatus = Idle | Running deriving (Eq)

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

data SequencerMode = Solo Int | Global deriving (Show)

-- | TODO: Add a switch for changing the event matching strategy.
toOutputPattern :: (Rhythmic a) => a -> [[Output]] -> OutputPattern
toOutputPattern pattern outputs =
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

activePatterns :: IO [PatternId]
activePatterns = withStore patternPoolStore $ pure . Map.keys . Map.filter ((== Running) . status)

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

stopPattern :: PatternId -> IO PatternPool
stopPattern id = updateStore patternPoolStore $ pure . Map.adjust (\sequencerPattern -> sequencerPattern {status = Idle}) id

activatePattern :: PatternId -> IO PatternPool
activatePattern id = updateStore patternPoolStore $ pure . Map.adjust (\sequencerPattern -> sequencerPattern {status = Running}) id

activateAllPatterns :: IO PatternPool
activateAllPatterns = updateStore patternPoolStore $ pure . Map.map (\sequencerPattern -> sequencerPattern {status = Running})

stopAllPatterns :: IO PatternPool
stopAllPatterns = updateStore patternPoolStore $ pure . Map.map (\sequencerPattern -> sequencerPattern {status = Idle})

-- * Global Pattern

-- $globalpattern
-- Analogous to the /base space/ of a /fiber bundle/. The 'globalPattern' is the means
-- to play the patterns contained in the 'PatternPool'. The /merging/ proceduce it
-- implements is analogous to a /projection/.

-- | The global pattern obtained from merging all running patterns in the pool.
globalPattern :: IO OutputPattern
globalPattern = withStore patternPoolStore $
  \patternPool -> do
    let patternLengthsMap = Map.map (length . getOutputPattern) patternPool
        leastCommonMultiple = Map.foldl' lcm 1 patternLengthsMap
        runningPatterns = Map.filter ((== Running) . status) patternPool
        adjustedOutputPatterns = Map.map (alignPattern leastCommonMultiple . getOutputPattern) runningPatterns
        gp = Map.foldl' (matchOutputEvents) [] adjustedOutputPatterns
    pure gp

soloPattern :: PatternId -> IO (Maybe SequencerPattern)
soloPattern id = withStore patternPoolStore $
  \patternPool -> pure $ Map.lookup id patternPool

{-@ alignPattern :: Integral a => n:a
                 -> {pttrn : OutputPattern | n `mod` (length pttrn) == 0 }
                 -> {pttrn' : OutputPattern | length pttrn' == n}@-}

-- | Align a 'OutputPattern' to a finer grain (/i.e./ to a bigger discrete chromatic universe).
alignPattern :: (Integral a) => a -> OutputPattern -> OutputPattern
alignPattern grain pttrn =
  let factor = (fromIntegral grain) `div` (length pttrn)
   in concat $ map (\x -> x : replicate (factor - 1) (Rest, Nothing)) pttrn

{-@ matchOutputEvents :: pttrn : OutputPattern
                      -> {pttrn' : OutputPattern | length pttrn == length pttrn'}
                      -> {pttrn'' : OutputPattern | length pttrn == length pttrn''} @-}

-- | Join the outputs of matching events. Expects patterns of the same length.
matchOutputEvents :: OutputPattern -> OutputPattern -> OutputPattern
matchOutputEvents [] pttrn' = pttrn' -- Should empty cases be handled here?
matchOutputEvents pttrn [] = pttrn
matchOutputEvents pttrn pttrn' = zipWith f pttrn pttrn'
  where
    f (event, outputs) (event', outputs') =
      if isOnset event || isOnset event'
        then (Onset, Just $ (fromMaybe [] outputs) ++ (fromMaybe [] outputs'))
        else (Rest, Nothing)

-- * Sequencer State

-- | Current play thread reference store.
sequencerStateStore :: Store SequencerState
-- sequencerStateStore = unsafePerformIO $ newStore $ Nothing
sequencerStateStore = Store 1

-- | A 'SequencerState' contains a 'Maybe (Ref TIO ())' and the 'SequencerMode'.
-- A 'Ref TIO ()' is a Timed IO Monad ('TIO') reference to the sequencer running thread.
-- 'Nothing' in this field means the sequencer is not running.
type SequencerState = (SequencerMode, Maybe (Ref TIO ()))

-- | Updates the 'sequencerStateStore' with the given reference and
-- returns both the old and new playing thread references.
updateSequencerThread :: Maybe (Ref TIO ()) -> IO SequencerState
updateSequencerThread currentlyPlaying = updateStore sequencerStateStore $
  \(mode, _) -> pure (mode, currentlyPlaying)

updateSequencerMode :: SequencerMode -> IO SequencerState
updateSequencerMode mode = updateStore sequencerStateStore $
  \(_, currentlyPlaying) -> pure (mode, currentlyPlaying)

getSequencerThread :: IO (Maybe (Ref TIO ()))
getSequencerThread = do
  (_, currentlyPlaying) <- readStore sequencerStateStore
  pure currentlyPlaying

getSequencerMode :: IO SequencerMode
getSequencerMode = do
  (mode, _) <- readStore sequencerStateStore
  pure mode

playStatus :: IO String
playStatus = do
  withStore sequencerStateStore $ \(mode, currentlyPlaying) -> do
    case currentlyPlaying of
      Nothing -> pure $ "Nothing is playing in " ++ show mode ++ " mode"
      Just _ -> pure $ "Just playing in " ++ show mode ++ " mode"

-- * Play Functionality

-- $playFuncionality
-- By lifting actions into the Timed IO Monad (TIO) we can forget about
-- managing computation time drifts explicitly and get (approximately)
-- correct timing.

-- | Play a pattern in the Timed IO Monad 'TIO'.
-- The playback is in a loop and runs on a forked thread.
-- This functions is called at 'runSequencer'
-- to play the current 'globalPattern'.
-- There the 'Micro' parameter corresponds to the duration of each event,
-- and is derived from the global cps value and the pattern length.
playOutputPattern :: Micro -> OutputPattern -> TIO ()
playOutputPattern dur sp =
  if dur == 0 || null sp
    then do
      wasPlaying <- lift $ getSequencerThread
      case wasPlaying of
        Nothing -> pure ()
        Just thread -> do
          freeze thread
          _ <- lift $ updateSequencerThread Nothing
          pure ()
    else do
      wasPlaying <- lift $ getSequencerThread
      case wasPlaying of
        Nothing -> do
          thread <- fork $ patternOutput dur sp
          _ <- lift $ updateSequencerThread (Just thread)
          pure ()
        Just oldThread -> do
          freeze oldThread
          delay dur -- the swap time matches the event time
          newThread <- fork (patternOutput dur sp)
          _ <- lift $ updateSequencerThread (Just newThread)
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
inSequencer :: IO a -> IO a
inSequencer action = do
  storeInit
  returnValue <- action
  runSequencer
  pure returnValue

-- | Initializes the 'patternPoolStore' and the 'sequencerStateStore'.
storeInit :: IO ()
storeInit = do
  [maybePoolStore, maybeThreadStore] <- mapM lookupStore [0, 1]
  case (maybePoolStore, maybeThreadStore) of
    (Nothing, Nothing) -> resetPatternPool >> writeStore sequencerStateStore (Global, Nothing)
    (Nothing, _) -> resetPatternPool
    (_, Nothing) -> writeStore sequencerStateStore (Global, Nothing)
    (_, _) -> pure ()

-- | Querie the environment and (re-)trigger execution.
runSequencer :: IO ()
runSequencer = do
  gp <- globalPattern
  mode <- getSequencerMode
  cps <- readcps
  let len = length gp
      dur = if len /= 0 then cpsToTimeStamp cps len else 0
  case mode of
    Global -> run $ playOutputPattern dur gp
    Solo patternId -> do
      sp <- soloPattern patternId
      case sp of
        Nothing -> do
          putStrLn $
            unlines
              [ "Pattern missing!",
                "Use 'queriePatterns' to show available patterns to 'solo',",
                "'unsolo' the sequencer or play 'd" ++ show patternId ++ "'."
              ]
          run $ playOutputPattern 0 []
        -- error $
        --   unlines
        --     [ ">>at 'runSequencer' in 'Solo' mode<<",
        --       "Pattern " ++ show patternId ++ " is not in the pool!",
        --       "Use 'queriePatterns' to show available patterns."
        --     ]
        Just pttrn -> do
          activatePattern patternId
          run $
            playOutputPattern dur $
              let outputPattern = getOutputPattern pttrn
               in alignPattern (lcm len $ length outputPattern) outputPattern

cpsToTimeStamp :: (RealFrac a, Integral b) => a -> b -> Micro
cpsToTimeStamp cps pttrnLen = Micro . round $ 1 / (toRational cps * fromIntegral pttrnLen) * 10 ^ 6

-- * Execution Interface

-- | Default pattern ID following the Tidal Cycles idiom.
d1, d2, d3, d4, d5, d6, d7, d8 :: (Rhythmic a) => [SampleName] -> a -> IO PatternPool
d1 samples = addPattern Running 1 (map (\s -> [Osc s]) samples)
d2 samples = addPattern Running 2 (map (\s -> [Osc s]) samples)
d3 samples = addPattern Running 3 (map (\s -> [Osc s]) samples)
d4 samples = addPattern Running 4 (map (\s -> [Osc s]) samples)
d5 samples = addPattern Running 5 (map (\s -> [Osc s]) samples)
d6 samples = addPattern Running 6 (map (\s -> [Osc s]) samples)
d7 samples = addPattern Running 7 (map (\s -> [Osc s]) samples)
d8 samples = addPattern Running 8 (map (\s -> [Osc s]) samples)

-- | Entry point for a pattern execution. It add the pattern to the pool
-- and updates the sequencer.
-- TODO: Add argument to modify event matching strategy for output values.
addPattern :: (Rhythmic a) => PatternStatus -> PatternId -> [[Output]] -> a -> IO PatternPool
addPattern st id outputs pttrn = inSequencer $ do
  let newSequencerPattern = toOutputPattern pttrn outputs
  addPatternToPool id $ SequencerPattern {getOutputPattern = newSequencerPattern, status = st}

solo :: PatternId -> IO SequencerState
solo patternId = inSequencer $ updateSequencerMode (Solo patternId)

unsolo :: IO SequencerState
unsolo = inSequencer $ updateSequencerMode Global

stop :: PatternId -> IO PatternPool
stop id = inSequencer $ stopPattern id

-- | Set all patterns to 'Idle'.
stopAll :: IO PatternPool
stopAll = inSequencer $ stopAllPatterns

start :: PatternId -> IO PatternPool
start id = inSequencer $ activatePattern id

-- | Set all pattern to 'Running'.
startAll :: IO PatternPool
startAll = inSequencer $ activateAllPatterns

-- | Removes a pattern from both the pool and playback.
kill :: PatternId -> IO PatternPool
kill id = inSequencer $ removePattern id

-- | Clear the pool and stop playing.
-- REVIEW: why not "inSequencer"?
clear :: IO ()
clear = inSequencer $ do resetPatternPool

-- | Reset the sequencer with the current state.
reset :: IO ()
reset = inSequencer $ pure ()

-- | Kill playing thread
hush :: IO ()
hush = run go
  where
    go :: TIO ()
    go = do
      currentlyPlaying <- lift $ getSequencerThread
      case currentlyPlaying of
        Nothing -> lift $ putStrLn "Nothing is playing in silence loudly, listen..."
        Just thread -> do
          freeze thread
          _ <- lift $ updateSequencerThread Nothing
          pure ()
