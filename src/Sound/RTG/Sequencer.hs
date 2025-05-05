-- |
-- Module      : Sequencer
-- Description : A sequencer using the Timed IO Monad.
-- Copyright   : (c) Xavier Góngora, 2025
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- The execution model of the sequencer follows an analogy with a /fiber bundle/,
-- where the 'PatternPool' is the /bundle/, the 'SequencerPattern'
-- is the /fiber/, and the 'globalPattern' is the /base space/.
-- Transformations on the bundle are projected into the base, which
-- is the pattern to be executed.
--
-- The implementation depends on the `foreign-store` package, which is explicit
-- about the fact that a 'Store' is not thread safe. Care is taken
-- for state updates to be done only within the main thread.
module Sound.RTG.Sequencer
  ( p,
    start,
    startAll,
    stop,
    stopAll,
    solo,
    unsolo,
    setcps,
    setbpm,
    kill,
    clear,
    status,
    querie,
    active,
    idle,
    reset,
    hush,
  )
where

import Control.Monad (forever, mapM_)
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
import Sound.RTG.Async (withAsync)
import Sound.RTG.Event (Event (..), isOnset, pairValuesWithOnsets)
import Sound.RTG.OscMessages (sendSuperDirtSample)
import Sound.RTG.RhythmicPattern (Rhythmic, rhythm)
import Sound.RTG.TimedMonad
  ( Micro (..),
    MonadRef (..),
    TIO,
    TRef (TRef),
    TimedMonad (..),
    dur,
  )
import Prelude hiding (read)

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
    getPatternStatus :: !PatternStatus
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

data SequencerMode = Solo !Int | Global deriving (Show)

-- * Sequencer Interface

-- | Run a pattern in the sequencer.
-- p :: Rhythmic a => [SampleName] -> PatternID -> a -> IO PatternPool
p patternId samples = addPattern Running patternId (map (\s -> [Osc s]) samples)

-- | Entry point for a pattern execution. Adds the pattern to the pool.
-- TODO: Add argument to modify event matching strategy for output values.
addPattern :: (Rhythmic a) => PatternStatus -> PatternId -> [[Output]] -> a -> IO PatternPool
addPattern patternStatus id outputs pttrn = inSequencer True $ do
  let newSequencerPattern = toOutputPattern pttrn outputs
  addPatternToPool id $
    SequencerPattern
      { getOutputPattern = newSequencerPattern,
        getPatternStatus = patternStatus
      }

status :: IO String
status = inSequencer False $ sequencerStatus

querie :: IO [PatternId]
querie = inSequencer False $ queriePatterns

active :: IO [PatternId]
active = inSequencer False $ runningPatterns

idle :: IO [PatternId]
idle = inSequencer False $ idlePatterns

setcps :: CPS -> IO SequencerState
setcps cps = inSequencer True $ updateSequencerCPS cps

setbpm :: BPC -> BPM -> IO SequencerState
setbpm bpc bpm =
  let newcps = (fromIntegral bpm / 60) / fromIntegral bpc
   in setcps newcps

solo :: PatternId -> IO SequencerState
solo patternId = inSequencer True $ updateSequencerMode (Solo patternId)

unsolo :: IO SequencerState
unsolo = inSequencer True $ updateSequencerMode Global

stop :: PatternId -> IO PatternPool
stop id = inSequencer True $ stopPattern id

-- | Set all patterns to 'Idle'.
stopAll :: IO PatternPool
stopAll = inSequencer True $ stopAllPatterns

start :: PatternId -> IO PatternPool
start id = inSequencer True $ activatePattern id

-- | Set all pattern to 'Running'.
startAll :: IO PatternPool
startAll = inSequencer True $ activateAllPatterns

-- | Removes a pattern from both the pool.
kill :: PatternId -> IO PatternPool
kill id = inSequencer True $ removePattern id

-- | Clear the pool.
clear :: IO ()
clear = inSequencer True $ do resetPatternPool

-- | Reset the sequencer with the current state.
reset :: IO ()
reset = inSequencer True $ pure ()

-- | Kill playing thread.
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

-- * Sequencer Operation

-- | Wrapper for operations that depend on the sequencer state.
-- Executes the given action and returns its value, running the
-- sequencer with the action's side effects.
inSequencer :: Bool -> IO a -> IO a
inSequencer update action = do
  storeInit
  returnValue <- action
  runSequencer update
  pure returnValue

-- | Initializes the 'patternPoolStore' to an empty 'PatternPool',
-- and the 'sequencerStateStore' to global mode, none playback thread and
-- a default cycles-per-second value.
storeInit :: IO ()
storeInit = do
  [maybePoolStore, maybeThreadStore] <- mapM lookupStore [0, 1]
  case (maybePoolStore, maybeThreadStore) of
    (Nothing, Nothing) -> resetPatternPool >> writeStore sequencerStateStore (Global, Nothing, defaultCPS)
    (Nothing, _) -> resetPatternPool
    (_, Nothing) -> writeStore sequencerStateStore (Global, Nothing, defaultCPS)
    (_, _) -> pure ()

-- | Querie the environment's shared state and (re-)trigger execution.
runSequencer :: Bool -> IO ()
runSequencer update =
  if not update
    then pure ()
    else do
      gp <- globalPattern
      (mode, currentlyPlaying, cps) <- getSequencerState
      let len = length gp
          eventDur = if len /= 0 then cpsToTimeStamp cps len else 0
          cycleDur = cpsToTimeStamp cps 1
      case mode of
        Global -> run $ sequenceOutputPattern eventDur gp
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
              -- Run a silent pattern if the soloed pattern is not found.
              run $ sequenceOutputPattern 0 []
            Just pttrn -> do
              activatePattern patternId
              run $
                sequenceOutputPattern eventDur $
                  let outputPattern = getOutputPattern pttrn
                   in alignPattern (lcm len $ length outputPattern) outputPattern

-- * Sequencer State

-- | Contains the shared global 'SequencerState'.
sequencerStateStore :: Store SequencerState
-- sequencerStateStore = unsafePerformIO $ newStore $ Nothing
sequencerStateStore = Store 1

-- | A 'SequencerState' its a 'Maybe (Ref TIO ())' and a 'SequencerMode'.
-- A 'Ref TIO ()' is a Timed IO Monad ('TIO') reference to the sequencer running thread.
-- 'Nothing' in this field means the sequencer is not running.
type SequencerState = (SequencerMode, Maybe (Ref TIO ()), CPS)

-- | Cycles Per Second
type CPS = Rational

-- | Beats Per Cycle
type BPC = Int

-- | Beats Per Minuto
type BPM = Int

defaultCPS :: CPS
defaultCPS = 0.5

updateSequencerThread :: Maybe (Ref TIO ()) -> IO SequencerState
updateSequencerThread currentlyPlaying = updateStore sequencerStateStore $
  \(mode, _, cps) -> pure (mode, currentlyPlaying, cps)

updateSequencerMode :: SequencerMode -> IO SequencerState
updateSequencerMode mode = updateStore sequencerStateStore $
  \(_, currentlyPlaying, cps) -> pure (mode, currentlyPlaying, cps)

updateSequencerCPS :: CPS -> IO SequencerState
updateSequencerCPS newcps = updateStore sequencerStateStore $
  \(mode, currentlyPlaying, _) -> pure (mode, currentlyPlaying, newcps)

getSequencerState :: IO SequencerState
getSequencerState = readStore sequencerStateStore

getSequencerThread :: IO (Maybe (Ref TIO ()))
getSequencerThread = do
  (_, currentlyPlaying, _) <- readStore sequencerStateStore
  pure currentlyPlaying

getSequencerMode :: IO SequencerMode
getSequencerMode = do
  (mode, _, _) <- readStore sequencerStateStore
  pure mode

getSequencerCPS :: IO CPS
getSequencerCPS = do
  (_, _, cps) <- readStore sequencerStateStore
  pure cps

-- TODO: add multiple lines of output to visually divide information.
sequencerStatus :: IO String
sequencerStatus = do
  withStore sequencerStateStore $ \(mode, currentlyPlaying, cps) -> do
    let floatCPS :: Float
        floatCPS = fromRational cps
    case currentlyPlaying of
      Nothing ->
        pure $
          "Nothing is playing in "
            ++ show mode
            ++ " mode"
            ++ " at "
            ++ show floatCPS
            ++ " cycles-per-second."
      Just _ ->
        pure $
          "Just playing in "
            ++ show mode
            ++ " mode"
            ++ " at "
            ++ show floatCPS
            ++ " cycles-per-second."

-- * Output Fuctionality

-- $outputfunctionality
-- By lifting output actions into the Timed IO Monad (TIO) we can forget about
-- managing computation time drifts explicitly and get (approximately)
-- correct timing.

-- | Sequence an 'OutputPattern' in the Timed IO Monad 'TIO'.
-- The playback is in a loop and runs on a forked thread.
-- This function is called at 'runSequencer' to /play/ an 'OutputPattern'.
-- The 'Micro' parameter corresponds to the duration of each event
-- and it is derived from the global cps value and the pattern length.
sequenceOutputPattern :: Micro -> OutputPattern -> TIO ()
sequenceOutputPattern eventDur op = do
  wasPlaying <- lift $ getSequencerThread
  if eventDur == 0 || null op
    then case wasPlaying of
      Nothing -> pure ()
      Just thread -> do
        freeze thread
        -- read thread
        _ <- lift $ updateSequencerThread Nothing
        pure ()
    else case wasPlaying of
      Nothing -> do
        thread <- fork $ patternOutput eventDur op
        _ <- lift $ updateSequencerThread (Just thread)
        pure ()
      Just oldThread -> do
          freeze oldThread
          newThread <- fork $ patternOutput eventDur op
          _ <- lift $ updateSequencerThread (Just newThread)
          -- delay dur -- NOTE: the swap time matches the event time
          -- _ <- fork $ sequenceOutputPattern dur op Nothing
          -- NOTE: Create a recursive call within a new internal thread.
          -- lift . withAsync (run $ patternOutput dur op) $
          --  \(TRef callTimeStamp (TRef (timeStamp, _))) -> do
          -- updateSequencerThread newThread
          -- run $ sequenceOutputPattern dur op (Just newThread)
          pure ()

-- TODO: un-cycle this and manage looping using a recursive call in 'sequenceOutputPattern'
patternOutput :: Micro -> [(a, Maybe [Output])] -> TIO ()
patternOutput d sp = forever $ mapM_ (eventOutput d . snd) sp

-- | Generates an event playback by the given duration inside the Timed IO Monad ('TIO').
eventOutput :: Micro -> Maybe [Output] -> TIO ()
eventOutput dur Nothing = delay dur
-- TODO: HERE may lie the cause of the weird expansion of pattern duration.
-- The list output is no simultaneous! As it should given they are associated
-- to the same event.
eventOutput dur (Just outputs) = mapM_ (\x -> instantOut dur x >> delay dur) outputs

-- | Output of single values in the Timed IO Monad ('TIO').
-- TODO: Note output is messing up time. For the mean time I'll focus on Osc output.
instantOut :: Micro -> Output -> TIO ()
instantOut _ (Osc sample) = lift $ sendSuperDirtSample sample
instantOut dur (Note pitch) =
  let Micro m = dur
      microsecs = fromIntegral m
      secs = microsecs / 10 ^ 6
   in lift $ do
        play $ note secs pitch -- too slow to catch up?

-- * PatternPool Proyection

-- | The global pattern obtained from merging all running patterns in the pool.
-- Analogous to the /base space/ of a /fiber bundle/. The 'globalPattern' is the means
-- to play the patterns contained in the 'PatternPool'. The /merging/ proceduce it
-- implements is analogous to a /projection/.
globalPattern :: IO OutputPattern
globalPattern = withStore patternPoolStore $
  \patternPool -> do
    let patternLengthsMap = Map.map (length . getOutputPattern) patternPool
        leastCommonMultiple = Map.foldl' lcm 1 patternLengthsMap
        runningPatterns = Map.filter ((== Running) . getPatternStatus) patternPool
        alignedOutputPatterns =
          Map.map (alignPattern leastCommonMultiple . getOutputPattern) runningPatterns
        gp = Map.foldl' (matchOutputEvents) [] alignedOutputPatterns
    pure gp

soloPattern :: PatternId -> IO (Maybe SequencerPattern)
soloPattern id = withStore patternPoolStore $
  -- \| TODO: align pattern here and not in 'playPatternOutput'
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
matchOutputEvents [] pttrn' = pttrn' -- NOTE: Should empty cases be handled here?
matchOutputEvents pttrn [] = pttrn
matchOutputEvents pttrn pttrn' = zipWith f pttrn pttrn'
  where
    f (event, outputs) (event', outputs') =
      if isOnset event || isOnset event'
        then (Onset, Just $ (fromMaybe [] outputs) ++ (fromMaybe [] outputs'))
        else (Rest, Nothing)

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

runningPatterns :: IO [PatternId]
runningPatterns = withStore patternPoolStore $ pure . Map.keys . Map.filter ((== Running) . getPatternStatus)

idlePatterns :: IO [PatternId]
idlePatterns = withStore patternPoolStore $ pure . Map.keys . Map.filter ((== Idle) . getPatternStatus)

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
stopPattern id =
  updateStore patternPoolStore $
    pure . Map.adjust (\sequencerPattern -> sequencerPattern {getPatternStatus = Idle}) id

activatePattern :: PatternId -> IO PatternPool
activatePattern id =
  updateStore patternPoolStore $
    pure . Map.adjust (\sequencerPattern -> sequencerPattern {getPatternStatus = Running}) id

activateAllPatterns :: IO PatternPool
activateAllPatterns =
  updateStore patternPoolStore $
    pure . Map.map (\sequencerPattern -> sequencerPattern {getPatternStatus = Running})

stopAllPatterns :: IO PatternPool
stopAllPatterns =
  updateStore patternPoolStore $
    pure . Map.map (\sequencerPattern -> sequencerPattern {getPatternStatus = Idle})

-- * Conversion

-- | Convert a rhythmic pattern into an 'OutputPattern'
-- TODO: Add a switch for changing the event matching strategy.
toOutputPattern :: (Rhythmic a) => a -> [[Output]] -> OutputPattern
toOutputPattern pattern outputs =
  let eventPattern = rhythm pattern
   in pairValuesWithOnsets eventPattern outputs

cpsToTimeStamp :: (RealFrac a, Integral b) => a -> b -> Micro
cpsToTimeStamp cps pttrnLen = Micro . round $ 1 / (toRational cps * fromIntegral pttrnLen) * 10 ^ 6
