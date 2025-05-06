-- |
-- Module      : Sequencer
-- Description : A sequencer using the Timed IO Monad.
-- Copyright   : (c) Xavier Góngora, 2025
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- A sequencer designed to run on @ghci@.
--
-- It plays an 'OutputPattern' derived from the 'PatternBundle' build during
-- the interactive session.
--
-- The implementation depends on the `foreign-store` package, which is explicit
-- about the fact that a 'Store' is not thread safe. Care is taken
-- for state updates to be done only within the main thread.
module Sound.RTG.Sequencer
  ( -- * Sequencer API
    p,
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

    -- * Sequencer Entry-Point
    inSequencer,

    -- * Sequencer State Operations
    getSequencerState,
    updateSequencerCPS,
    updateSequencerCounter,
    updateSequencerMode,
    updateSequencerEventDuration,
    updateSequencerOutputPattern,

    -- * Pattern Bundle State Operations
    getPatternBundle,
    queriePatterns,
    runningPatterns,
    idlePatterns,
    resetPatternBundle,
    addPatternToBundle,
    removePattern,
    activatePattern,
    stopPattern,
    activateAllPatterns,
    stopAllPatterns,
  )
where

import Control.Monad (forever, mapM_, when)
import Euterpea (note, play)
import Foreign.Store
  ( Store (..),
    lookupStore,
    readStore,
    storeAction,
    withStore,
    writeStore,
  )
import Sound.RTG.Event (pairValuesWithOnsets)
import Sound.RTG.OscMessages (sendSuperDirtSample)
import Sound.RTG.PatternBundle
import Sound.RTG.RhythmicPattern (Rhythmic, rhythm)
import Sound.RTG.TimedMonad
  ( Micro (..),
    MonadRef (..),
    TIO,
    TRef (TRef),
    TimedMonad (..),
  )

-- * Data Types

-- | A 'SequencerState' its a 'Maybe (Ref TIO ())' and a 'SequencerMode'.
-- A 'Ref TIO ()' is a Timed IO Monad ('TIO') reference to the sequencer running thread.
-- 'Nothing' in this field means the sequencer is not running.
data SequencerState = SequencerState
  { getMode :: !SequencerMode,
    getThread :: !(Maybe (Ref TIO ())),
    getCPS :: !CPS,
    getCounter :: !Counter,
    getOutput :: !OutputPattern,
    getEventDuration :: !Micro
  }

-- The sequencer playing mode.
data SequencerMode = Solo !Int | Global deriving (Show)

-- | Cycles Per Second.
type CPS = Rational

-- | Beats Per Cycle.
type BPC = Int

-- | Beats Per Minute.
type BPM = Int

-- | Cycle Counter.
type Counter = Int

instance Show SequencerState where
  show (SequencerState mode thread cps counter output eventDurMicro) =
    let playStatus = case thread of
          Nothing -> "IDLE"
          Just _ -> "RUNNING"
        outputLength = length $ output
        floatCPS :: Float
        floatCPS = fromRational cps
        eventDurSec :: Rational
        eventDurSec = microToSec eventDurMicro
     in unlines $
          [ "Sequencer is " ++ playStatus ++ ".",
            "Set to " ++ show floatCPS ++ " cycles per second.",
            "In " ++ show mode ++ " mode.",
            "Cycle counter: " ++ show counter ++ ".",
            "Pattern length: " ++ show outputLength,
            "Event duration: " ++ show eventDurSec
          ]

-- * Sequencer Interface

-- | Run a pattern in the sequencer.
-- p :: Rhythmic a => [SampleName] -> PatternID -> a -> IO PatternBundle
p patternId samples = addPattern Running patternId (map (\s -> [Osc s]) samples)

-- | Entry point for a pattern execution. Adds the pattern to the bundle.
-- TODO: Add argument to modify event matching strategy for output values.
addPattern :: (Rhythmic a) => PatternStatus -> PatternId -> [[Output]] -> a -> IO PatternBundle
addPattern patternStatus id outputs pttrn = inSequencer True $ do
  let newSequencerPattern = toOutputPattern pttrn outputs
  addPatternToBundle id $
    SequencerPattern
      { getOutputPattern = newSequencerPattern,
        getPatternStatus = patternStatus
      }

status :: IO ()
status = inSequencer False $ do
  sequencerStatus
  patternsInBundle <- queriePatterns
  putStrLn $ "Patterns in bundle: " ++ show patternsInBundle
  activePatterns <- runningPatterns
  putStrLn $ "Active patterns: " ++ show patternsInBundle

querie :: IO [PatternId]
querie = inSequencer False $ queriePatterns

active :: IO [PatternId]
active = inSequencer False $ runningPatterns

idle :: IO [PatternId]
idle = inSequencer False $ idlePatterns

setcps :: CPS -> IO ()
setcps cps = inSequencer True $ updateSequencerCPS cps >> pure ()

setbpm :: BPC -> BPM -> IO ()
setbpm bpc bpm =
  let newcps = (fromIntegral bpm / 60) / fromIntegral bpc
   in setcps newcps

solo :: PatternId -> IO ()
solo patternId = inSequencer True $ updateSequencerMode (Solo patternId) >> pure ()

unsolo :: IO ()
unsolo = inSequencer True $ updateSequencerMode Global >> pure ()

stop :: PatternId -> IO PatternBundle
stop id = inSequencer True $ stopPattern id

-- | Set all patterns to 'Idle'.
stopAll :: IO PatternBundle
stopAll = inSequencer True $ stopAllPatterns

start :: PatternId -> IO PatternBundle
start id = inSequencer True $ activatePattern id

-- | Set all pattern to 'Running'.
startAll :: IO PatternBundle
startAll = inSequencer True $ activateAllPatterns

-- | Removes a pattern from both the bundle.
kill :: PatternId -> IO PatternBundle
kill id = inSequencer True $ removePattern id

-- | Clear the bundle.
clear :: IO ()
clear = inSequencer True $ do resetPatternBundle

-- | Reset the sequencer with the current state.
reset :: IO ()
reset = inSequencer True $ pure ()

-- | Kill playing thread.
hush :: IO ()
hush = inSequencer False $ run go
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

-- | Wrapper for all sequencer operations. Executes the given action and returns
-- its value, running the sequencer with the action's side effects in between.
-- Its boolean argument means whether the action updates the state, to avoid
-- querying operations from re-triggering the sequencer, and print the current
-- state in such cases.
--
-- NOTE: Because the sequencer must be ran after the updating action,
-- and the return value of this action is printed afterwards (by ghci automatic 'show'),
-- we coherse the return values of all sequencer actions
-- that return an updated sequencer state to '()' at the API functions.
inSequencer :: Bool -> IO a -> IO a
inSequencer update action = do
  storeInit
  returnValue <- action
  when update $ do
    runSequencer
    sequencerState <- getSequencerState
    print sequencerState
  pure returnValue

-- | Initializes the 'patternBundleStore' to an empty 'PatternBundle',
-- and the 'sequencerStateStore' to global mode, none playback thread and
-- a default cycles-per-second value.
storeInit :: IO ()
storeInit = do
  [maybeBundleStore, maybeThreadStore] <- mapM lookupStore [0, 1]
  case (maybeBundleStore, maybeThreadStore) of
    (Nothing, Nothing) -> do
      resetPatternBundle
      writeStore sequencerStateStore $
        SequencerState Global Nothing defaultCPS 0 [] 0
    (Nothing, _) -> resetPatternBundle
    (_, Nothing) ->
      writeStore sequencerStateStore $
        SequencerState Global Nothing defaultCPS 0 [] 0
    (_, _) -> pure ()

-- | Querie the environment's shared state and (re-)trigger execution
-- according to the 'SequencerMode'.
runSequencer :: IO ()
runSequencer = do
  gp <- globalPattern
  sequencerState <- getSequencerState
  let cps = getCPS sequencerState
      mode = getMode sequencerState
      len = length gp
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

defaultCPS :: CPS
defaultCPS = 0.5

-- | Set the sequencer to a new thread.
updateSequencerThread :: Maybe (Ref TIO ()) -> IO SequencerState
updateSequencerThread newThread = updateStore sequencerStateStore $
  \sequencerState -> pure $ sequencerState {getThread = newThread}

-- | Set the sequencer playing mode.
updateSequencerMode :: SequencerMode -> IO SequencerState
updateSequencerMode newmode = updateStore sequencerStateStore $
  \sequencerState -> pure $ sequencerState {getMode = newmode}

-- | Sets the sequencer cycles per second.
updateSequencerCPS :: CPS -> IO SequencerState
updateSequencerCPS newcps = updateStore sequencerStateStore $
  \sequencerState -> pure $ sequencerState {getCPS = newcps}

-- | Updates the sequencer cycle count by 1.
updateSequencerCounter :: IO SequencerState
updateSequencerCounter = updateStore sequencerStateStore $
  \sequencerState -> pure $ sequencerState {getCounter = getCounter sequencerState + 1}

-- | Set the sequencer output pattern.
updateSequencerOutputPattern :: OutputPattern -> IO SequencerState
updateSequencerOutputPattern outputPattern = updateStore sequencerStateStore $
  \sequencerState -> pure $ sequencerState {getOutput = outputPattern}

-- | Set the sequencer event duration.
updateSequencerEventDuration :: Micro -> IO SequencerState
updateSequencerEventDuration eventDur = updateStore sequencerStateStore $
  \sequencerState -> pure $ sequencerState {getEventDuration = eventDur}

-- | Returns the whole sequencer current state.
getSequencerState :: IO SequencerState
getSequencerState = readStore sequencerStateStore

-- | Returns the thread where the sequencer is running at.
getSequencerThread :: IO (Maybe (Ref TIO ()))
getSequencerThread = do
  sequencerState <- readStore sequencerStateStore
  pure $ getThread sequencerState

-- | Returns the sequencer playing mode.
getSequencerMode :: IO SequencerMode
getSequencerMode = do
  sequencerState <- readStore sequencerStateStore
  pure $ getMode sequencerState

-- | Returns the sequencer cycles per second.
getSequencerCPS :: IO CPS
getSequencerCPS = do
  sequencerState <- readStore sequencerStateStore
  pure $ getCPS sequencerState

-- | Returns the sequencer output pattern.
getSequencerOutputPattern :: IO OutputPattern
getSequencerOutputPattern = do
  sequencerState <- readStore sequencerStateStore
  pure $ getOutput sequencerState

-- | Returns sequencer cycle counter.
getSequencerCounter :: IO Counter
getSequencerCounter = do
  sequencerState <- readStore sequencerStateStore
  pure $ getCounter sequencerState

-- | Returns the sequencer event duration. This value is derived from the
-- the output pattern length and the cycles per second the sequencer runs at.
getSequencerEventDuration :: IO Micro
getSequencerEventDuration = do
  sequencerState <- readStore sequencerStateStore
  pure $ getEventDuration sequencerState

-- | Prints the sequencer state using its custom show function.
sequencerStatus :: IO ()
sequencerStatus = do
  withStore sequencerStateStore $ print

-- * Output Fuctionality

-- $outputfunctionality
-- By lifting output actions into the Timed IO Monad (TIO) we can forget about
-- managing computation time drifts explicitly and get (approximately)
-- correct timing.

-- | Sequence an 'OutputPattern' in the Timed IO Monad 'TIO'
-- by updating the sequencer state.
-- The playback loop runs on a forked thread.
-- This function is called at 'runSequencer' to /play/ an 'OutputPattern'.
-- The 'Micro' parameter corresponds to the duration of each event
-- and it is derived from the global cps value and the pattern length.
sequenceOutputPattern :: Micro -> OutputPattern -> TIO ()
sequenceOutputPattern eventDur op = do
  maybeThread <- lift $ getSequencerThread
  if eventDur == 0 || null op
    then case maybeThread of
      Nothing -> pure ()
      Just thread -> do
        -- freeze thread
        _ <- lift $ updateSequencerOutputPattern []
        _ <- lift $ updateSequencerThread Nothing
        _ <- lift $ updateSequencerEventDuration 0
        pure ()
    else case maybeThread of
      Nothing -> do
        _ <- lift $ updateSequencerOutputPattern op
        _ <- lift $ updateSequencerEventDuration eventDur
        thread <- fork . forever $ runOutputPattern
        _ <- lift $ updateSequencerThread (Just thread)
        pure ()
      Just runningThread -> do
        _ <- lift $ updateSequencerOutputPattern op
        _ <- lift $ updateSequencerEventDuration eventDur
        pure ()

-- | Runs the sequencer's state output pattern in the Timed IO Monad.
runOutputPattern :: TIO ()
runOutputPattern = do
  outputPattern <- lift getSequencerOutputPattern
  eventDur <- lift getSequencerEventDuration
  patternOutput eventDur outputPattern

-- | A whole pattern output action in the Timed IO Monad.
patternOutput :: Micro -> [(a, Maybe [Output])] -> TIO ()
patternOutput eventDur outputPattern = mapM_ (eventOutput eventDur . snd) outputPattern

-- | Generates an event playback by the given duration in the Timed IO Monad.
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

-- * PatternBundle Proyection

-- | The global pattern obtained from merging all running patterns in the bundle.
-- It is the means to play all the patterns simultaneously.
-- The /merging/ procedure is implemented by the bundle /projection/.
globalPattern :: IO OutputPattern
globalPattern = withStore patternBundleStore (pure . proyection)

-- | Extracts a single patern from the bundle.
soloPattern :: PatternId -> IO (Maybe SequencerPattern)
soloPattern id = withStore patternBundleStore (pure . fiber id)

-- * PatternBundle State

-- $patternbundlestate
-- The 'PatternBundle' is stored in the 'patternBundleStore', so here whe define custom
-- 'Store' operations to manipulate it.
-- 'Store's are used for the sequencer state to persist @ghci@ reloads.

-- | The 'Store' index for the'PatternBundle'.
patternBundleStore :: Store PatternBundle
patternBundleStore = Store 0

-- | Returns the stored pattern bundle.
getPatternBundle :: IO PatternBundle
getPatternBundle = readStore patternBundleStore

-- | Returns the keys of al patterns in the bundle.
queriePatterns :: IO [PatternId]
queriePatterns = withStore patternBundleStore $ pure . bundleKeys

-- | Returns all 'Running' patterns in the pattern bundle.
runningPatterns :: IO [PatternId]
runningPatterns = withStore patternBundleStore $ pure . runningPatternKeys

-- | Returns all 'Idle' patterns in the pattern bundle.
idlePatterns :: IO [PatternId]
idlePatterns = withStore patternBundleStore $ pure . idlePatternKeys

-- | Store and return the result of an update operation on the stored value.
updateStore :: Store a -> (a -> IO a) -> IO a
updateStore store update = storeAction store . withStore store $ update

-- | Empties the pattern bundle.
resetPatternBundle :: IO ()
resetPatternBundle = writeStore patternBundleStore $ emptyBundle

-- | Add or update a pattern in the pattern bundle.
addPatternToBundle :: PatternId -> SequencerPattern -> IO PatternBundle
addPatternToBundle id pattern =
  updateStore patternBundleStore $ pure . insert id pattern

-- | Remove a pattern from the bundle.
removePattern :: PatternId -> IO PatternBundle
removePattern id = updateStore patternBundleStore $ pure . remove id

-- | Set a pattern in the bundle to 'Idle' state.
stopPattern :: PatternId -> IO PatternBundle
stopPattern id =
  updateStore patternBundleStore $
    pure . disable id

-- | Set a pattern in the bundle to 'Running' state.
activatePattern :: PatternId -> IO PatternBundle
activatePattern id =
  updateStore patternBundleStore $
    pure . enable id

-- | Set all patterns in the bundle to 'Running' state.
activateAllPatterns :: IO PatternBundle
activateAllPatterns =
  updateStore patternBundleStore $
    pure . enableAll

-- | Set all patterns in the bundle to 'Idle' state.
stopAllPatterns :: IO PatternBundle
stopAllPatterns =
  updateStore patternBundleStore $
    pure . disableAll

-- * Conversion

-- | Convert a rhythmic pattern into an 'OutputPattern'
-- TODO: Add a switch for changing the event matching strategy.
toOutputPattern :: (Rhythmic a) => a -> [[Output]] -> OutputPattern
toOutputPattern pattern outputs =
  let eventPattern = rhythm pattern
   in pairValuesWithOnsets eventPattern outputs

cpsToTimeStamp :: (RealFrac a, Integral b) => a -> b -> Micro
cpsToTimeStamp cps pttrnLen = Micro . round $ 1 / (toRational cps * fromIntegral pttrnLen) * 10 ^ 6

microToSec :: (Fractional a) => Micro -> a
microToSec (Micro n) = fromIntegral n / 10 ^ 6
