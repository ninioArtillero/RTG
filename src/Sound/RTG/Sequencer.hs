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
  ( -- * Sequencer Operations

    -- ** Play
    p,
    a,
    start,
    startAll,
    stop,
    stopAll,
    solo,
    unsolo,
    hush,

    -- ** Configure
    reset,
    resume,
    setcps,
    setbpm,
    kill,
    clear,

    -- ** Querie
    status,
    querie,
    active,
    idle,

    -- * Transformations
    actionT,
    actionB,
    handFanT,
    handFanB,
  )
where

import Control.Monad (forever, mapM_, when)
import Data.Char (toUpper)
import Euterpea (note, play)
import Foreign.Store
  ( Store (..),
    lookupStore,
    readStore,
    storeAction,
    withStore,
    writeStore,
  )
import qualified Sound.RTG.BundleTransformations as Bundle
import Sound.RTG.Event (pairValuesWithOnsets)
import Sound.RTG.List (contract)
import Sound.RTG.OscMessages (sendSuperDirtSample)
import Sound.RTG.PatternBundle
import Sound.RTG.RhythmicPattern
  ( Rhythm (..),
    Rhythmic,
    rhythm,
    rhythmBalance,
    rhythmEvenness,
  )
import Sound.RTG.TimedMonad
  ( Micro (..),
    MonadRef (..),
    TIO,
    TRef (TRef),
    TimedMonad (..),
  )

-- TODO: Change to lambda-case or use 'maybe' to ease reading.

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
data SequencerMode = Solo !Int | Global | Transform deriving (Show)

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
        outputLength = length output
        floatCPS :: Float
        floatCPS = fromRational cps
        eventDurMiliSec :: Float
        eventDurMiliSec = microToSec eventDurMicro * 10 ^ 3
     in unlines $
          [ "Sequencer is " ++ playStatus ++ ",",
            "in " ++ (map toUpper . show) mode ++ " mode.",
            "Cycle duration: " ++ show (1 / floatCPS) ++ " s",
            "Cycle counter: " ++ show counter ++ "\n",
            "Output length: " ++ show outputLength,
            "Output pattern: " ++ (show $ rhythm output),
            "Event duration: " ++ show eventDurMiliSec ++ " ms",
            "Evenness = " ++ show (rhythmEvenness output),
            "Balance = " ++ show (rhythmBalance output)
          ]

-- * Sequencer Interface

-- ** Play

-- | Run a pattern in the sequencer, playing the given samples simultaneously on it.
p :: (Rhythmic a) => PatternId -> [SampleName] -> a -> IO PatternBundle
p patternId samples = addPattern Running patternId [map (\s -> Osc s) samples]

-- | Run a pattern in the sequencer by matching samples sequentially.
-- This is an alternative event matching strategy, originally introduced as a bug
-- for the intended behavior of 'p' (which is now documented).
a :: (Rhythmic a) => PatternId -> [SampleName] -> a -> IO PatternBundle
a patternId samples = addPattern Running patternId (map (\s -> [Osc s]) samples)

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

-- ** Querie

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

-- ** Playback Functions

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

-- | Reset the sequencer with the current state.
reset :: IO ()
reset = inSequencer True $ pure ()

-- ** Transform

-- | Resume the global pattern.
resume :: IO ()
resume = inSequencer True $ updateSequencerMode Global >> pure ()

-- | Implements a fixed bug from 'eventOutput', which produced an
-- interesting expansion of patterns when an event in the sequencer output pattern
-- had a /non-sigleton/ output list. It was caused by the following definition,
-- which added a duration between each /simultaneous/ output value.
--
-- @eventOutput (Just outputs) = mapM_ (\x -> instantOut dur x >> delay dur) outputs@
--
-- The result was that each event triggered its output values in succession.
-- /This functions does just that/.
-- As a consecuence, the full cycle duration is expanded by
-- \[ d \sum_{i=1}^{n}  tail(e_i) \]
-- where \(d\) is the event duration, \(n\) is the number of events with more than
-- one output, \(e_i\) ranges over such events and \(tail(e_i)\) is the tail length
-- of the event's output list.
handFanT :: IO ()
handFanT = inSequencer True $ do
  updateSequencerMode Transform
  transformSequencerOutput Bundle.handFan
  pure ()

handFanB :: IO PatternBundle
handFanB = inSequencer True $ do
  updateSequencerMode Global
  transformSequencerBundle $ Bundle.liftB Bundle.handFan

actionB :: (Rhythmic a) => a -> IO PatternBundle
actionB rhythm = inSequencer True $ do
  updateSequencerMode Global
  transformSequencerBundle $ Bundle.fibreProduct rhythm

actionT :: (Rhythmic a) => a -> IO ()
actionT rhythm = inSequencer True $ do
  updateSequencerMode Transform
  transformSequencerOutput $ Bundle.outputProduct rhythm
  pure ()

-- * Sequencer Operation

-- | Wrapper for all sequencer operations. Executes the given action and returns
-- its value, running the sequencer with the action's side effects in between.
-- Its boolean argument signals whether the action updates the state, to avoid
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
      writeStore sequencerStateStore defaultSequencerState
    (Nothing, _) -> resetPatternBundle
    (_, Nothing) -> writeStore sequencerStateStore defaultSequencerState
    (_, _) -> pure ()

-- | Querie the sequencer cps and the global pattern to (re-)trigger execution
-- according to the 'SequencerMode'. Here the sequencer event duration is calculated.
runSequencer :: IO ()
runSequencer = do
  gp <- globalPattern
  st <- getSequencerState
  let cps = getCPS st
      mode = getMode st
      globalLength = length gp
      globalEventDur = if globalLength /= 0 then cpsToTimeStamp cps globalLength else 0
      -- Time and output transformations use the current states.
      currentOutput = getOutput st
      currentEventDur = getEventDuration st
  case mode of
    Global -> run $ sequenceOutputPattern globalEventDur gp
    Solo patternId -> do
      maybeOutputPattern <- soloPattern patternId
      case maybeOutputPattern of
        Nothing -> do
          putStrLn $
            unlines
              [ "Pattern missing!",
                "Use 'queriePatterns' to show available patterns to 'solo',",
                "'unsolo' the sequencer or play 'd" ++ show patternId ++ "'."
              ]
          -- Run a silent pattern if the soloed pattern is not found.
          run $ sequenceOutputPattern 0 mempty
        Just op -> do
          let soloEventDur =
                if length op /= 0 then cpsToTimeStamp cps (length op) else 0
          -- Avoid soloing an idle pattern by default.
          activatePattern patternId
          run $ sequenceOutputPattern soloEventDur op
    Transform -> run $ sequenceOutputPattern currentEventDur currentOutput

-- * Sequencer State

defaultSequencerState :: SequencerState
defaultSequencerState =
  SequencerState
    { getMode = Global,
      getThread = Nothing,
      getCPS = 0.5,
      getCounter = 0,
      getOutput = mempty,
      getEventDuration = 0
    }

-- | Contains the shared global 'SequencerState'.
sequencerStateStore :: Store SequencerState
-- sequencerStateStore = unsafePerformIO $ newStore $ Nothing
sequencerStateStore = Store 1

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
-- by updating the sequencer output pattern, running thread and event duration.
-- Loops the playback forever on a forked thread.
-- This function is called at 'runSequencer' to /play/ an 'OutputPattern', which
-- calculates the 'Micro' parameter, corresponding to the duration of each event,
-- from the sequencer cps value and the global pattern length.
sequenceOutputPattern :: Micro -> OutputPattern -> TIO ()
sequenceOutputPattern eventDur op = do
  maybeThread <- lift $ getSequencerThread
  -- Manage edge case: null duration or pattern.
  if eventDur == 0 || null op
    then case maybeThread of
      Nothing -> pure ()
      Just thread -> do
        -- freeze thread
        _ <- lift $ updateSequencerOutputPattern mempty
        _ <- lift $ updateSequencerThread Nothing
        _ <- lift $ updateSequencerEventDuration 0
        pure ()
    -- Spark a looping thread and/or just update the values it references.
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
  patternOutput eventDur (getRhythm outputPattern)
  -- NOTE: Might return the updated couter for other use.
  lift updateSequencerCounter
  pure ()

-- | A whole pattern output action in the Timed IO Monad.
patternOutput :: (Eq a, Monoid a) => Micro -> [(a, Maybe [Output])] -> TIO ()
patternOutput eventDur outputPattern =
  mapM_
    ( \((event, outputs), n) ->
        eventOutput (eventDur * fromIntegral n) outputs
    )
    -- We contract the outputPattern to avoid redundant calls to eventOutput.
    -- NOTE: If rests with non-empty outputs /leak/, they will be treated as onsets
    -- and result in a performance penalty.
    -- Currently specified behavior aims for this not to happen, but it is not (yet)
    -- formally enforced. This must be considered if "ghost output" features are
    -- implemented later on.
    $ contract outputPattern

-- | Generates an event playback by the given duration in the Timed IO Monad.
eventOutput :: Micro -> Maybe [Output] -> TIO ()
eventOutput dur Nothing = delay dur
eventOutput dur (Just outputs) = mapM_ (instantOut dur) outputs >> delay dur

-- | Output of single values as /instantaneous/ actions in the Timed IO Monad ('TIO').
-- TODO: Note output is messing up time. For the mean time I'll focus on Osc output.
instantOut :: Micro -> Output -> TIO ()
instantOut _ (Osc sample) = lift $ sendSuperDirtSample sample
instantOut dur (Note pitch) =
  let Micro m = dur
      microsecs = fromIntegral m
      secs = microsecs / 10 ^ 6
   in lift $ do
        play $ note secs pitch -- too slow to catch up?

-- * PatternBundle Projection

-- | The global pattern obtained from merging all running patterns in the bundle.
-- It is the means to play all the patterns simultaneously.
-- The /merging/ procedure is implemented by the bundle /projection/.
globalPattern :: IO OutputPattern
globalPattern = withStore patternBundleStore (pure . projection)

-- | Extracts a single patern from the bundle and aligns it with the global pattern
-- length
soloPattern :: PatternId -> IO (Maybe OutputPattern)
soloPattern id = withStore patternBundleStore $ \patternBundle -> do
  pure $ fiber id patternBundle

{- NOTE: Alignment was needed in soloPattern because the global event duration was
-- was passed to it by runSequencer. To optimize this now we calculate
-- the soloed pattern event duration explicitly to call sequenceOutputPattern with it.
  gp <- globalPattern
  let maybeSequencerPattern = fiber id patternBundle
  pure $
    fmap
      ( \sequencerPattern ->
          let op = getOutputPattern sequencerPattern
           in alignPattern (length gp) op
      )
      maybeSequencerPattern
-}

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

-- | TODO: Consider specializing this function to CPS and use newtypes to enforce
-- correct values. Alternative: create a LH spec.
cpsToTimeStamp :: (RealFrac a, Integral b) => a -> b -> Micro
cpsToTimeStamp cps pttrnLen
  | cps <= 0 = error "Sequencer.cpsToTimeStamp: non-positive cps value. "
  | pttrnLen <= 0 = error "Sequencer.cpsToTimeStamp: non-positive pattern length. "
  | otherwise = Micro . round $ 1 / (toRational cps * fromIntegral pttrnLen) * 10 ^ 6

microToSec :: (Fractional a) => Micro -> a
microToSec (Micro n) = fromIntegral n / 10 ^ 6

-- * Sequencer Transformations

transformSequencerBundle :: (PatternBundle -> PatternBundle) -> IO PatternBundle
transformSequencerBundle f = updateStore patternBundleStore $ pure . f

transformSequencerOutput :: (OutputPattern -> OutputPattern) -> IO SequencerState
transformSequencerOutput f = updateStore sequencerStateStore $ \st -> do
  let newOutput = f (getOutput st)
  pure $ st {getOutput = newOutput}

-- * Sequencer Time Transformations

adjustCPSToEventDur :: Micro -> [a] -> IO SequencerState
adjustCPSToEventDur eventDur [] = getSequencerState
adjustCPSToEventDur eventDur outputPattern =
  let cycleDur = microToSec eventDur * fromIntegral (length outputPattern)
   in updateSequencerCPS $ 1 / cycleDur
