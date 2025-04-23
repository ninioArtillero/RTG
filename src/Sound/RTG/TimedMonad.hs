{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : TimedMonad
-- Description : "A Timed IO Monad" article transcription/implementation
-- Copyright   : (c) David Janin, 2020
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- The Timed Monad extends the IO monad to include timestamps
-- such that the semantics match the time specification of programs,
-- Most code in this module's comes from the following paper:
--
-- Janin, David. 2020. “A Timed IO Monad.”
-- In Practical Aspects of Declarative Languages,
-- edited by Ekaterina Komendantskaya and Yanhong Annie Liu, 12007:131–47.
-- Lecture Notes in Computer Science. Cham: Springer International Publishing.
-- https://doi.org/10.1007/978-3-030-39197-3_9.
--
-- A NOTE indicates modifications and additions where appropiate.
module Sound.RTG.TimedMonad where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (ap, liftM)
import Data.Kind (Type)
import Euterpea (Music, Pitch, note, pitch, play, qn)
import Sound.RTG.Async
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import Text.Read (readMaybe)
import Prelude hiding (read)

-- | Timestamp type. A /timestamp/ is defined here as the duration elapsed
-- from some fixed but unknown initial time. We expect timestamps,
-- therefore durations as well, to be totally ordered in a time scale.
-- The type variable @d@ represents the duration type, which will be commonly
-- restricted to be a 'Num' type.
-- While the sum of durations makes sense, the sum of two timestamps does not, so
-- they are only equiped with primitives 'duration' and 'shift'.
-- NOTE: derived a 'Show' instance for testing purposes.
newtype Time d = Time d deriving (Show, Eq, Ord)

-- | The relative duration between two timestamps.
duration :: (Num d) => Time d -> Time d -> d
duration (Time d1) (Time d2) = d2 - d1

-- | Shift a timestamp by some duration.
shift :: (Num d) => Time d -> d -> Time d
shift (Time d1) d2 = Time (d1 + d2)

class
  (Ord d, Num d, Monad m, Monad t) =>
  TimedMonad m d t
    | t -> m,
      t -> d
  where
  -- | Current /specified/ timestamp.
  now :: t (Time d)

  -- | Current time drift, i.e. the difference between the /specified/ timestamp
  -- and the /actual/ timestamp (as measured by the underlying runtime).
  -- It is a timing performance measure.
  drift :: t d

  -- | Wait until the current /specified/ timestamp shifted by the given positive duration.
  delay :: d -> t ()

  -- | Turns an action of the underlying monad to an action in the timed monad
  -- by giving it a zero specified duration. It assumes the action to be /instantaneous/.
  -- See documentation of 'dur'.
  lift :: m a -> t a

  -- | Allows moving a timed action back into the underlying untimed monad.
  run :: t a -> m a

realNow :: (TimedMonad m d t) => t (Time d)
realNow = do
  t <- now
  shift t <$> drift

-- | Returns the /speficied/ duration of a timed action (shall always be positive).
-- Used to formulate the following (equational) laws, which specify the semantics
-- of timed primivites.
--
-- Return actions take no time and the duration of two actions composed by the
-- bind operator is the sum of the durations of these actions.
--
-- prop> dur (return a) == return 0
-- prop> dur (m >> m') == do {d <- dur m; d' <- dur m'; return (d+d')}
--
-- Derived from these, we also get that functors preserve specified durations,
-- time measurement acting over types as a /fibration/.
--
-- prop> durm m == dur (fmap f m)
--
-- 'now' and 'drift' should instantaneously (/specified-time-wise/) return and
-- have no side effects, meaning that
--
-- prop> dur now == return 0
-- prop> dur drift == return 0
--
-- For positive durations @d,d1,d2 :: d@, the /specified/ duration of 'delay d'
-- is the parameter 'd', delay is additive with respect to bind and delayes with
-- negative durations have no effects at all.
--
-- prop> dur (delay d) == delay d >> return d
-- prop> delay (d1 + d2) == delay d1 >> delay d2
-- prop> delay (-d) == return ()
--
-- 'lift' is instantaneous in the sense of the following laws.
-- The first two mean 'lift' is a monad transformer from the monad @m@
-- into the monad @t@. However 'TimedMonad' is not a monad transformer, as
-- only specific monads having the timing primitives can be transformed.
--
-- prop> lift . return == return
-- prop> lift (m >>= f) == lift m >>= lift . f
-- prop> dur (lift m) == lift m >> return 0
--
-- 'lift' also preserves the essence of the actions it lifts.
--
-- prop> run . lift == id
--
-- The reverse composition does not for any timed action
-- with non-zero duration.
dur :: (TimedMonad m d t) => t a -> t d
dur m = do
  t0 <- now
  _ <- m
  t1 <- now
  return (duration t1 t0)

-- | This /lift/ gives the action an specified duration equal to the actual
-- duration of its execution minus the existing time drift before it. This
-- can be used with blocking actions like 'getChar' from the IO monad.
timedLift :: (TimedMonad m d t) => m a -> t a
timedLift m = do
  a <- lift m
  d <- drift
  delay d
  return a

-- Default Timed Monad Instances

-- | Monads with enough timing information to be extended to a 'TimedMonad'.
-- Must satisfy __time monotonicity__, meaning time shall flow from the past to the future.
--
-- prop> let (t0,t1) = unsafePerformIO $ do {t0 <- getRealTime; _ <- m; t1 <- getRealTime; return (t0,t1)} in t0 <= t1
--
-- Also wait must be coherent, meaning that 'waitUntil' shall never resume before the expected
-- timestamp is actually passed for real.
--
-- prop> let t1 = unsafePerformIO $ do {_ <- waitUntil t0; t1 <- getRealTime; return t1} in t0 <= t1
class (Ord d, Num d, Monad m) => HasTimer m d where
  getRealTime :: m (Time d)
  waitUntil :: Time d -> m ()
  getDrift :: Time d -> m d
  getDrift t = do
    r <- getRealTime
    return (duration r t)

-- IO example

-- | Type of durations measured in microseconds.
-- NOTE: Intances for 'Integral' and its superclasses 'Enum' and 'Real'
-- are needed for the use of 'toInteger' below. This are not derived in the paper,
-- which is either an error or due to a change in the 'Num' class.
newtype Micro = Micro Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

-- | Depends on @System.Clock@ to give @IO@ the timing information.
getSystemTime :: IO (Time Micro)
getSystemTime = do
  t <- getTime Monotonic
  return . Time . fromInteger $ div (toNanoSecs t) 1000

instance HasTimer IO Micro where
  getRealTime = getSystemTime
  waitUntil (Time d) = do
    Time r <- getSystemTime
    threadDelay . fromInteger . toInteger $ d - r

-- | A /timed action/ used to derive a 'TimedMonad' from a monad @m@ with
-- timing information. It is a variation on a classical state monad transformer.
-- Here, we make the timestamp of the corresponding action is explicit.
newtype TA m d a = TA (Time d -> m (Time d, a))

-- NOTE: Default instances of 'Functor' and 'Applicative' are defined,
-- to be compatible with current Haskell, by moving the definition of 'return'
-- to 'pure' and using some library functions from "Control.Monad".
-- Also the @HasTimer m d@ restriction its ommited as it is not needed.
instance (Monad m) => Monad (TA m d) where
  TA ta >>= f =
    TA
      ( \s -> do
          (s1, a) <- ta s
          let (TA ta1) = f a
          ta1 s1
      )

instance (Monad m) => Functor (TA m d) where
  fmap = liftM

instance (Monad m) => Applicative (TA m d) where
  pure a = TA (\s -> return (s, a))
  (<*>) = ap

-- Derived Timed Monad Instance

-- NOTE: removed unnecesary constraint @Monad m@ already implied by @HasTimer m d@
instance (HasTimer m d) => TimedMonad m d (TA m d) where
  now = TA (\s -> return (s, s))
  drift = TA $ \s -> do
    d <- getDrift s
    return (s, d)
  delay d | d <= 0 = return ()
  delay d | d > 0 = TA $ \s -> do
    dr <- getDrift s
    waitUntil (shift s (d - dr))
    return (shift s d, ())
  lift m = TA $ \s -> do
    a <- m
    return (s, a)
  run (TA ta) = do
    t <- getRealTime
    (_, a) <- ta t
    return a

-- | The default time extension of the IO monad, having the instance
-- @TimedMonad IO Micro TIO@ deriving from the instance of @HasTimer IO Micro@.
type TIO = TA IO Micro

-- Symbolic Timed Extension of a Monad

-- | This class models the relationship between an /inner/ duration
-- (_e.g._ the physical time) and
-- an /outer/ duration (_e.g._ the musical time) using a tempo/speed type
-- (supposed to be piecewise constant).
-- Any instance must satisfy the following equations (up to rounding errors dependent
-- on the actual numerical types):
--
-- prop> backStep s (step s i) == i
-- prop> step d (backStep d o) == o
class (Num i, Num o, Num s) => ScaleChange i o s | s -> i, s -> o where
  initialSpeed :: s
  step :: s -> i -> o
  backStep :: s -> o -> i

newtype Beat = Beat Double deriving (Eq, Ord, Show, Num)

newtype BPM = BPM Double deriving (Eq, Ord, Show, Num)

instance ScaleChange Micro Beat BPM where
  initialSpeed = BPM 60
  step (BPM t) (Micro d) = Beat $ t * fromIntegral d / ratio
    where
      ratio = 60 * 10 ^ 6
  backStep (BPM t) (Beat d) = Micro . fromIntegral . floor $ (d * ratio) / t
    where
      ratio = 60 * 10 ^ 6

-- | Symbolic timed states. This type bundles the 'ScaleChange' types.
data ST i o s = ST {innerTime :: Time i, outerTime :: Time o, speed :: s}

-- | Symbolic timed actions. A generalization of timed actions 'TA'.
-- NOTE: @newtype@ is used instead of @data@ for optimization.
newtype STA m i o s a = STA (ST i o s -> m (ST i o s, a))

-- NOTE: The following instances are assumed by the article.
-- They are consequence of STA being, again, a variation of a state monad
-- transformer.

instance (Monad m) => Functor (STA m i o s) where
  fmap = liftM

instance (Monad m) => Applicative (STA m i o s) where
  pure a = STA (\st -> return (st, a))
  (<*>) = ap

instance (Monad m) => Monad (STA m i o s) where
  STA sta >>= f =
    STA
      ( \st -> do
          (st', a) <- sta st
          let STA sta' = f a
          (st'', b) <- sta' st'
          return (st'', b)
      )

-- | Standard timed extension of a Monad with timing information using symbolic
-- time states.
-- This derived 'TimedMonad' instance uses the 'outerTime' as the explicit timestamp
-- or /specified/ time.
-- NOTE: This is just assumed by the article. It is analogous to the 'TA' instance.
instance (Ord o, ScaleChange i o s, HasTimer m i) => TimedMonad m o (STA m i o s) where
  -- \| The 'outerTIme' of the 'ScaleChange' is used as the explicit timestamp.
  now = STA (\st -> return (st, outerTime st))

  -- \| The time drift is calculated using the 'innerTime' of the 'ScaleChange',
  -- which corresponds to the timing information of 'HasTimer' instance.
  drift = STA $ \st -> do
    d <- getDrift $ innerTime st
    return (st, step (speed st) d)

  -- \| Updates both 'innerTime' and 'outerTime' by the given time.
  delay d | d <= 0 = return ()
  delay d | d > 0 = STA $ \(ST ti to s) -> do
    dr <- getDrift ti
    let ti' = shift ti (backStep s d - dr)
        to' = shift to (d - step s dr)
    waitUntil ti'
    return (ST ti' to' s, ())

  -- \| A 'pure'-like embedding.
  lift m = STA $ \st -> do
    a <- m
    return (st, a)

  --  NOTE: Explicit type annotations are needed to unify the types from the
  -- 'ScaleChange' and 'HasTimer' constraints.
  run (STA sta) = do
    Time (inner :: i) <- getRealTime
    let outer :: o
        outer = step (initialSpeed :: s) inner
    (_, a) <- sta $ ST (Time inner) (Time outer) initialSpeed
    return a

-- | Musically timed extension of the 'IO' monad.
type MusicIO = STA IO Micro Beat BPM

-- | Change the underlying tempo (corresponding to the 'speed' of the 'ScaleChange').
setTempo :: BPM -> MusicIO ()
setTempo t
  | t <= 0 = error "setTempo: forbidden negative tempo"
  | otherwise = STA $ \st -> let ST ti to _ = st in return (ST ti to t, ())

-- Example
-- TODO: Implement example as is. In particular, startNote
-- and endNote will need finer midi message control.

playInIO = run $ setTempo 90 >> playMusic f 30

-- playInIO :: IO ()
-- playInIO = run playMusic

type Note = Pitch

-- | A function that tells with note is to be played at any instant n.
f :: Int -> Music Pitch
f n = note qn (pitch n)

-- | An up-down chromatic scale starting at C4.
g :: Int -> Note
g n =
  if (n `div` 12) `mod` 2 == 0
    then pitch $ (+ 60) $ n `mod` 12
    else pitch $ (+ 72) $ negate $ n `mod` 12

-- f :: Int -> Note
-- f n = pitch n

-- | Play each note for one second.
playMusic :: (Int -> Music Pitch) -> Int -> MusicIO ()
playMusic f n =
  do
    playNote (f n) 1
    delay 1
    playMusic f (n + 1)

playNote :: Music Pitch -> Beat -> MusicIO ()
playNote n d = lift (forkIO $ play n) >> delay d

-- playNote :: Note -> Beat -> MusicIO ()
-- playNote n d = startNote n >> delay d >> stopNote n

-- Timed Promises

-- | Monad references implementing the concept of promises for a monad @m@.
-- Defines an associated type (family) 'Ref' for @m@ of kind @* -> *@.
-- NOTE: Following a compiler warning, the deprecated @StarIsType@ extension is dropped and
-- 'Data.Kind.Type' is used instead for the type family's signature.
class (Monad m) => MonadRef m where
  type Ref m :: Type -> Type
  fork :: m a -> m (Ref m a)
  read :: Ref m a -> m a
  tryRead :: Ref m a -> m (Maybe a)
  parRead :: Ref m a -> Ref m b -> m (Either a b)

-- | NOTE: This instance is suggested by the companion article:
-- Janin, D. 2020 "An equational model of asynchronous concurrent programming".
-- It turns to be a simplification of the @async@ library for basic concurrency.
instance MonadRef IO where
  type Ref IO = Async
  fork = async
  read = wait
  tryRead = tryWait
  parRead = waitEither

-- | Timed monad references. The constraint 'MonadRef m' is needed when
-- manipulating values of this type due to the use of the associated
-- type (synonym) family 'Ref' in its second field.
data TRef m d a = TRef (Time d) (Ref m (Time d, a))

-- | Equipping a timed extension of a monad by monad references,
-- given the underlying monad itself has references.
instance (MonadRef m, HasTimer m d) => MonadRef (TA m d) where
  type Ref (TA m d) = TRef m d
  fork (TA m) = TA $ \s -> do r <- fork (m s); return (s, TRef s r)
  read (TRef _ r) = TA $ \s -> do (t, a) <- read r; return (max s t, a)
  tryRead (TRef _ r) = TA $ \s ->
    do
      c <- tryRead r
      case c of
        Nothing -> return (s, Nothing)
        Just (t, a) -> return (max s t, Just a)
  parRead (TRef _ r1) (TRef _ r2) = TA $ \s ->
    do
      c <- parRead r1 r2
      case c of
        Left (t, a) -> return (max s t, Left a)
        Right (t, b) -> return (max s t, Right b)

-- | Returns the specified duration of a referenced action when finished.
durRef :: (MonadRef m, HasTimer m d) => TRef m d a -> TA m d d
durRef (TRef t0 r) =
  TA
    ( \s -> do
        (t, _) <- read r
        return (max s t, duration t t0)
    )

-- | Replay the referenced action from start, with the same duration
-- but no side effect.
replayRef :: (MonadRef m, HasTimer m d) => TRef m d a -> TA m d a
replayRef r = do
  t1 <- now
  d <- durRef r
  a <- read r
  t2 <- now
  delay (d - duration t2 t1)
  return a

-- | Replays a refenced action but expanding (or shriking) its duration
-- by appliying its function parameter.
expandRef :: (MonadRef m, HasTimer m d) => (d -> d) -> TRef m d a -> TA m d a
expandRef f r = do
  t1 <- now
  d <- durRef r
  a <- read r
  t2 <- now
  let d1 = f d - duration t2 t1
  delay d1
  return a
