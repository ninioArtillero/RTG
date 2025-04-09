{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : TimedMonad
Description : "A Timed IO Monad" article transcription/implementation
Copyright   : (c) David Janin, 2020
License     : GPL-3
Maintainer  : ixbalanque@protonmail.ch
Stability   : experimental

The Timed Monad extends the IO monad to include timestamps
such that the semantics match the time specification of programs,
Most code in this module's comes from the following paper:

Janin, David. 2020. “A Timed IO Monad.”
In Practical Aspects of Declarative Languages,
edited by Ekaterina Komendantskaya and Yanhong Annie Liu, 12007:131–47.
Lecture Notes in Computer Science. Cham: Springer International Publishing.
https://doi.org/10.1007/978-3-030-39197-3_9.

A NOTE indicates modifications and additions where appropiate.
-}


module Sound.RTG.TimedMonad where

import System.Clock
import Control.Concurrent
import Control.Monad (liftM,ap)
import Euterpea
import Data.Kind
import Prelude hiding (read)

-- | Timestamp type. A /timestamp/ is defined here as the duration elapsed
-- from some fixed but unknown initial time. We expect timestamps,
-- therefore durations as well, to be totally ordered in a time scale.
-- The type variable @d@ represents the duration type, which will be commonly
-- restricted to be a 'Num' type.
-- While the sum of durations makes sense, the sum of two timestamps does not, so
-- they are only equiped with primitives 'duration' and 'shift'.
newtype Time d = Time d deriving (Eq,Ord)

duration :: Num d => Time d -> Time d -> d
duration (Time d1) (Time d2) = d2 - d1

shift :: Num d => Time d -> d -> Time d
shift (Time d1) d2 = Time (d1 + d2)

class (Ord d, Num d, Monad m, Monad t)
  => TimedMonad m d t | t->m , t->d where
  -- | Current /specified/ timestamp.
  now :: t (Time d)
  -- | Current time drift, i.e. the difference between the /specified/ timestamp
  -- and the /actual/ timestamp (as measured by the underlying runtime).
  drift :: t d
  -- | Wait until the current /specified/ timestamp shifted by the given positive duration.
  delay :: d -> t()
  -- | Turns an action of the underlying monad to an action in the timed monad.
  lift :: m a -> t a
  -- | Allows moving a timed action back into the underlying untimed monad.
  run :: t a -> m a

realNow :: TimedMonad m d t => t (Time d)
realNow = do t <- now
             shift t <$> drift

-- | Returns the /speficied/ duration of a timed action (shall always be positive).
-- Used to formulate the following (equational) laws.
--
-- Return actions take no time and the duration of two actions composed by the
-- bind operator is the sum of the durations of these actions.
--
-- prop> dur (return a) == return 0
-- prop> dur (m >> m') == do {d <- dur m; d' <- dur m'; return (d+d')}
--
-- Derived from this, we also see that functors preserve specified durations,
-- time measurement actind over types as a fibration.
--
-- prop> durm m == dur (fmap f m)
--
-- 'now' and 'drift' should instantaneously (/specified/ time-wise) return and
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
-- The reverse composition does not for any timed action with non-zero duration.
dur :: TimedMonad m d t => t a -> t d
dur m = do t0 <- now
           _ <- m
           t1 <- now
           return (duration t1 t0)

-- | This 'lift' takes into account  the actual duration of the action, so this
-- can be used with blocking actions like 'getChar' from the IO monad.
timedLift :: TimedMonad m d t => m a -> t a
timedLift m = do a <- lift m
                 d <- drift
                 delay d
                 return a

-- | Monads with enough timing information to be extended to a 'TimedMonad'.
-- Must satisfy __time monotonicity__, meaning time shall flow from the past to the future.
--
-- prop> let (t0,t1) = unsafePerformIO $ do {t0 <- getRealTime; _ <- m; t1 <- getRealTime; return (t0,t1)} in t0 <= t1
--
-- Also wait must be coherent, meaning that 'waitUntil' shall never resume beforet he expected
-- timestamp is actually passed for real.
--
-- prop> let t1 = unsafePerformIO $ do {_ <- waitUntil t0; t1 <- getRealTime; return t1} in t0 <= t1
class (Ord d, Num d, Monad m) => HasTimer m d where
  getRealTime :: m (Time d)
  waitUntil :: Time d -> m ()
  getDrift :: Time d -> m d
  getDrift t = do r <- getRealTime
                  return (duration r t)

-- IO example

-- | Type of durations measured in microseconds.
-- NOTE: Intances for 'Integral' and its superclasses 'Enum' and 'Real'
-- are needed for the use of 'toInteger' below. This are not derived in the paper,
-- which is either an error or due to a change in the 'Num' class.
newtype Micro = Micro Int deriving (Show, Eq, Ord, Num, Enum, Real, Integral)


getSystemTime :: IO (Time Micro)
getSystemTime = do t <- getTime Monotonic
                   return . Time . fromInteger $ div (toNanoSecs t) 1000

instance HasTimer IO Micro where
  getRealTime = getSystemTime
  waitUntil (Time d) = do Time r <- getSystemTime
                          threadDelay . fromInteger . toInteger $ d - r


-- Deriving a TimedMonad instance from a monad with timing information

-- | A timed action.
newtype TA m d a = TA (Time d -> m (Time d, a))

-- | Timed actions are monads.
-- NOTE: Default instances of 'Functor' and 'Applicative' are defined,
-- to be compatible with current Haskell, by moving the definition of 'return'
-- to 'pure' and using some library functions from "Control.Monad".
-- Also the @HasTimer m d@ restriction its ommited as it is not needed.
instance Monad m => Monad (TA m d) where
  TA ta >>= f = TA (\s -> do (s1,a) <- ta s
                             let (TA ta1) = f a
                             ta1 s1)

---------------------------------------------------------------------------------

-- These require the constraits as they are dependent on the 'Monad' instance.

instance Monad m => Functor (TA m d) where
  fmap = liftM

instance Monad m => Applicative (TA m d) where
  pure a = TA (\s -> return (s,a))
  (<*>) = ap

---------------------------------------------------------------------------------

-- NOTE: removed unnecesary constraint @Monad m@ already implied by @HasTimer m d@
instance HasTimer m d => TimedMonad m d (TA m d) where
  now = TA (\s -> return (s,s))
  drift = TA $ \s -> do d <- getDrift s
                        return (s,d)
  delay d | d <= 0 = return ()
  delay d | d > 0 = TA $ \s -> do dr <- getDrift s
                                  waitUntil (shift s (d - dr))
                                  return (shift s d, ())
  lift m = TA $ \s -> do a <- m
                         return (s,a)
  run (TA ta) = do t <- getRealTime
                   (_,a) <- ta t
                   return a

-- | The default time extension of the IO monad, having the instance
-- @TimedMonad IO Micro TIO@ deriving from the instance of @HasTimer IO Micro@.
type TIO = TA IO Micro

----------------------------------------------------------------------

-- | This class enclose the relationship between an /inner/ duration
-- (_e.g._ the physical time) and
-- an /outer/ duration (_e.g._ the musical time) using a tempo/speed type
-- (supposed to be piecewise constant).
-- Any instance must satisfy the following equations (up to rounding errors dependent
-- on the actual numerical types):
--
-- prop> backStep s (step s i) == i
-- prop> step d (backStep d o) == o
class (Num i, Num o, Num s) => ScaleChange i o s | s ->i, s->o where
  initialSpeed :: s
  step :: s -> i -> o
  backStep :: s -> o -> i

newtype Beat = Beat Double deriving (Eq,Ord,Show,Num)
newtype BPM = BPM Double deriving (Eq,Ord,Show,Num)

instance ScaleChange Micro Beat BPM where
  initialSpeed = BPM 60
  step (BPM t) (Micro d) = Beat $ t * fromIntegral d/ratio
    where ratio = 60 * 10^6
  backStep (BPM t) (Beat d) = Micro . fromIntegral . floor $ (d/t) * ratio
    where ratio = 60 * 10^6

-- | Symbolic timed states.
data ST i o s = ST { innerTime :: Time i, outerTime :: Time o, speed :: s }

-- | Symbolic timed actions. A generalization of timed actions 'TA'
-- NOTE: @newtype@ is used instead of @data@ for optimization.
newtype STA m i o s a = STA (ST i o s -> m (ST i o s,a))

-- NOTE: The following instances are suggested or implied by the article
-- They are consequence of STA being, again, a rather simple state monad.

instance Monad m => Functor (STA m i o s) where
  fmap = liftM

instance Monad m => Applicative (STA m i o s) where
  pure a = STA (\st -> return (st,a))
  (<*>) = ap

instance Monad m => Monad (STA m i o s) where
  STA sta >>= f = STA (\st -> do (st', a) <- sta st
                                 let STA sta' = f a
                                 (st'', b) <- sta' st'
                                 return (st'',b))


-- | Standard timed extension of a Monad with timing information using symbolic
-- time states. Analogous to the 'TA' instance.
-- TODO: Should not @Ord o@ be derived from variable constraints of the 'TimedMonad'?
instance (Ord o, ScaleChange i o s, HasTimer m i) => TimedMonad m o (STA m i o s) where
  now = STA (\st -> return (st, outerTime st))
  drift = STA $ \st -> do d <- getDrift $ innerTime st
                          return (st,step (speed st) d)
  delay d | d <= 0 = return ()
  delay d | d > 0 = STA $ \(ST ti to s) -> do dr <- getDrift ti
                                              let ti' = shift ti (backStep s d - dr)
                                                  to' = shift to (d - step s dr)
                                              waitUntil ti'
                                              return (ST ti' to' s, ())
  lift m = STA $ \st -> do a <- m
                           return (st,a)
  -- | NOTE: Explicit type annotations are needed to unify the types from the 'ScaleChange' and 'HasTimer'
  -- constraints
  run (STA sta) = do Time (inner :: i) <- getRealTime
                     let outer :: o
                         outer = step (initialSpeed :: s) inner
                     (_,a) <- sta $ ST (Time inner) (Time outer) initialSpeed
                     return a

--------------------------------------

type MusicIO = STA IO Micro Beat BPM

setTempo :: BPM -> MusicIO ()
setTempo t | t <= 0 = error "setTempo: forbidden negative tempo"
           | otherwise = STA $ \st -> let ST ti to _ = st in return (ST ti to t, ())

playInIO = run $ setTempo 90 >> playMusic func 30

-- Test function
func :: Int -> Music Pitch
func n = note qn (pitch n)

playMusic :: (Int -> Music Pitch) -> Int -> MusicIO ()
playMusic f n =
  do playNote (f n) 1
     delay 1
     playMusic f (n+1)

playNote :: Music Pitch -> Beat -> MusicIO ()
playNote n d = lift (forkIO $ play n) >> delay d

-- | Monad references implementing the concept of promises.
-- NOTE: Following a compiler warning, the deprecated @StarIsType@ extension is dropped and
-- 'Data.Kind.Type' is used instead for the type family's signature.
class Monad m => MonadRef m where
  type Ref m :: Type -> Type
  fork :: m a -> m (Ref m a)
  read :: Ref m a -> m a
  tryRead :: Ref m a -> m (Maybe a)
  parRead :: Ref m a -> Ref m b -> m (Either a b)

-- | Timed monad references
data TRef m d a = TRef (Time d) (Ref m (Time d , a))

-- | Equipping a timed extension of a monad by monad references,
-- given the underlying monad itself has references.
instance (MonadRef m, HasTimer m d) => MonadRef (TA m d) where
  type Ref (TA m d) = TRef m d
  fork (TA m) = TA $ \s -> do {r <- fork (m s); return (s, TRef s r )}
  read (TRef _ r) = TA $ \s -> do {(t, a) <- read r ; return (max s t, a)}
  tryRead (TRef _ r) = TA $ \s ->
    do c <- tryRead r
       case c of Nothing     -> return (s, Nothing)
                 Just (t, a) -> return (max s t, Just a)
  parRead (TRef _ r1) (TRef _ r2) = TA $ \s ->
    do c <- parRead r1 r2
       case c of Left (t, a)  -> return (max s t, Left a)
                 Right (t, b) -> return (max s t, Right b)

-- REVIEW the following

durRef :: (MonadRef m, HasTimer m d) => TRef m d a -> TA m d d
durRef (TRef t0 r) = TA (\s -> do (t,_) <- read r
                                  return (max s t, duration t t0))

replayRef :: (MonadRef m, HasTimer m d) => TRef m d a -> TA m d a
replayRef r = do t1 <- now
                 d <- durRef r
                 a <- read r
                 t2 <- now
                 delay (d - duration t2 t1)
                 return a

expandRef :: (MonadRef m, HasTimer m d) => (d -> d ) -> TRef m d a -> TA m d a
expandRef f r = do t1 <- now
                   d <- durRef r
                   a <- read r
                   t2 <- now
                   let d1 = f d - duration t2 t1
                   delay d1
                   return a
