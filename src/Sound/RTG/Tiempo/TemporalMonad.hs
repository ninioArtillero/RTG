-- | Temporal Monad implementation of Virtual Time Semantics from
--
-- Aaron, Samuel, Dominic Orchard, y Alan F. Blackwell. 2014.
-- “Temporal semantics for a live coding language”.
-- In Proceedings of the 2nd ACM SIGPLAN international workshop on
-- Functional art, music, modeling & design, 37–47. FARM ’14.
-- New York, NY, USA: Association for Computing Machinery.
-- https://doi.org/10.1145/2633638.2633648.
module Sound.RTG.Tiempo.TemporalMonad where

import Sound.Osc.Time (currentTime)
import Sound.Osc (pauseThread)


type Time = Double
type VTime = Double

newtype Temporal a = T ((Time,Time) -> (VTime -> IO (a, VTime))) deriving

instance Functor Temporal where
  f <$> T p = T (fmap (fmap (fmap f p)))

instance Functor Applicative
  pure a = T (\(_,_) -> \vt -> return (a,vt))

instance Monad Temporal where
  return = pure
  T p >>= q = T (\(startT,nowT) -> \vt ->
                    do (x,vt') <- p (startT,nowT) vt
                       let T q' = q x
                       thenT <- currentTime
                       q' (startT,thenT) vt')

time :: Temporal Time
time = T (\(_,nowT) -> \vT -> return (nowT,vT))

start :: Temporal Time
start = T (\(startT,_) -> \vT -> return (startT,vT))

getVirtualTime :: Temporal VTime
getVirtualTime = T (\(_,_) -> \vT -> return (vT , vT ))

setVirtualTime :: VTime -> Temporal ()
setVirtualTime vT = T (\_ -> \_ -> return ((), vT ))

kernelSleep :: RealFrac a => a -> Temporal ()
kernelSleep t = T (\(_,_) -> \vT ->
                      do pauseThread t
                         return ((), vT ))
