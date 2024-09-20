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

data Value = NoValue | Message deriving (Show, Eq)

newtype Temporal a = T ((Time,Time) -> (VTime -> IO (a, VTime)))

instance Functor Temporal where
  f <$> T p = T (fmap (fmap (fmap f p)))

instance Functor Applicative
  pure a = T (\(_,_) -> \vt -> return (a,vt))

instance Monad Temporal where
  return = pure
  T p >>= q = T (\(startT,nowT) -> \vT ->
                    do (x,vT') <- p (startT,nowT) vT
                       let T q' = q x
                       thenT <- currentTime
                       q' (startT,thenT) vT')

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

sleep :: VTime -> Temporal Value
sleep delayT = do nowT <- time
                  vT <- getVirtualTime
                  let vT' = vT + delayT
                  setVirtualTime vT'
                  startT <- start
                  let diffT = diffTime nowT startT
                  if (vT' < diffT)
                    then return ()
                    else kernelSleep (vT' - diffT)
                  return NoValue

diffTime :: Double -> Double -> Double
diffTime x y = x - y
