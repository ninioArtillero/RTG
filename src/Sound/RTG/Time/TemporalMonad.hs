{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Avoid lambda" #-}
-- | Temporal Monad for virtual time semantics from
--
-- Aaron, Samuel, Dominic Orchard, y Alan F. Blackwell. 2014.
-- “Temporal semantics for a live coding language”.
-- In Proceedings of the 2nd ACM SIGPLAN international workshop on
-- Functional art, music, modeling & design, 37–47. FARM ’14.
-- New York, NY, USA: Association for Computing Machinery.
-- https://doi.org/10.1145/2633638.2633648.
module Sound.RTG.Time.TemporalMonad (o, l, s') where

import           Control.Concurrent               (ThreadId, forkIO, readMVar)
import           Control.Monad                    (forever)
import           Euterpea                         (Pitch, note, play)
import qualified Sound.Osc.Fd                     as Osc
import           Sound.RTG.Rhythm.RhythmicPattern (Binary (..), Rhythm (..),
                                                   Rhythmic (..), Root,
                                                   getRhythm, scalePitches,
                                                   toRhythm)
import           Sound.RTG.Time.Messages          (CPS, Dur, SampleName,
                                                   eventDuration,
                                                   superDirtMessage,
                                                   superDirtPort)
import           Sound.RTG.Time.UnSafe            (globalCPS)
-- TODO: Change real types to rationals (to have use the same number type across modules)?
-- efficiency gain?
type Time = Double
type VTime = Double

-- | Output values
data Value = NoValue | Output !Osc.Message deriving (Show, Eq)

newtype Temporal a = T ((Time,Time) -> (VTime -> IO (a, VTime)))

-- First fmap composes, the seconds maps into the IO monad
-- and the third into the first element of the pair.
instance Functor Temporal where
  fmap f  (T p) = T ( \(startT,nowT) -> \vT -> fmap (\(x,y) -> (f x,y)) . p (startT, nowT) $ vT)

instance Applicative Temporal where
  pure a = T (\(_,_) -> \vT -> return (a,vT))
  T f <*> T p = T (\(startT,nowT) -> \vT ->
                    do (f, vT') <- f (startT,nowT) vT
                       thenT <- Osc.currentTime
                       (x,vT'') <- p (startT,thenT) vT'
                       return (f x, vT''))

instance Monad Temporal where
  return = pure
  T p >>= q = T (\(startT,nowT) -> \vT ->
                    do (x,vT') <- p (startT,nowT) vT
                       let T q' = q x
                       thenT <- Osc.currentTime
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
                      do Osc.pauseThread t
                         return ((), vT ))

-- | Update virtual time and suspends computation
-- if actual time does not overflows it.
sleep :: VTime -> Temporal Value
sleep delayT = do nowT <- time
                  vT <- getVirtualTime
                  let vT' = vT + delayT
                  setVirtualTime vT'
                  startT <- start
                  let diffT = diffTime nowT startT
                  if vT' < diffT
                    then return ()
                    else kernelSleep (vT' - diffT)
                  return NoValue

diffTime :: Double -> Double -> Double
diffTime x y = x - y

-- | Executing a temporal computation/program
runTime :: Temporal a -> IO a
runTime (T c) = do startT <- Osc.currentTime
                   (a, _) <- c (startT,startT) 0
                   return a

-- Play functions

playEvent :: Osc.Transport t => t -> SampleName -> Dur -> Binary -> Temporal Value
playEvent port sample delayT x =
  T (\(_,_) -> \vT -> do case x of
                           One -> do Osc.sendMessage port (superDirtMessage sample);
                                     return (Output $ superDirtMessage sample, vT)
                           Zero -> return (NoValue,vT))

openSuperDirtPort :: Temporal Osc.Udp
openSuperDirtPort = T (\(_,_) -> \vT ->
                       do port <- superDirtPort
                          return (port , vT))


temporalPattern :: CPS -> SampleName -> [Binary] -> Temporal ()
temporalPattern _ _ [] = return ()
temporalPattern cps sample pttrn = do
  let n = length pttrn
      delayT = eventDuration cps n
  port <- openSuperDirtPort
  sequence_ . addSleeps delayT . map (playEvent port sample delayT) $ pttrn

scalePattern :: CPS -> [Pitch] -> Temporal ()
scalePattern _ [] = return ()
scalePattern cps scale = do
  let n = length scale
      delayT = eventDuration cps n
  sequence_ . addSleeps delayT . map (playMusic $ delayT/2) $ scale

playMusic :: CPS -> Pitch ->  Temporal Value
playMusic dur p =
  T (\(_,_) -> \vT -> do forkIO . play $ note (toRational dur) p; return (NoValue,vT))


addSleeps :: Time -> [Temporal Value] -> [Temporal Value]
addSleeps delayT = foldr (\t acc -> t : sleep delayT : acc) []


-- | Play a rhythmic pattern once
-- TODO: parametrize and hide CPS
o :: Rhythmic a => SampleName -> a -> IO ThreadId
o sample rhythmic = do
  forkIO $ do
    cps <- readMVar globalCPS
    runTime . temporalPattern (fromRational cps) sample . getRhythm . toRhythm $ rhythmic

-- | Loop a rhythmic pattern
-- TODO: parametrize and hide CPS
l :: Rhythmic a => SampleName -> a -> IO ThreadId
l sample rhythmic = do
  forkIO $ forever $ do
    cps <- readMVar globalCPS
    runTime . temporalPattern (fromRational cps) sample . getRhythm . toRhythm $ rhythmic

-- | Play as scale
s' :: Rhythmic a => Root -> a -> IO ThreadId
s' root rhythm =
  forkIO . forever $ do
    cps <- readMVar globalCPS
    runTime . scalePattern (fromRational cps) . scalePitches root $ rhythm
