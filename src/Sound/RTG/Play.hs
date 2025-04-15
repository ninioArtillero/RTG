{-|
Module      : Play
Description : Play functions to run patterns using the Temporal Monad
Copyright   : (c) Xavier Góngora, 2023
License     : GPL-3
Maintainer  : ixbalanque@protonmail.ch
Stability   : experimental
-}
module Sound.RTG.Play (o, l, s') where

import           Control.Concurrent               (ThreadId, forkIO, readMVar)
import           Control.Monad                    (forever)
import           Euterpea                         (Pitch, note, play)
import qualified Sound.Osc.Fd                     as Osc
import           Sound.RTG.RhythmicPattern (Binary (..), Rhythm (..),
                                                   Rhythmic (..),
                                                   rhythm
                                                   )
import Sound.RTG.PlayScale (scalePitches, Root)
import           Sound.RTG.OscMessages       (CPS, Dur, SampleName,
                                                   eventDuration,
                                                   superDirtMessage,
                                                   superDirtPort)
import           Sound.RTG.UnSafe            (globalCPS)
import Sound.RTG.TemporalMonad (Temporal (..), Value (..), Time, runTime, sleep)



-- | Play a rhythmic pattern once.
o :: Rhythmic a => SampleName -> a -> IO ThreadId
o sample rhythmic = do
  forkIO $ do
    cps <- readMVar globalCPS
    runTime . temporalPattern (fromRational cps) sample . rhythm $ rhythmic

-- | Loop a rhythmic pattern.
l :: Rhythmic a => SampleName -> a -> IO ThreadId
l sample rhythmic = do
  forkIO $ forever $ do
    cps <- readMVar globalCPS
    runTime . temporalPattern (fromRational cps) sample . rhythm $ rhythmic

-- | Play as scale.
s' :: Rhythmic a => Root -> a -> IO ThreadId
s' root rhythmic =
  forkIO . forever $ do
    cps <- readMVar globalCPS
    runTime . scalePattern (fromRational cps) . scalePitches root $ rhythmic

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


