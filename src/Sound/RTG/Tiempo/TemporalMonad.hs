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
module Sound.RTG.Tiempo.TemporalMonad where

import           Control.Concurrent              (forkIO)
import           Sound.Osc.Fd                    (Datum (AsciiString), Message,
                                                  Transport (..), Udp, ascii,
                                                  currentTime, message, openUdp,
                                                  pauseThread, sendMessage)
import           Sound.RTG.Ritmo.RhythmicPattern (Binary (..), Rhythm (..),
                                                  Rhythmic (..), getRhythm,
                                                  toRhythm)
import Data.List (intersperse)


type Time = Double
type VTime = Double

-- | Output value type
data Value = NoValue | Output Message deriving (Show, Eq)

newtype Temporal a = T ((Time,Time) -> (VTime -> IO (a, VTime)))

-- First fmap composes, the seconds maps into the IO monad
-- and the third into the first element of the pair.
instance Functor Temporal where
  fmap f  (T p) = T ( \(startT,nowT) -> \vT -> fmap (\(x,y) -> (f x,y)) . p (startT, nowT) $ vT)

instance Applicative Temporal where
  pure a = T (\(_,_) -> \vT -> return (a,vT))
  T f <*> T p = T (\(startT,nowT) -> \vT ->
                    do (f, vT') <- f (startT,nowT) vT
                       thenT <- currentTime
                       (x,vT'') <- p (startT,thenT) vT'
                       return (f x, vT''))

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
runTime (T c) = do startT <- currentTime
                   (a, _) <- c (startT,startT) 0
                   return a

-- IMPLEMENTATION

type Dur = Double
type CPS = Double
type SampleName = String

-- NOTE: que dentro de un valor temporal, la mónada en cuestión es la IO
playEvent :: Transport t => t -> SampleName -> Dur -> Binary -> Temporal Value
playEvent port sample delayT x = T (\(_,_) -> \vT ->
                                       do case x of
                                            One -> do
                                              sendMessage port (superDirtMessageGen sample);
                                              return (Output $ superDirtMessageGen sample, vT)
                                            Zero -> return (NoValue,vT))

openSuperDirtPort :: Temporal Udp
openSuperDirtPort = T (\(_,_) -> \vT ->
                       do port <- openUdp "127.0.0.1" 57120
                          return (port , vT))


playPattern :: CPS -> SampleName -> [Binary] -> Temporal ()
playPattern _ _ [] = return ()
playPattern cps sample pttrn = do
  let n = length pttrn
      delayT = eventDuration cps n
  port <- openSuperDirtPort
  sequence_ . intersperse (sleep delayT) . map (playEvent port sample delayT) $ pttrn


playM :: Rhythmic a => SampleName -> a -> IO ()
playM sample rhythmic = do
  forkIO $
    runTime . playPattern 0.4 sample . getRhythm . toRhythm $ rhythmic
  return ()


superDirtMessageGen :: SampleName -> Message
superDirtMessageGen sample = message
        "/dirt/play"
        [
          AsciiString $ ascii "s",
          AsciiString $ ascii sample
        ]


eventDuration :: Double -> Int -> Double
eventDuration cps pulses = secondsPerCycle / eventsPerCycle
  where
    secondsPerCycle = 1 / cps
    eventsPerCycle = fromIntegral pulses
