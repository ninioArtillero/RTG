-- | Output messages types and utility functions

module Sound.RTG.Time.Messages where

import qualified Sound.Osc as Osc

type Dur = Double
type CPS = Double
type SampleName = String

superDirtMessage :: SampleName -> Osc.Message
superDirtMessage sample = Osc.message
        "/dirt/play"
        [
          Osc.AsciiString $ Osc.ascii "s",
          Osc.AsciiString $ Osc.ascii sample
        ]

eventDuration :: CPS -> Int -> Dur
eventDuration cps pulses = secondsPerCycle / eventsPerCycle
  where
    secondsPerCycle = 1 / cps
    eventsPerCycle = fromIntegral pulses

superDirtPort :: IO Osc.Udp
superDirtPort = Osc.openUdp "127.0.0.1" 57120
