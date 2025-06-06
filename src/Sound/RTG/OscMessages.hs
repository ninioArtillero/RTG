-- |
-- Module      : OscMessages
-- Description : Default OSC messages
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.OscMessages (sendSuperDirtSample, superDirtMessage, superDirtPort, SampleName, Dur, CPS) where

import qualified Sound.Osc.Fd as Osc

type Dur = Double

type CPS = Double

type SampleName = String

sendSuperDirtSample :: SampleName -> IO ()
sendSuperDirtSample sample = do
  port <- superDirtPort
  Osc.sendMessage port (superDirtMessage sample)

superDirtMessage :: SampleName -> Osc.Message
superDirtMessage sample =
  Osc.message
    "/dirt/play"
    [ Osc.AsciiString $ Osc.ascii "s",
      Osc.AsciiString $ Osc.ascii sample
    ]

superDirtPort :: IO Osc.Udp
superDirtPort = Osc.openUdp "127.0.0.1" 57120
