# Tipos de datos básicos de Conductive

Fuente: ConductiveBaseData.hs
Created (hackage): Fri Sep 14 16:30:33 JST 2012


```haskell
module Sound.Conductive.ConductiveBaseData where

import Sound.Conductive.MutableMap

-- | a data type for traditional musical time

data MusicalTime = MusicalTime { measure :: Int
                               , beat :: Double
                               } deriving (Show)

-- | a data type for describing a tempo and when it began

data TempoChange = TempoChange { newTempo :: Double
                               , beatOfTempoChange :: Double
                               , timeOfTempoChange :: Double
                               } deriving (Show)

-- | a data type describing a time signature and when it began. A time signature is specified as number of beats per measure.

data TimeSignature = TimeSignature { startingMeasure :: Int
                                   , startingBeat :: Double
                                   , timeSignature :: Int
                                   } deriving (Show)

-- | for Players, the key time-related data type

data TempoClock = TempoClock { startTime :: Double
                             , tempoChanges :: [TempoChange]
                             , timeSignatureChanges :: [TimeSignature]
                             } deriving (Show)


-- | Players, TempoClocks, etc. are stored here.

data MusicalEnvironment = MusicalEnvironment
    { environmentName :: String
    , playerStore :: MutableMap String Player
    , tempoClockStore :: MutableMap String TempoClock
    , iOIStore :: MutableMap String (MusicalEnvironment -> Player -> Double -> Double -> IO Double)
    , actionStore :: MutableMap String (MusicalEnvironment -> Player -> Double -> Double -> IO ())
    , interruptStore :: MutableMap String ([IO ()])
    }

instance Show MusicalEnvironment where
    show x = environmentName x

-- | a data type used by the play function and useful when displaying running players

data PlayerStatus =   Stopped 
                    | Playing 
                    | Pausing 
                    | Paused 
                    | Stopping 
                    | Resetting
                    deriving (Eq,Show)

-- | Players are played using the play functions.

data Player = Player
    { playerName :: String
    , playerStatus :: PlayerStatus
    , playerCounter :: Integer
    , playerClock :: String
    , playerIOI :: String
    , playerAction :: String
    , playerInterrupt :: String
    , playerBeat :: Double
    , playerStartingBeat :: Double
    , playerPauseTime :: Double
    } deriving (Show)
```
