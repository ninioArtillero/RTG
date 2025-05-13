-- |
-- Module      : TimePatterns
-- Description : A collection of (rational) patterns representing scales and rhythms
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.TimePatterns
  ( -- * Equal tempered twelve tone scale.
    diatonic,
    diminished,
    wholeTone,
    japanese,
    gypsy,

    -- * Toussaint's distinguished timelines.

    -- | Five onset in sixteen pulses timelines from chap. 7 of
    -- "The Geometry of Musical Rhythm", G. Toussaint.
    shiko,
    clave,
    soukous,
    rumba,
    bossa,
    gahu,

    -- * Notable irreducible perfectly balanced rhythms.
    fiveBalance,
    amiotScale,

    -- *  Test patterns.
    firstQuart,
    crowded,
    patternLibrary,
  )
where

import Data.Group (Group (..))
import qualified Data.Set as Set
import Sound.RTG.Event (Event, integralsToEvents)
import Sound.RTG.PerfectBalance (indicatorVector)
import Sound.RTG.RhythmicPattern (Rhythm (Rhythm), Rhythmic (..))
import Sound.RTG.Utils (modOne, setNub)
import Sound.RTG.Zip (euclideanZipWith)

type Time = Rational

-- | Time patterns are ordered lists of 'Rational' values
-- within the \([0,1)\) interval.
newtype TimePattern = TimePattern {getPattern :: [Time]}

-- | Equality follows 'TimePattern' specification.
instance Eq TimePattern where
  xs == ys = queryPattern xs == queryPattern ys

-- | Time patterns are displayed using floating numbers, hoping that they are
-- easier to read this way.
instance Show TimePattern where
  show = ("Time pattern: " ++) . show . timeAsFloats

-- | Used exclusively for the 'Show' instance.
timeAsFloats :: TimePattern -> [Float]
timeAsFloats pattern = map fromRational $ queryPattern pattern :: [Float]

-- | This function enforces the specification of the 'TimePattern' type.
-- It wraps rationals within the number line to the \([0,1)\) interval,
-- which conceptually represent a /circle/, order the values and remove duplicates.
queryPattern :: TimePattern -> [Time]
queryPattern = setNub . map modOne . getPattern

instance Semigroup TimePattern where
  (<>) = timePatternProduct

-- | A modular sum within the circle (see 'queryPattern'), matching patterns of
-- different length using an 'euclideanPattern' distribution.
timePatternProduct :: TimePattern -> TimePattern -> TimePattern
timePatternProduct xs ys =
  let pat1 = getPattern xs
      pat2 = getPattern ys
   in TimePattern $ euclideanZipWith (\x y -> modOne $ x + y) pat1 pat2

instance Monoid TimePattern where
  mempty = TimePattern [0]

instance Group TimePattern where
  invert = TimePattern . map (\n -> if n /= 0 then 1 - n else 0) . queryPattern

-- | Reflects the circle, represented as the interval \([0,1)\),
-- on a diameter passing through \(0.5\).
reflectCircle :: [Time] -> [Time]
reflectCircle = map (\n -> if n /= 0 then 1 - n else 0)

instance Rhythmic TimePattern where
  toRhythm = Rhythm . timeToOnset . queryPattern

-- | Uses the 'indicatorVector' to embed the pattern in a discrete chromatic universe.
timeToOnset :: [Time] -> [Event]
timeToOnset xs = integralsToEvents (indicatorVector xs)

diatonic :: TimePattern
diatonic = TimePattern [0 / 12, 2 / 12, 4 / 12, 5 / 12, 7 / 12, 9 / 12, 11 / 12]

diminished :: TimePattern
diminished = TimePattern [0 / 12, 2 / 12, 3 / 12, 5 / 12, 6 / 12, 8 / 12, 9 / 12, 11 / 12]

wholeTone :: TimePattern
wholeTone = TimePattern [0 / 12, 2 / 12, 4 / 12, 6 / 12, 8 / 12, 10 / 12]

-- | Gypsy scale, also known as the double harmonic scale.
-- Is the unique 7-pattern in a 12-space
-- both perfectly balanced and irreducibly periodic.
gypsy :: TimePattern
gypsy = TimePattern [0 / 12, 1 / 12, 4 / 12, 5 / 12, 7 / 12, 8 / 12, 11 / 12]

-- | The japanese pentatonic scale.
japanese :: TimePattern
japanese = TimePattern [0 / 12, 1 / 12, 5 / 12, 7 / 12, 8 / 12]

-- | Unique 5-pattern in a 12-space
-- both perfectly balanced and irreducibly periodic.
fiveBalance :: TimePattern
fiveBalance = TimePattern [0 / 12, 4 / 12, 5 / 12, 8 / 12, 11 / 12]

shiko :: TimePattern
shiko = TimePattern [0 / 16, 4 / 16, 6 / 16, 10 / 16, 12 / 16]

clave :: TimePattern
clave = TimePattern [0 / 16, 3 / 16, 6 / 16, 10 / 16, 12 / 16]

soukous :: TimePattern
soukous = TimePattern [0 / 16, 3 / 16, 6 / 16, 10 / 16, 11 / 16]

rumba :: TimePattern
rumba = TimePattern [0 / 16, 3 / 16, 7 / 16, 10 / 16, 12 / 16]

bossa :: TimePattern
bossa = TimePattern [0 / 16, 3 / 16, 6 / 16, 10 / 16, 13 / 16]

gahu :: TimePattern
gahu = TimePattern [0 / 16, 3 / 16, 6 / 16, 10 / 16, 14 / 16]

-- | The unique perfectly balanced rhythm in a 30-fold chromatic universe
-- that cannot be produced by the sum of disjoint polygons.
amiotScale :: TimePattern
amiotScale = TimePattern [0 / 30, 6 / 30, 7 / 30, 13 / 30, 17 / 30, 23 / 30, 24 / 30]

firstQuart :: TimePattern
firstQuart = TimePattern [0 / 16, 1 / 16, 2 / 16, 3 / 16]

crowded :: TimePattern
crowded = TimePattern [1, 1, 1, 1]

patternLibrary :: IO ()
patternLibrary =
  mapM_
    putStrLn
    [ "12-TET SCALES:",
      "",
      "diatonic",
      "diminished",
      "wholeTone",
      "gypsy",
      "japanese",
      "",
      "DISTIGUISHED TOUSSAINT RHYTHMS",
      "",
      "shiko",
      "clave",
      "rumba",
      "bossa",
      "gahu",
      "soukous",
      "",
      "UNIQUE PATTERNS",
      "",
      "fiveBalance",
      "amiotScale"
    ]
