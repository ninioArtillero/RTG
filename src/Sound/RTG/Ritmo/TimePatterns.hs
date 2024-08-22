-- | A collection of predefined time patterns
module Sound.RTG.Ritmo.TimePatterns where

import Sound.RTG.Ritmo.RatioDecons (modOne)
import Data.Group ( Group(invert) )
import Data.List (nub, sort)

type Time = Rational

newtype TimePattern = TimePattern [Time]

queryPattern :: TimePattern -> [Time]
queryPattern (TimePattern ts) = map modOne ts

getPattern :: TimePattern -> [Time]
getPattern (TimePattern ts) = ts

instance Show TimePattern where
  show = show . nub . sort . queryPattern

-- | This operation is isomorphic to the monadic bind for lists
instance Semigroup TimePattern where
  xs <> ys = TimePattern [x*y | x <- getPattern xs, y <- getPattern ys ]

instance Monoid TimePattern where
  mempty = TimePattern [1]

instance Group TimePattern where
  invert = TimePattern . map (\n -> if n /= 0 then 1 / n else 0) . getPattern

-- | Twelve tone equal temperament scales.
diatonic :: TimePattern
diatonic = TimePattern [0/12, 2/12, 4/12, 5/12, 7/12, 9/12, 11/12]

diminished :: TimePattern
diminished = TimePattern [0/12, 2/12, 3/12, 5/12, 6/12, 8/12, 9/12, 11/12]

wholeTone :: TimePattern
wholeTone = TimePattern [0/12, 2/12, 4/12, 6/12, 8/12, 10/12]

-- | Gypsy scale, also known as the double harmonic scale.
-- Is the unique 7-pattern in a 12-space
-- both perfectly balanced and irreducibly periodic.
gypsy :: TimePattern
gypsy = TimePattern [0/12, 1/12, 4/12, 5/12, 7/12, 8/12, 11/12]

japanese :: TimePattern
japanese = TimePattern [0/12, 1/12, 5/12, 7/12, 8/12]

-- | Unique 5-pattern in a 12-space
-- both perfectly balanced and irreducibly periodic.
fiveB :: TimePattern
fiveB = TimePattern [0/12, 4/12, 5/12, 8/12, 11/12]

-- | Toussaint's five onset in sixteen pulse distinguished timelines.
-- from chap. 7 of "The Geometry of Musical Rhythm", G. Toussaint.
shiko :: TimePattern
shiko = TimePattern [0/16, 4/16, 6/16, 10/16, 12/16]

clave :: TimePattern
clave = TimePattern [0/16, 3/16, 6/16, 10/16, 12/16]

soukous :: TimePattern
soukous = TimePattern [0/16, 3/16, 6/16, 10/16, 11/16]

rumba :: TimePattern
rumba = TimePattern [0/16, 3/16, 7/16, 10/16, 12/16]

bossa :: TimePattern
bossa = TimePattern [0/16, 3/16, 6/16, 10/16, 13/16]

gahu :: TimePattern
gahu = TimePattern [0/16, 3/16, 6/16, 10/16, 14/16]

-- | The unique perfectly balanced rhythm in a 30-fold chromatic universe
-- that cannot be produced by the sum of disjoint polygons.
amiotScale :: TimePattern
amiotScale = TimePattern [0/30, 6/30, 7/30, 13/30, 17/30, 23/30, 24/30]

firstQuart :: TimePattern
firstQuart = TimePattern [0/16, 1/16, 2/16, 3/16]

crowded :: TimePattern
crowded = TimePattern [1, 1, 1, 1]
