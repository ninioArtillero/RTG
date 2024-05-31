-- | A collection of predefined time patterns
module Sound.RTG.Ritmo.TimePatterns where

import           Sound.RTG.Ritmo.Pattern (Pattern, Time)

-- | Twelve tone equal temperament scales.
diatonic :: Pattern Time
diatonic = [0/12, 2/12, 4/12, 5/12, 7/12, 9/12, 11/12]

diminished :: Pattern Time
diminished = [0/12, 2/12, 3/12, 5/12, 6/12, 8/12, 9/12, 11/12]

wholeTone :: Pattern Time
wholeTone = [0/12, 2/12, 4/12, 6/12, 8/12, 10/12]

-- | Gypsy scale, also known as the double harmonic scale.
-- Is the unique 7-pattern in a 12-space
-- both perfectly balanced and irreducibly periodic.
gypsy :: Pattern Time
gypsy = [0/12, 1/12, 4/12, 5/12, 7/12, 8/12, 11/12]

japanese :: Pattern Time
japanese = [0/12, 1/12, 5/12, 7/12, 8/12]

-- | Unique 5-pattern in a 12-space
-- both perfectly balanced and irreducibly periodic.
fiveB :: Pattern Time
fiveB = [0/12, 4/12, 5/12, 8/12, 11/12]

-- | Toussaint's five onset in sixteen pulse distinguished timelines.
-- from chap. 7 of "The Geometry of Musical Rhythm", G. Toussaint.
shiko :: Pattern Time
shiko = [0/16, 4/16, 6/16, 10/16, 12/16]

clave :: Pattern Time
clave = [0/16, 3/16, 6/16, 10/16, 12/16]

soukous :: Pattern Time
soukous = [0/16, 3/16, 6/16, 10/16, 11/16]

rumba :: Pattern Time
rumba = [0/16, 3/16, 7/16, 10/16, 12/16]

bossa :: Pattern Time
bossa = [0/16, 3/16, 6/16, 10/16, 13/16]

gahu :: Pattern Time
gahu = [0/16, 3/16, 6/16, 10/16, 14/16]

-- | The unique perfectly balanced rhythm in a 30-fold chromatic universe
-- that cannot be produced by the sum of disjoint polygons.
amiotScale :: Pattern Time
amiotScale = [0/30, 6/30, 7/30, 13/30, 17/30, 23/30, 24/30]

firstQuart :: Pattern Time
firstQuart = [0/16, 1/16, 2/16, 3/16]

crowded :: Pattern Time
crowded = [1, 1, 1, 1]
