-- | The pattern type and some basic operation
module Sound.RTG.Ritmo.Pattern where

import           Data.List                   (nub, sort)
import           Sound.RTG.Ritmo.RatioDecons (modOne)
import           Sound.RTG.Ritmo.Bjorklund   (euclideanPattern)

-- | Se utiliza tiempo racional para aprovechar su correlación con el pensamiento musical y
-- para preservar la precisión, postergando los cálculos con flotantes.
type Time = Rational

type Pattern a = [a]

-- Pattern zip functions

-- | Preserves the tail of the zip
frontWideZip :: Semigroup a => Pattern a -> Pattern a -> Pattern a
pttrn1 `frontWideZip` pttrn2 = zipWith (<>) pttrn1 pttrn2 ++ backDiffPattern pttrn1 pttrn2

-- | Regular zipWith using the Semigroup operator
frontNarrowZip :: Semigroup a => Pattern a -> Pattern a -> Pattern a
frontNarrowZip = zipWith (<>)

backWideZip :: Monoid a => Pattern a -> Pattern a -> Pattern a
pttrn1 `backWideZip` pttrn2 =
  if l1 > l2
    then zipWith (<>) pttrn1 (replicate diff mempty ++ pttrn2)
    else zipWith (<>) (replicate diff mempty ++ pttrn1) pttrn2
  where l1 = length pttrn1
        l2 = length pttrn2
        diff = abs $ l1 - l2

backNarrowZip :: Semigroup a => Pattern a -> Pattern a -> Pattern a
pttrn1 `backNarrowZip` pttrn2 =
  if l1 > l2
    then zipWith (<>) (drop diff pttrn1) pttrn2
    else zipWith (<>) pttrn1 (drop diff pttrn2)
  where l1 = length pttrn1
        l2 = length pttrn2
        diff = abs $ l1 - l2

centerWideZip :: Monoid a => Pattern a -> Pattern a -> Pattern a
pttrn1 `centerWideZip` pttrn2 =
  if l1 > l2
    then zipWith (<>) pttrn1 (replicate diffA mempty ++ pttrn2 ++ replicate diffB mempty)
    else zipWith (<>) (replicate diffA mempty ++ pttrn1 ++ replicate diffB mempty) pttrn2
  where l1 = length pttrn1
        l2 = length pttrn2
        diff = abs $ l1 - l2
        diffA = round . (/ 2) . fromIntegral $ diff
        diffB = diff - diffA

centerNarrowZip :: Semigroup a => Pattern a -> Pattern a -> Pattern a
pttrn1 `centerNarrowZip` pttrn2 =
  if l1 > l2
    then zipWith (<>) (drop diff pttrn1) pttrn2
    else zipWith (<>) pttrn1 (drop diff pttrn2)
  where l1 = length pttrn1
        l2 = length pttrn2
        diff = round . (/ 2) . fromIntegral . abs $ l1 - l2

-- | When patterns have different size,
-- distributes the operated elements as evenly as possible matching euclidean onsets.
-- Otherwise it zips one to one.
-- TODO: there's ambiguity regarding the position of the euclidean pattern,
-- this could be exploited. For example, use all and choose the one
-- with the least rests.
-- TODO: choose finite lists... may be I need stronger types (GADTs?).
euclideanZip :: Semigroup a => Pattern a -> Pattern a -> Pattern a
pttrn1 `euclideanZip` pttrn2
  | len1 == len2 = zipWith (<>) pttrn1 pttrn2
  | otherwise = fzip pttrn markedPattern []
  where (pttrn, markedPattern)= if len1 == k
          then (pttrn1, pttrn2 `zip` euclideanPattern k n)
          else (pttrn2, pttrn1 `zip` euclideanPattern k n)
        len1 = length pttrn1; len2 = length pttrn2
        k = min len1 len2; n = max len1 len2
        fzip :: Semigroup a => Pattern a -> Pattern (a,Int) -> Pattern a -> Pattern a
        fzip [] ys zs = reverse zs
        -- superflous case?
        fzip xs [] zs = reverse zs
        fzip (x:xs) (y:ys) zs = if snd y == 1 then fzip xs ys ((x <> fst y):zs)
                                              else fzip (x:xs) ys ((fst y):zs)
        -- is this a fold? branched fold?


-- Pattern operations

rotateLeft :: Int -> Pattern a -> Pattern a
rotateLeft _ [] = []
rotateLeft n xs = zipWith const (drop n (cycle xs)) xs

rotateRight :: Int -> Pattern a -> Pattern a
rotateRight _ [] = []
rotateRight n xs = take size $ drop (size - (n `mod` size)) (cycle xs)
  where
    size = length xs

patternSum :: Num a => Pattern a -> Pattern a -> Pattern a
patternSum = zipWith (+)

backDiffPattern :: Pattern a -> Pattern a -> Pattern a
backDiffPattern xs ys
  | lx > ly = drop ly xs
  | otherwise = drop lx ys
  where
    lx = length xs
    ly = length ys

frontDiffPattern :: Pattern a -> Pattern a -> Pattern a
frontDiffPattern xs ys
  | lx > ly = take (lx - ly) xs
  | otherwise = take (ly - lx) ys
  where
    lx = length xs
    ly = length ys


-- Auxiliary functions

-- | A pattern of time values in ascending order and without duplicates,
-- and wrapped inside the interval [0,1) with 1 is excluded.
-- In effect, this normalizes cyclic time.
stdForm :: Pattern Time -> Pattern Time
stdForm = sort . nub . map modOne

startPosition :: (Eq a, Monoid a) => Pattern a -> Pattern a
startPosition [] = []
startPosition pttrn@(x:xs)
  | null (reduceEmpty pttrn) = []
  | x == mempty = startPosition $ rotateLeft 1 pttrn
  | otherwise = pttrn

-- | Steps away from the first onset
position :: (Eq a, Monoid a) => Pattern a -> Int
position xs
  | null xs = 0
  | let [x] = take 1 xs in x /= mempty = 0
  | otherwise = 1 + position (drop 1 xs)

reduceEmpty :: (Eq a, Monoid a) => Pattern a -> Pattern a
reduceEmpty []           = []
reduceEmpty pttrn@(x:xs) = if x == mempty then reduceEmpty xs else pttrn
