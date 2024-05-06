{-# LANGUAGE InstanceSigs #-}
module Sound.RTG.Ritmo.RhythmicPattern where
{-|
Module      : RhythmicPattern
Description : Main data type and its API helper functions
Copyright   : (c) Xavier Góngora, 2024
License     : GPL-3
Maintainer  : ixbalanque@protonmail.ch
Stability   : experimental

Rhythmic patterns are wrapped patterns with aditional structure.
-}

import           Data.Group                     (Group, invert)
import           Data.List                      (group, sort)
import qualified Sound.RTG.Ritmo.Pattern        as P
import           Sound.RTG.Ritmo.PerfectBalance (indicatorVector)

-- | This data type represents integers modulo 2
data Binary = Zero | One deriving (Eq, Ord, Enum, Bounded)

instance Show Binary where
  show :: Binary -> String
  show Zero = show 0
  show One  = show 1

instance Semigroup Binary where
  (<>) :: Binary -> Binary -> Binary
  Zero <> One  = One
  One  <> Zero = One
  _    <> _    = Zero

instance Monoid Binary where
  mempty :: Binary
  mempty = Zero

instance Group Binary where
  invert :: Binary -> Binary
  invert  = id

-- | Onset patterns are represented by binary valued lists
-- so that group structure can de lifted.
type OnsetPattern = P.Pattern Binary

-- | Clusters are groupings of pattern onsets generated by the
-- mutual nearest-neighbor graph (MNNG).
type OnsetClusters = [OnsetPattern]

-- | Meter carries musical context information
-- related to a patterns underlying pulse.
type Meter = Int

newtype Sign = Sign { sign :: Int } deriving (Show,Eq)

-- | This data type encondes a rhythmic pattern along with
-- other structure related to rhythm perception.
data Rhythmic = Rhythm {
                        pttrn       :: OnsetPattern,
                        clusters    :: OnsetClusters,
                        meter       :: !Meter,
                        orientation :: !Sign
                       } deriving (Eq,Show)

instance Semigroup Rhythmic where
  (<>) :: Rhythmic -> Rhythmic -> Rhythmic
  (Rhythm pttrn1 clusters1 meter1 s1) <> (Rhythm pttrn2 clusters2 meter2 s2) =
    let sign1 = sign s1
        sign2 = sign s2
        direction | sign1 == 0 = sign2
                  | sign2 == 0 = sign1
                  | otherwise = sign1 * sign2
        -- mtr = if orientation == 1 then meter1 + meter2 else max meter1 meter2
        signedMeter = sign1 * meter1 + sign2 * meter2
        meterDiff = abs (meter1 - meter2)
    in Rhythm {
      pttrn =  case direction of
          0  -> []
          1  -> pttrn1 ++ pttrn2
          -1 ->
            -- Alternativa haciendo zip sobre el final de la lista
            -- case (sign1 > 0, compare meter1 meter2) of
            --   (True, GT) -> drop meterDiff $ zipWith (<>) pttrn1 ( replicate meterDiff Zero ++ pttrn2)
            --   (False, GT) -> drop meterDiff $ zipWith (<>) pttrn1 ( replicate meterDiff Zero ++ pttrn2)
            --   (True, LT) -> drop meterDiff $ zipWith (<>) (replicate meterDiff Zero ++ pttrn1) pttrn2
            --   (False, LT) -> drop meterDiff $ zipWith (<>) (replicate meterDiff Zero ++ pttrn1) pttrn2,
            reduceEmpty $ zipWith (<>) pttrn1 pttrn2 ++ P.diff pttrn1 pttrn2,
      clusters = clusters1 ++ clusters2,
      meter = abs signedMeter,
      orientation = Sign $ signum signedMeter
      }

instance Monoid Rhythmic where
  mempty :: Rhythmic
  mempty = toRhythm []

instance Group Rhythmic where
  invert :: Rhythmic -> Rhythmic
  invert = inv

inv :: Rhythmic -> Rhythmic
inv (Rhythm pttrn clusters meter (Sign n)) = Rhythm pttrn clusters meter (Sign (-n))

inv' :: Rhythmic -> Rhythmic
inv' (Rhythm pttrn clusters meter sign) = Rhythm (reverse pttrn) clusters meter sign

inv'' :: Rhythmic -> Rhythmic
inv'' (Rhythm pttrn clusters meter (Sign n)) = Rhythm (reverse pttrn) clusters meter (Sign (-n))

toOnset :: Integral a => P.Pattern a -> OnsetPattern
toOnset = map (\n -> if (== 0) . (`mod` 2) $ n then Zero else One)

toInts :: OnsetPattern -> P.Pattern Int
toInts = let toInt x = case x of Zero -> 0; One -> 1
         in map toInt

toRhythm :: P.Pattern P.Time -> Rhythmic
toRhythm xs
  | null xs = Rhythm {
               pttrn = [],
               clusters = [],
               meter = 0,
               orientation = Sign 0
              }
  | reverse xs == sort xs = Rhythm {
      -- Negative rhythms are represented by decreasing patterns
               pttrn = p,
               clusters = mutualNNG p,
               meter = length p,
               orientation = Sign (-1)
              }
  | otherwise = Rhythm {
               pttrn = p,
               clusters = mutualNNG p,
               meter = length p,
               orientation = Sign 1
              }
  where p = toOnset (indicatorVector xs)

makeRhythm :: Int -> P.Pattern Int -> Rhythmic
makeRhythm n ns = Rhythm {
  pttrn = p,
  clusters = mutualNNG p,
  meter = length p,
  orientation = s
  }
  where
    p = toOnset ns
    s= Sign (signum n)

mutualNNG :: OnsetPattern -> OnsetClusters
mutualNNG xs = []

-- | Compute the Inter-Onset-Interval of an onset pattern
iois :: OnsetPattern -> [Int]
iois xs =
  let intervals = group $ drop 1 $ scanl (\acc x -> if x == One then x:acc else acc) [] $ startPosition xs
  in map length intervals

startPosition :: OnsetPattern -> OnsetPattern
startPosition [] = []
startPosition pttrn@(x:xs)
  | null (reduceEmpty pttrn) = []
  | x == Zero = startPosition $ P.rotateLeft 1 pttrn
  | otherwise = pttrn

-- | Steps away from the first onset
position :: OnsetPattern -> Int
position xs
  | null (reduceEmpty xs) = 0
  | take 1 xs == [One] = 0
  | otherwise = position (drop 1 xs) + 1


reduceEmpty :: OnsetPattern -> OnsetPattern
reduceEmpty []           = []
reduceEmpty pttrn@(x:xs) = if x == Zero then reduceEmpty xs else pttrn

-- | Toussaint's six distinguished rhythms
clave = toRhythm P.clave
rumba = toRhythm P.rumba
gahu = toRhythm P.gahu
shiko = toRhythm P.shiko
bossa = toRhythm P.bossa
soukous = toRhythm P.soukous

unitTestRhythmic :: IO ()
unitTestRhythmic =
  do
    putStrLn $ "Identity: " ++ show (rumba <> mempty == rumba &&
      mempty <> rumba == rumba)
    putStrLn $ "Inverses: " ++ show ((clave <> invert clave == mempty) &&
      (invert clave <> clave == mempty))
    putStrLn $ "Associativity: " ++ show (((soukous <> clave) <> gahu == soukous <> (clave <> gahu)) && ((invert clave <> clave) <> gahu == invert clave <> (clave <> gahu)))
