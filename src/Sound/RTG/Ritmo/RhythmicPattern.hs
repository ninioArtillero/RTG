{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.List                      as List
import           Sound.RTG.Geometria.Euclidean
import           Sound.RTG.Ritmo.Bjorklund      (euclideanPattern)
import qualified Sound.RTG.Ritmo.Pattern        as P
import           Sound.RTG.Ritmo.PerfectBalance (indicatorVector)

-- | This data type represents integers modulo 2
data Binary = Zero | One deriving (Eq, Ord, Enum, Bounded)

instance Show Binary where
  show Zero = show 0
  show One  = show 1

instance Semigroup Binary where
  Zero <> One  = One
  One  <> Zero = One
  _    <> _    = Zero

instance Monoid Binary where
  mempty = Zero

instance Group Binary where
  invert  = id

-- | Onset patterns are represented by binary valued lists
-- so that group structure can de lifted.
newtype Rhythm a = Rhythm {getRhythm :: P.Pattern a} deriving (Eq,Show)

instance Functor Rhythm where
  fmap f (Rhythm xs) = Rhythm (fmap f xs)

-- | Two general posibilities for the applicative instances: ZipList or regular list
instance Applicative Rhythm where
  pure xs = Rhythm $ pure xs
  Rhythm fs <*> Rhythm xs = Rhythm (zipWith ($) fs xs) --test

-- DEFINIR MONADA... QUIZAS AÑADIR COMO ESTADO EL METER

type RhythmicPattern = Rhythm Binary

-- | Clusters are groupings of pattern onsets generated by the
-- mutual nearest-neighbor graph (MNNG).
type OnsetClusters = [Rhythm Binary]

-- | Meter carries musical context information
-- related to a patterns underlying pulse.
type Meter = Int

-- | An interface to use diferent group operations
class Semigroup a => Rhythmic a where
  toRhythm :: a -> RhythmicPattern
  (<°>) :: Rhythmic b => a -> b -> RhythmicPattern
  x <°> y = toRhythm x <> toRhythm y
  (<!>) :: a -> a -> RhythmicPattern
  x <!> y = toRhythm (x <> y)


instance Rhythmic Euclidean where
  toRhythm (Euclidean k n p) = Rhythm . integralToOnset . P.rotateLeft p $ euclideanPattern k n

instance Rhythmic RhythmicPattern where
  toRhythm = id

instance Rhythmic (P.Pattern Binary) where
  toRhythm = Rhythm

-- Falta lograr propiedad de inversos
instance (Eq a, Monoid a) => Semigroup (Rhythm a) where
  Rhythm pttrn1 <> Rhythm pttrn2 = Rhythm $ reduceEmpty $ zipWith (<>) pttrn1 pttrn2 ++ P.diff pttrn1 pttrn2

-- Podría usar una relación de equivalencia
instance Monoid RhythmicPattern where
  mempty = Rhythm []

{-
instance Group a => Group (Rhythm a) where
  invert = inv

-- Main functions

inv :: Group a => Rhythm a -> Rhythm a
inv = Rhythm . map (\x -> if x == Zero then One else Zero) . getRhythm

-}
-- | Computes the mutual nearest neighbor graph for the Rhythmic type cluster field.
-- For example:
-- cluster rumba = [[1,0,0,1], [0,0,0], [1,0,0], [1,0,1], [0,0,0]]
-- TODO Decide what to do with clusters that wrap pass the cycle border
-- For example, bossa has only one cluster:
-- clusters bossa = [[1,0,0,1,0,0,1],[0,0,0],[1,0,0,1,0,0]]
mutualNNG :: P.Pattern Binary -> [P.Pattern Binary]
mutualNNG xs = map (\neighborhood -> if length neighborhood <= 1 then clusterBuilder neighborhood else longClusterBuilder neighborhood) neighborhoods
  where neighborhoods = parseNeighborhoods $ iois xs
        clusterBuilder neighborhood =
          case neighborhood of
            [] -> []
            (n, (b1,b2)) : nbs -> case (b1,b2) of
              (True,True)   -> One : replicate (n-1) Zero ++ [One]
              (True,False)  -> One : replicate (n-1) Zero
              (False,False) -> replicate (n-1) Zero
              (False,True)  -> replicate (n-1) Zero ++ [One]
        longClusterBuilder neighborhood =
          case neighborhood of
            [] -> []
            (n, (b1,b2)) : nbs -> case (b1,b2) of
              (_,True)   -> One : replicate (n-1) Zero ++ [One] ++ longClusterBuilder nbs
              (_,False)  -> One : replicate (n-1) Zero ++ longClusterBuilder nbs


parseNeighborhoods :: [Int] -> [[(Int,(Bool,Bool))]]
-- | Applicative style is used on the input, which means that
-- the rhythmic pattern is evaluated on both functions surrounding @<*>@ before zipping)
parseNeighborhoods bs = map reverse $ parseNeighborhoodsIter (zip <$> id <*> biggerNeighbor $ bs) []

-- | Helper function with an extra parameter to join the intervals of nearest neighbors
parseNeighborhoodsIter :: [(Int, (Bool,Bool))] -> [(Int, (Bool,Bool))] -> [[(Int, (Bool,Bool))]]
parseNeighborhoodsIter [] [] = []
parseNeighborhoodsIter [] xs = [xs]
parseNeighborhoodsIter (m@(int,ns):bs) xs = case ns of
  -- | A local minimum is its own cluster. Avoid passing empty list.
  (True,True) -> if null xs
    then [m] : parseNeighborhoodsIter bs []
    else xs : [m] : parseNeighborhoodsIter bs []
  -- | Start a new cluster without passing empty lists
  (True,False) -> if null xs
    then parseNeighborhoodsIter bs [m]
    else xs : parseNeighborhoodsIter bs [m]
  -- | Add interval to cluster
  (False,False) -> parseNeighborhoodsIter bs (m:xs)
  -- | Close a cluster
  (False,True) -> (m:xs) : parseNeighborhoodsIter bs []

-- | Compare an element's left and right neighbors. True means its bigger.
biggerNeighbor :: [Int] -> [(Bool,Bool)]
biggerNeighbor xs = let leftNeighbors = zipWith (>) (P.rotateRight 1 xs) xs
                        rightNeighbors = zipWith (>) (P.rotateLeft 1 xs) xs
                    in zip leftNeighbors rightNeighbors

-- | Compute the Inter-Onset-Intervals of an onset pattern
iois :: P.Pattern Binary -> [Int]
iois = let intervals = List.group . drop 1 . scanl pickOnsets [] . startPosition
           pickOnsets acc x = if x == One then x:acc else acc
       in map length . intervals

-- Conversion functions

integralToOnset :: Integral a => P.Pattern a -> P.Pattern Binary
integralToOnset = map (\n -> if (== 0) . (`mod` 2) $ n then Zero else One)

toInts :: P.Pattern Binary -> P.Pattern Int
toInts = let toInt x = case x of Zero -> 0; One -> 1
         in map toInt

timeToOnset :: P.Pattern P.Time -> P.Pattern Binary
timeToOnset xs = integralToOnset (indicatorVector xs)

ioisToOnset :: [Int] -> P.Pattern Binary
ioisToOnset = foldr (\x acc -> if x>0 then (One:replicate (x-1) Zero) ++ acc else error "There was a non-positive IOI") []

-- Auxiliary functions

startPosition :: (Eq a, Monoid a) => P.Pattern a -> P.Pattern a
startPosition [] = []
startPosition pttrn@(x:xs)
  | null (reduceEmpty pttrn) = []
  | x == mempty = startPosition $ P.rotateLeft 1 pttrn
  | otherwise = pttrn

-- | Steps away from the first onset
position :: (Eq a, Monoid a) => P.Pattern a -> Int
position xs
  | null xs = 0
  | let [x] = take 1 xs in x /= mempty = 0
  | otherwise = 1 + position (drop 1 xs)


reduceEmpty :: (Eq a, Monoid a) => P.Pattern a -> P.Pattern a
reduceEmpty []           = []
reduceEmpty pttrn@(x:xs) = if x == mempty then reduceEmpty xs else pttrn

-- Toussaint's six distinguished rhythms for examples

clave = timeToOnset P.clave
rumba = timeToOnset P.rumba
gahu = timeToOnset P.gahu
shiko = timeToOnset P.shiko
bossa = timeToOnset P.bossa
soukous = timeToOnset P.soukous

-- TODO: Create module with rhythmic pattern combinators:
-- sequence, parallel, complement, reverse...
