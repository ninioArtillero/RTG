-- |
-- Module      : Structure
-- Description : Functions to extract rhythmic pattern structural properties
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.Structure (iois, mnng) where

import qualified Data.List as List
import Sound.RTG.List
  ( rotateLeft,
    rotateRight,
    startPosition,
  )
import Sound.RTG.RhythmicPattern (Rhythmic, rhythm)
import Sound.RTG.Event (Event (..))

-- TODO: This module still depends on Event constructors.
-- Can it be decoupled?

-- | Computes the /mutual nearest neighbor graph/ of an onset pattern.
-- Lists wrap as if embedded in a circle.
-- The result is a list of patterns formed by the clustering of
-- mutually nearest onsets.
-- For example:
--
-- >>> mnng rumba
-- [[1,0,0,1], [0,0,0], [1], [0,0], [1,0,1], [0,0,0]]
--
-- >>> mnng bossa
-- [[1,0,0,1,0,0,1,0,0,1,0,0,1],[0,0,0]]

-- >>> mnng diatonic
-- [[1,1],[0],[1,0,1],[0],[1,1],[0],[1],[0]]
--
-- NOTE: Isochronous rhythms are collapsed to trival pulses (all onset lists)
--
-- >>> mnng wholeTone
-- [1,1,1,1,1,1]
--
-- TODO: formulate properties
-- TODO: optmize recursion
-- TODO: refactor to show intention
mnng :: (Rhythmic a) => a -> [[Event]]
mnng xs = concatMap (\neighborhood -> if length neighborhood <= 1 then clusterBuilder neighborhood else reverse $ longClusterBuilderIter neighborhood [] []) neighborhoods
  where
    neighborhoods = parseNeighborhoods . iois $ xs
    clusterBuilder neighborhood =
      case neighborhood of
        [] -> []
        (n, (c1, c2)) : nbs -> case (c1, c2) of
          (GT, GT) -> [Onset : replicate (n - 1) Rest ++ [Onset]]
          (LT, LT) -> [replicate (n - 1) Rest]
          (GT, LT) -> [[Onset], replicate (n - 1) Rest]
          (LT, GT) -> [replicate (n - 1) Rest, [Onset]]
          -- The only singleton case left is one interval rhythms (EQ,EQ)
          (_, _) -> [[Onset]]
    longClusterBuilderIter [] [] cluster = cluster
    longClusterBuilderIter [] acc cluster = acc : cluster
    longClusterBuilderIter neighborhood acc cluster =
      case neighborhood of
        (n, (c1, c2)) : nbs -> case (c1, c2) of
          (EQ, EQ) ->
            if not (null nbs) && (snd . head) nbs == (EQ, LT)
              then longClusterBuilderIter nbs (acc ++ (Onset : replicate (n - 1) Rest) ++ [Onset]) cluster
              else longClusterBuilderIter nbs (acc ++ (Onset : replicate (n - 1) Rest)) cluster
          (LT, EQ) ->
            if (snd . head) nbs == (EQ, LT)
              then longClusterBuilderIter nbs (acc ++ [Onset]) (replicate (n - 1) Rest : cluster)
              else longClusterBuilderIter nbs acc (replicate (n - 1) Rest : cluster)
          (EQ, LT) ->
            longClusterBuilderIter nbs [] (replicate (n - 1) Rest : acc : cluster)
          (GT, EQ) ->
            longClusterBuilderIter nbs (Onset : replicate (n - 1) Rest) cluster
          (EQ, GT) ->
            longClusterBuilderIter nbs [] ((acc ++ (Onset : replicate (n - 1) Rest ++ [Onset])) : cluster)

-- | A list of pairs where the second value indicates whether
-- its neighbors first values are bigger
type Neighborhood = [(Int, (Ordering, Ordering))]

-- | Takes an IOI pattern and transforms it into a list of neighborhoods
-- joining the intervals of mutual nearest neighbors
parseNeighborhoods :: [Int] -> [Neighborhood]
parseNeighborhoods bs = reverse . map reverse $ parseNeighborhoodsIter (clusterStart . toNeighborhood $ bs) [] []

-- | Look for a starting neighbor for the cluster to avoid cluster wrapping
-- around the list in 'parseNeighborhoods'
clusterStart :: Neighborhood -> Neighborhood
clusterStart [] = []
clusterStart n
  | not $ any ((== GT) . fst . snd) n = n
  | otherwise = lookStart n
  where
    lookStart n = if (fst . snd . head $ n) == GT then n else lookStart $ rotateLeft 1 n

toNeighborhood :: [Int] -> Neighborhood
-- Applicative style is used on the input, which means that
-- the pattern is evaluated on both functions surrounding @<*>@ before zipping
toNeighborhood = zip <$> id <*> compareNeighbor

-- | Compare an element's left and right neighbors. True means its bigger.
compareNeighbor :: [Int] -> [(Ordering, Ordering)]
compareNeighbor xs =
  let leftNeighbors = zipWith compare (rotateRight 1 xs) xs
      rightNeighbors = zipWith compare (rotateLeft 1 xs) xs
   in zip leftNeighbors rightNeighbors

-- | Iterative helper function for 'parseNeighborhoods'
parseNeighborhoodsIter :: Neighborhood -> Neighborhood -> [Neighborhood] -> [Neighborhood]
parseNeighborhoodsIter [] [] neighborhoods = neighborhoods
parseNeighborhoodsIter [] acc neighborhoods = acc : neighborhoods
parseNeighborhoodsIter (n@(_, bs) : ns) acc neighborhoods =
  case bs of
    -- Singleton neighborhoods:
    -- Local minimum
    (GT, GT) ->
      if null acc
        then parseNeighborhoodsIter ns [] ([n] : neighborhoods)
        else parseNeighborhoodsIter ns [] ([n] : acc : neighborhoods)
    -- Local maximum
    (LT, LT) ->
      if null acc
        then parseNeighborhoodsIter ns [] ([n] : neighborhoods)
        else parseNeighborhoodsIter ns [] ([n] : acc : neighborhoods)
    -- Decrement
    (GT, LT) ->
      if null acc
        then parseNeighborhoodsIter ns [] ([n] : neighborhoods)
        else parseNeighborhoodsIter ns [] ([n] : acc : neighborhoods)
    -- Increment
    (LT, GT) ->
      if null acc
        then parseNeighborhoodsIter ns [] ([n] : neighborhoods)
        else parseNeighborhoodsIter ns [] ([n] : acc : neighborhoods)
    -- Composite neighborhoods:
    -- Add interval to cluster
    (EQ, EQ) ->
      parseNeighborhoodsIter ns (n : acc) neighborhoods
    -- Begin cluster
    (_, EQ) ->
      if null acc
        then parseNeighborhoodsIter ns [n] neighborhoods
        else parseNeighborhoodsIter ns [n] (acc : neighborhoods)
    -- End a cluster
    (EQ, _) ->
      parseNeighborhoodsIter ns [] ((n : acc) : neighborhoods)

-- | Compute the Inter-Onset-Intervals of an onset pattern
iois :: (Rhythmic a) => a -> [Int]
-- Intervals are calculated by counting the times scanl doesn't add another onset
iois =
  let intervals = List.group . drop 1 . scanl pickOnsets [] . startPosition
      pickOnsets acc x = if x == Onset then x : acc else acc
   in map length . intervals . rhythm
