-- | Utility functions

module Sound.RTG.Internal.Utils where

import qualified Data.Set as Set

setNub :: Ord a => [a] -> [a]
setNub = Set.toAscList . Set.fromList
