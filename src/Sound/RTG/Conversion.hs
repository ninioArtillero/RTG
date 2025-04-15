-- |

--module Sound.RTG.Conversion (integralToOnset, toInts, ioisToOnset, onsetCount) where
module Sound.RTG.Conversion (onsetCount) where

import Sound.RTG.RhythmicPattern ( Binary(..) )
-- Conversion functions

integralToOnset :: Integral a => [a] -> [Binary]
integralToOnset = map (\n -> if (== 0) . (`mod` 2) $ n then Zero else One)

toInts :: [Binary] -> [Int]
toInts = let toInt x = case x of Zero -> 0; One -> 1
         in map toInt

ioisToOnset :: [Int] -> [Binary]
ioisToOnset = foldr (\x acc -> if x>0 then (One:replicate (x-1) Zero) ++ acc else error "There was a non-positive IOI") []

onsetCount :: [Binary] -> Int
onsetCount = foldl (\acc x -> case x of Zero -> acc; One -> acc + 1) 0
