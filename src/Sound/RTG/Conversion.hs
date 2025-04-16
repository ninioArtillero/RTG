-- |

module Sound.RTG.Conversion (integralToOnset, toInts, ioisToOnset, onsetCount) where

import Sound.RTG.RhythmicPattern ( Event (..) )

-- | Convert by congruence modulo 2
integralToOnset :: Integral a => [a] -> [Event]
integralToOnset = map (\n -> if (== 0) . (`mod` 2) $ n then Rest else Onset)

toInts :: [Event] -> [Int]
toInts = let toInt x = case x of Rest -> 0; Onset -> 1
         in map toInt

ioisToOnset :: [Int] -> [Event]
ioisToOnset = foldr (\x acc -> if x>0 then (Onset:replicate (x-1) Rest) ++ acc else error "There was a non-positive IOI") []

onsetCount :: [Event] -> Int
onsetCount = foldl (\acc x -> case x of Rest -> acc; Onset -> acc + 1) 0
