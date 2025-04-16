module Sound.RTG.Conversion (integralsToEvents, eventsToInts, ioisToEvents, onsetCount) where

import Sound.RTG.RhythmicPattern (Event (..))

-- | Convert by congruence modulo 2
integralsToEvents :: (Integral a) => [a] -> [Event]
integralsToEvents = map (\n -> if (== 0) . (`mod` 2) $ n then Rest else Onset)

eventsToInts :: [Event] -> [Int]
eventsToInts =
  let toInt x = case x of Rest -> 0; Onset -> 1
   in map toInt

ioisToEvents :: [Int] -> [Event]
ioisToEvents = foldr (\x acc -> if x > 0 then (Onset : replicate (x - 1) Rest) ++ acc else error "There was a non-positive IOI") []

onsetCount :: [Event] -> Int
onsetCount = foldl' (\acc x -> case x of Rest -> acc; Onset -> acc + 1) 0
