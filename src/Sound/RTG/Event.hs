-- |
-- Module      : Event
-- Description : Event types and functions.
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG.Event
  ( Event (..),
    isOnset,
    swapEvent,
    fixOnset,
    integralsToEvents,
    eventsToInts,
    ioisToEvents,
    onsetCount,
    zipValuesWithOnsets,
    matchValuesWithOnsets,
  )
where

import Data.Group (Group (..))

-- | Events are either onsets or rests and are modeled after integers modulo 2.
data Event = Rest | Onset deriving (Eq, Ord, Enum, Bounded)

isOnset :: Event -> Bool
isOnset Onset = True
isOnset Rest = False

instance Show Event where
  show Rest = show 0
  show Onset = show 1

instance Semigroup Event where
  Rest <> Onset = Onset
  Onset <> Rest = Onset
  _ <> _ = Rest

instance Monoid Event where
  mempty = Rest

instance Group Event where
  invert = id

swapEvent :: Event -> Event
swapEvent event = if event == Onset then Rest else Onset

fixOnset :: Event -> Event -> Event
fixOnset x y = if x == Onset then Onset else y

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

-- | Pair each 'Onset' with the corresponding value from a list in sequence,
-- wrapping if necessary. 'Rest's get 'Nothing'.
-- TODO: rename, zip might be misleading
zipValuesWithOnsets :: [Event] -> [a] -> [(Event, Maybe a)]
zipValuesWithOnsets [] _ = []
zipValuesWithOnsets events [] = zip events (repeat Nothing)
zipValuesWithOnsets (x : xs) values@(y : ys) = case x of
  Rest -> (Rest, Nothing) : zipValuesWithOnsets xs (drop 1 . cycle $ values)
  Onset -> (Onset, Just y) : zipValuesWithOnsets xs (drop 1 . cycle $ values)

-- | Arrange a value list to match a rhyhtmic pattern.
-- Values wrap to match every onset.
matchValuesWithOnsets :: [Event] -> [a] -> [Maybe a]
matchValuesWithOnsets [] _ = []
matchValuesWithOnsets events [] = map (const Nothing) events
matchValuesWithOnsets (x : xs) values@(y : ys) =
  if x == Onset
    then Just y : matchValuesWithOnsets xs (drop 1 . cycle $ values)
    else Nothing : matchValuesWithOnsets xs (drop 1 . cycle $ values)
