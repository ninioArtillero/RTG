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
    pairValuesWithOnsets,
    matchValuesToOnsets,
    eventsToTimePattern,
  )
where

import Data.Group (Group (..))
import Data.List (findIndices, foldl')

-- | Events are either onsets or rests.
data Event = Rest | Onset deriving (Eq, Ord, Enum, Bounded)

isOnset :: Event -> Bool
isOnset Onset = True
isOnset Rest = False

instance Show Event where
  show Rest = show 0
  show Onset = show 1

-- | Isomorphic to 'Bool' with  '||'.
-- An 'invert' operation no longer fulfills group properties,
-- but /lifts/ into a /superposition/ (or union) of patterns,
-- which is a reasonable default pattern operation.
-- If the operation is isomorphic to integers modulo 2, we get a proper
-- group instance with the 'id' as 'invert'. This /lifts/ into
-- the complement of the intersection of patterns.
instance Semigroup Event where
  Onset <> Onset = Onset
  Onset <> Rest = Onset
  Rest <> Onset = Onset
  Rest <> Rest = Rest

instance Monoid Event where
  mempty = Rest

-- | TODO: Remove instance if using '||' operation. Group properties no longer hold.
instance Group Event where
  invert = id

swapEvent :: Event -> Event
swapEvent event = if event == Onset then Rest else Onset

fixOnset :: Event -> Event -> Event
fixOnset x y = if x == Onset then Onset else y

-- * Conversion

-- | Convert by congruence modulo 2.
integralsToEvents :: (Integral a) => [a] -> [Event]
integralsToEvents = map (\n -> if (== 0) . (`mod` 2) $ n then Rest else Onset)

-- | 'Onset' is converted to 1 and 'Rest' to 0.
eventsToInts :: (Num a) => [Event] -> [a]
eventsToInts =
  let toInt x = case x of Rest -> 0; Onset -> 1
   in map toInt

ioisToEvents :: [Int] -> [Event]
ioisToEvents =
  foldr
    ( \x acc ->
        if x > 0
          then (Onset : replicate (x - 1) Rest) ++ acc
          else error "Sound.RTG.Event.ioisToEvents: There was a non-positive IOI"
    )
    []

-- | Covert a rhythmic pattern into a time pattern.
eventsToTimePattern :: (Eq a, Monoid a) => [a] -> [Rational]
eventsToTimePattern events = map (/ eventCount) onsetIndexes
  where
    eventCount = fromIntegral $ length events
    onsetIndexes = map fromIntegral $ findIndices (/= mempty) events

onsetCount :: (Num a) => [Event] -> a
onsetCount = foldl' (\acc x -> case x of Rest -> acc; Onset -> acc + 1) 0

-- | Pair each 'Onset' with the corresponding value from a list in sequence,
-- wrapping said list if necessary. A 'Rest' gets 'Nothing'.
pairValuesWithOnsets :: [Event] -> [a] -> [(Event, Maybe a)]
pairValuesWithOnsets [] _ = []
pairValuesWithOnsets events [] = zip events (repeat Nothing)
pairValuesWithOnsets events values = zip events arrangedValues
  where
    arrangedValues = matchValuesToOnsets events values

-- | Arrange a value list to match a rhyhtmic pattern.
-- Values wrap if necessary to match every onset.
matchValuesToOnsets :: [Event] -> [a] -> [Maybe a]
matchValuesToOnsets [] _ = []
matchValuesToOnsets events [] = map (const Nothing) events
matchValuesToOnsets (x : xs) values@(y : ys) =
  if isOnset x
    then Just y : matchValuesToOnsets xs (drop 1 . cycle $ values)
    else Nothing : matchValuesToOnsets xs (cycle $ values)
