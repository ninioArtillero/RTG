{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : RhythmicPattern
-- Description : Main rhythm data type and transformations
-- Copyright   : (c) Xavier Góngora, 2024
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- A 'RhythmicPattern' is a event list in a newtype wrapper.
-- Types with a 'Rhythmic' instance can be converted to a 'RhythmicPattern'.
module Sound.RTG.RhythmicPattern (Rhythmic (..), Rhythm (..), Event (..), rhythm) where

import Data.Group (Group, invert)
import Sound.RTG.Zip (euclideanZip)

-- | This data type represents integers modulo 2
data Event = Rest | Onset deriving (Eq, Ord, Enum, Bounded)

-- TODO: Create Event module with Conversion functions?

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

-- | Rhythm wrapper to define a new custom instances for lists
newtype Rhythm a = Rhythm {getRhythm :: [a]} deriving (Eq, Show, Functor)

-- | Two general posibilities for the applicative instances: ZipList or regular list
instance Applicative Rhythm where
  pure xs = Rhythm $ pure xs
  Rhythm fs <*> Rhythm xs = Rhythm (zipWith ($) fs xs) -- test

-- TODO: DEFINIR MONADA... QUIZAS AÑADIR COMO ESTADO EL METER

-- TODO: Falta lograr propiedad de inversos.
-- Tal propiedad implicaría que dados dos ritmos cualesquiera r1 y r2
-- entonces existe x de forma que r1 <> x == r2 es True
instance (Semigroup a) => Semigroup (Rhythm a) where
  Rhythm pttrn1 <> Rhythm pttrn2 = Rhythm $ pttrn1 `euclideanZip` pttrn2

-- TODO: ¿Lista vacía, relación de equivalencia o lista infinita?
-- Depende de la operación. Depende de la operación.
instance (Semigroup a, Monoid a) => Monoid (Rhythm a) where
  mempty = Rhythm []

instance (Semigroup a, Monoid a, Group a) => Group (Rhythm a) where
  invert = fmap invert

type RhythmicPattern = Rhythm Event

-- | Clusters are groupings of pattern onsets generated by the
-- mutual nearest-neighbor graph (MNNG).
type OnsetClusters = [Rhythm Event]

-- | Meter carries musical context information
-- related to a patterns underlying pulse.
type Meter = Int

-- | The interface for rhythmic pattern types.
-- It lifts instances to rhythmic patterns.
class (Semigroup a, Monoid a, Group a) => Rhythmic a where
  -- | Minimal complete definition
  toRhythm :: a -> RhythmicPattern

  -- | Inverses
  --
  -- prop> x & inv x = mempty
  --
  -- prop> inv x & x = mempty
  inv :: a -> RhythmicPattern
  inv = toRhythm . invert

  -- | Default group operation
  (&) :: (Rhythmic b) => a -> b -> RhythmicPattern
  x & y = toRhythm x <> toRhythm y

  -- | Complement. Exchange Onsets and Rests (Onset and Rest).
  --
  -- prop> (x & co x) = toRhythm $ replicate (length x) Onset
  --
  -- prop> co (co x) = toRhythm x
  co :: a -> RhythmicPattern
  co x =
    let rhythm = toRhythm x
     in fmap (\case Rest -> Onset; Onset -> Rest) rhythm

  -- | Reverse. Play pattern backwards, different from Inverse.
  --
  -- prop> rev (rev x) = toRhythm x
  rev :: a -> RhythmicPattern
  rev x =
    let Rhythm xs = toRhythm x
     in Rhythm $ reverse xs

  -- | Sequence. Plays each pattern every other cycle.
  -- TODO: needs to account for cycle/cycle speed
  (|>) :: (Rhythmic b) => a -> b -> RhythmicPattern
  r1 |> r2 = Rhythm $ (getRhythm . toRhythm) r1 ++ (getRhythm . toRhythm) r2

  -- | Add up
  --
  -- prop> x <+> x = x
  --
  -- prop> x <+> co x = toRhythm $ replicate (length x) Onset
  (<+>) :: (Rhythmic b) => a -> b -> RhythmicPattern
  r1 <+> r2 = fixOnset <$> toRhythm r1 <*> toRhythm r2
    where
      fixOnset x y = if x == Onset then Onset else y

-- TODO
--
-- ¿Paralellization of patterns? Would depend on a implementation of concurrent streams.
--
-- Interpolate. Continuous transformation of patterns.
-- (/\)
--
-- Diverge. Interpolate into complement.
-- (\/) = (/\) . co

infixr 5 &

infixr 5 |>

infixl 5 <+>

-- TODO: ¿can euclidean rhythm generate all rhythms?
-- Euclidean rhythms generalize isochronous rhythms and evenly spacing. This might be enough.
-- And in this way rhythm generation might be abstracted.
-- Check this ideas after reading Toussaint chapters 20 and 21

-- | Access the binary pattern underlying a rhythmic type
rhythm :: (Rhythmic a) => a -> [Event]
rhythm = getRhythm . toRhythm

instance Rhythmic RhythmicPattern where
  toRhythm = id

-- instance Integral a => Rhythmic [a] where
--   toRhythm = Rhythm . integralToOnset
