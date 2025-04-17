-- |
-- Module      : TiledStream
-- Description : Tiled stream type and tiled product
-- Copyright   : (c) Xavier Góngora, 2023
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- Tiled streams and tiled product based on the specification found in:
--
-- Janin, David, Florent Berthaut, Myriam Desainte-Catherine, Yann Orlarey,
-- and Sylvain Salvati. 2013. “The T-Calculus: Towards a Structured Programing
-- of (Musical) Time and Space.” In Proceedings of the First ACM SIGPLAN Workshop
-- on Functional Art, Music, Modeling & Design, 23–34.
-- Boston Massachusetts USA: ACM. https://doi.org/10.1145/2505341.2505347.
module Sound.RTG.TiledStream where

import Data.Monoid (Sum (..))
import Euterpea.Music

-- | Tiled streams are bi-infinite sequences indexed by integers,
-- paremetrized by a type representing musical values.
-- The index represents the date of the musical value as an event
-- so that each step will be interpreted as a fixed relative duration.
data TiledStream a = TiledStream {active :: RealizationInterval, sync :: SyncInterval, stream :: Stream a}

-- | Abstracts the notion of a musical bar
type SyncInterval = Int

-- | Represents a range where the non-zero part of a stream lies.
-- It is such that the sync interval [0,d] is constained within it.
-- NOTE: In the paper this are taken to be extended Naturals,
-- i.e. with an Infinite value, and are part of the "sinchronization profile"
-- (l,d,r), which are equivalent here to (-l, d+r)
type RealizationInterval = (Int, Int)

-- | A stream using integers as relative dates (time abstration)
type Stream a = Int -> a

instance Functor TiledStream where
  fmap f t = t {stream = \k -> let x = stream t k in f x}

-- | T-Calculus basic types are uniformly seen as semiring structures
-- with two infix operators analogous to + and *
instance (Semigroup a) => Semigroup (TiledStream a) where
  t1 <> t2 = merge $ t1 +* t2

instance (Monoid a) => Monoid (TiledStream a) where
  mempty =
    TiledStream
      { active = (0, 0),
        sync = 0,
        stream = const mempty
      }

-- T-Calculus operations

-- | The tiled product for spatio-temporal combination of tiled streams,
-- generalizing sequential and parallel composition
(+*) :: TiledStream a -> TiledStream b -> TiledStream (a, b)
t1 +* t2 =
  TiledStream
    { active = productRealization t1 t2,
      sync = sync t1 + sync t2,
      stream = \k -> (stream t1 k, stream t2 $ k - sync t1)
    }

reset :: TiledStream a -> TiledStream a
reset t = t {sync = 0}

coReset :: TiledStream a -> TiledStream a
coReset t =
  t
    { sync = 0,
      stream = \k -> stream t $ k + sync t
    }

merge :: (Semigroup m) => TiledStream (m, m) -> TiledStream m
-- In applicative style, the new stream is defined by
-- doing a point-wise semigroup operation of each component stream.
merge t = t {stream = (<>) <$> stream (projection1 t) <*> stream (projection2 t)}

-- Embeddings

-- | List embedding
--
-- prop> phi (xs ++ ys) == phi xs <> phi ys
phi :: (Monoid a) => [a] -> TiledStream a
phi xs =
  TiledStream
    { active = (0, length xs),
      sync = length xs,
      stream = \k -> if 0 <= k && k < length xs then xs !! k else mempty
    }

-- | Stream embedding
-- NOTE: Streams are usually thought to be defined on the natural numbers
psi :: (Monoid a) => Stream a -> TiledStream a
psi s =
  TiledStream
    { active = (0, 0),
      sync = 0,
      stream = \k -> if 0 < k then s k else mempty
    }

-- General operations

-- TODO: define higher order polyvariadic function
-- for mapping construct
-- https://wiki.haskell.org/Varargs

{-

-- | Accumulate arguments of different type
class StreamMapping m where
  f :: TiledStream a -> TiledStream b -> m

-- | Base case instance for result type
instance StreamMapping (TiledStream t) where
  f x y = TiledStream { active = productRealization x y
                      , sync   = sync x + sync y
                      , stream = stream $ f x y }

-- | Recursive step instance for extending arguments
instance StreamMapping r => StreamMapping (c -> r) where
  f x y c = f (f x y) c

-}

-- Auxiliary tiled stream functions

productRealization :: TiledStream a -> TiledStream b -> RealizationInterval
productRealization t1 t2 =
  let (a1, a2) = active t1; (b1, b2) = active t2
   in (min a1 (a2 + sync t1), max a2 (b2 + sync t1))

-- | The projection on the first entry lifted to a tiled stream
projection1 :: TiledStream (a, b) -> TiledStream a
projection1 = fmap fst

-- | The projection on the second entry lifted to a tiled stream
projection2 :: TiledStream (a, b) -> TiledStream b
projection2 = fmap snd

fork :: TiledStream a -> TiledStream b -> TiledStream (a, b)
fork t1 t2 = reset t1 +* t2

join :: TiledStream a -> TiledStream b -> TiledStream (a, b)
join t1 t2 = t1 +* coReset t2

-- | The monoid constraint give us means to combine values
-- and set the stream to the default (silent) value
-- outside the realization interval
liftVal :: (Monoid a) => a -> TiledStream a
liftVal x =
  TiledStream
    { active = (0, 1),
      sync = 1,
      stream = \k -> if k == 0 then x else mempty
    }

liftInt :: Int -> TiledStream (Sum Int)
liftInt n = liftVal (Sum n)

liftPitch :: Pitch -> TiledStream [Pitch]
liftPitch p = liftVal [p]

queryActive :: TiledStream a -> [(a, String)]
queryActive t = [(stream t k, show k) | let (a, b) = active t, k <- [a .. b]]

query :: Int -> TiledStream a -> [(a, String)]
query n t = [(stream t k, show k) | k <- [-n .. n]]

-- Test examples

-- | Pitch lists have a standard monoid instance, so they can be lifted
type Pitches = [Pitch]

c4, d5, g4 :: TiledStream Pitches
c4 = liftPitch (C, 4)
d5 = liftPitch (D, 5)
g4 = liftPitch (G, 4)

s1 = c4 <> d5 <> g4

repPitchStream p = liftPitch p <> repPitchStream p
