-- | Tiled streams and tiled product based on
-- Janin, David, Florent Berthaut, Myriam Desainte-Catherine, Yann Orlarey,
-- and Sylvain Salvati. 2013. “The T-Calculus: Towards a Structured Programing
-- of (Musical) Time and Space.” In Proceedings of the First ACM SIGPLAN Workshop
-- on Functional Art, Music, Modeling & Design, 23–34.
-- Boston Massachusetts USA: ACM. https://doi.org/10.1145/2505341.2505347.

module Sound.RTG.TiledStreams where

import Sound.RTG.Music (Pitch, NoteName(..) )
import Data.List (foldl')
import Data.Monoid (Sum(..))

-- | Tiled streams are bi-infinite sequences indexed by integers,
-- paremetrized by a type representing musical values.
-- The index represents the date of the musical value as an event
-- so that each step will be interpreted as a fixed relative duration.
data TiledStream a = TiledStream { active :: RealizationInterval , sync :: SyncInterval , stream :: Stream a }

-- | Abstracts the notion of a musical bar
type SyncInterval = Int

-- | Represents a range where the non-zero part of a stream lies.
-- Introduced to allow recursive TiledStream definitions.
-- It is such that the sync interval [0,d] is constained within it.
type RealizationInterval = (Int,Int)

-- | A stream using integers as relative dates (time abstration)
type Stream a =  Int -> a

-- | Definition of the tiled product, which generalizes both sequential
-- and parallel composition.
instance Semigroup a => Semigroup (TiledStream a) where
  t1 <> t2 = merge $ (+*) t1 t2

instance Monoid a => Monoid (TiledStream a) where
  mempty = TiledStream { active = (0,0)
                       , sync = 0
                       , stream = const mempty }

-- Pitch lists have a standard monoid instance, so they can be lifted
type Pitches = [Pitch]

-- | The tiled product for spatio-temporal combination of tiled streams
(+*) :: TiledStream a -> TiledStream b -> TiledStream (a,b)
t1 +* t2 = 
  TiledStream { active = productRealization t1 t2 
              , sync   = sync t1 + sync t2
              , stream = \k -> ( stream t1 k , stream t2 $ k - sync t1 ) }

-- General operations

-- TODO: define higher order polyvariadic function
-- for mapping construct
-- https://wiki.haskell.org/Varargs

-- | Accumulate arguments of different type
class StreamMapping m where
  f :: TiledStream a -> TiledStream b -> m

-- | Base case instance for result type
instance StreamMapping (TiledStream t) where
  f x y = TiledStream { active = productRealization x y
                      , sync   = sync x + sync y
                      , stream = stream $ f x y }

instance Functor TiledStream where
  fmap f t = t { stream = \k -> let x = stream t k in f x }

-- | Recursive step instance for extending arguments
instance StreamMapping r => StreamMapping (c -> r) where
  f x y = f (f x y) z

-- Basic operations

productRealization :: TiledStream a -> TiledStream b -> RealizationInterval
productRealization t1 t2 = let (a1,a2)= active t1; (b1,b2) = active t2
                    in (min a1 (a2 + sync t1), max a2 (b2 + sync t1))

merge :: Semigroup m => TiledStream (m,m) -> TiledStream m
-- In applicative style, the new stream is defined by
-- doing a point-wise semigroup operation of each component stream.
merge t = t { stream = (<>) <$> stream (projection1 t) <*> stream (projection2 t) }

-- | The projection on the first entry lifted to a tiled stream
projection1 :: TiledStream (a,b) -> TiledStream a
projection1 = fmap fst

-- | The projection on the second entry lifted to a tiled stream
projection2 :: TiledStream (a,b) -> TiledStream b
projection2 = fmap snd

fork :: TiledStream a -> TiledStream b -> TiledStream (a,b)
fork t1 t2 =  reset t1 +* t2

join :: TiledStream a -> TiledStream b -> TiledStream (a,b)
join t1 t2 = t1 +* coReset t2

reset :: TiledStream a -> TiledStream a
reset t = t { sync = 0 }

coReset :: TiledStream a -> TiledStream a
coReset t = t { sync   = 0
              , stream = \k -> stream t $ k + sync t}

-- | The monoid constraint give us means to combine values
-- and set the stream to the default (silent) value
-- outside the realization interval
liftVal :: Monoid a => a -> TiledStream a
liftVal x = TiledStream { active = (0,1)
                        , sync   = 1
                        , stream = \k -> if k == 0 then x else mempty }

query :: TiledStream a -> [(a,String)]
query t = [(stream t k, "Index:" ++ show k) | let (a,b) = active t, k <- [a..b]]

-- Test examples
x,y,z :: TiledStream Pitches
x = liftVal [(C,4)] 
y = liftVal [(D,5)]
z = liftVal [(G,4)]



