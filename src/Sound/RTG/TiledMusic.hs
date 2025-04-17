-- |
-- Module      : TiledMusic
-- Description : "Tiled Polymorphic Temporal Media" article transcription/implementation
-- Copyright   : (c) Paul Hudak, 2014
--                   David Janin, 2014
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
--
-- Types and operations for music tile composition, using Euterpea library for
-- music types and functions.
-- Most code in this module's comes from the following paper:
--
-- Hudak, Paul, and David Janin. 2014. “Tiled Polymorphic Temporal Media.”
-- In Proceedings of the 2nd ACM SIGPLAN International Workshop on Functional
-- Art, Music, Modeling & Design, 49–60. FARM ’14.
-- Gothenburg, Sweden: Association for Computing Machinery.
-- https://doi.org/10.1145/2633638.2633649.
--
-- A NOTE indicates modifications and additions where appropiate.
--
-- In constrast to previous work by Janin et al. (2013) on the "T-Calculus" (see module 'TiledStream'),
-- here the tiles are not pressumed to be /bi-infinite/ (as streams indexed by integers),
-- but infinite tiles are considered towards the future (positive rationals).
module Sound.RTG.TiledMusic
  ( (%),
    r,
    t,
    re,
    co,
    inv,
    mToT,
    iMToT,
    tempoT,
    liftT,
    tToM,
    playT,
    resync,
    coresync,
    stretch,
    costretch,
    repeatT,
    shiftT,
    iterateT,
    fixT,
    forceSync,
    tileReProd,
  )
where

import Control.Exception (assert)
import Euterpea

-- | A tile is a music value together with /sincronización marks/
-- used in the tiled product '%' to compose them spacetime-wise.
-- @preT@ is the duration before the /logical start/ of the music
-- while @postT@ is the duration until the /logical end/.
-- It is posible for the Music to be non-silent afterwards.
data Tile a = Tile
  { preT :: Dur,
    postT :: Dur,
    musT :: Music a
  }

-- | The /distance/ between the sinchronization marks of a tile.
durT :: Tile a -> Dur
durT (Tile pre post m) = post - pre

-- | The tiled product
(%) :: Tile a -> Tile a -> Tile a
Tile pr1 po1 m1 % Tile pr2 po2 m2 =
  let d = po1 - pr2
   in Tile
        (max pr1 (pr1 - d))
        (max po2 (po2 + d))
        ( if d > 0
            then m1 :=: mDelay d m2
            else mDelay (-d) m1 :=: m2
        )

-- | Auxiliary function to append musical rest when sinchronizing music values.
-- The negative case is not used, but defined (simetrically) to exhaust all cases.
mDelay :: Dur -> Music a -> Music a
mDelay d m = case signum d of
  1 -> rest d :+: m
  0 -> m
  -1 -> m :+: rest (-d)

-- | Create a silent tile with the given duration.
-- NOTE:__Corrected from the paper__.
r :: Dur -> Tile a
r d =
  if d < 0
    then Tile (-d) 0 (rest (-d))
    else Tile 0 d (rest d)

-- | Inverse, Reset and Co-Reset functions are projections from tiles
-- to context tiles (tiles whith duration zero).
-- Reset and Co-Reset are idempotent, meaning that
--
-- prop> re (re t) == re t
-- prop> co (co t) == co t
--
-- Also, inv acts as a /duality operator/ between them:
--
-- prop> re (inv t) == co t
-- prop> co (inv t) == re t
--
-- The original tile can be recovered if its former duration is known.
--
-- prop> t == re t % r (durT t)
-- prop> t == r (durT t) % co t
--
-- The following properties requiere a notion of /observational equivalence/.
--
-- Reset and Co-Reset can be defined in terms of Inverse and the tiled product.
--
-- prop> re t `equiv` t % inv t
-- prop> co t `equiv` inv t % t
--
-- Inverse produces the /semigroup inverse/ of a tile, meaning that
--
-- prop> t % inv t % t `equiv` t
-- prop> inv t % t % inv t `equiv` inv t
--
-- TODO: implement observational equivalence.
-- TODO: create Arbitrary instance for Tile
re, co, inv :: Tile a -> Tile a
re (Tile pre post m) = Tile pre pre m
co (Tile pre post m) = Tile post post m
inv (Tile pre post m) = Tile post pre m

-- | This embedding of finite music values to tiles
-- is a homomorphism from the monoid of music values (with sequential composition)
-- into (one to one, up to observational equivalence) the monoid of musical tiles
-- with the tiled product.
--
-- prop> mToT (m1 :+: m2) `equiv` mToT m1 % mToT m2
mToT :: Music a -> Tile a
mToT m = Tile 0 (dur m) m

-- | This embedding of music values into idempotent tiles
-- is a homomorphism from the monoid of music values (with parallel composition)
-- to the monoid of musical tiles with the tiled product.
--
-- prop> iMToT (m1 :=: m2) `equiv` iMToT m1 % iMToT m2
iMToT :: Music a -> Tile a
iMToT m = Tile 0 0 m

-- | Construct a one note ('Music Pitch') tile from a
-- Euterpea API convinience function.
t :: (Octave -> Dur -> Music Pitch) -> Octave -> Dur -> Tile Pitch
t n o d =
  if d < 0
    then Tile (-d) 0 (n o (-d))
    else Tile 0 d (n o d)

-- | Change tempo (scaling), only for positive values.
tempoT :: Dur -> Tile a -> Tile a
tempoT r (Tile pr po m) =
  assert (r > 0) (Tile (pr / r) (po / r) (tempo r m))

-- | Function lift.
-- TODO: Create a Functor instance?
liftT :: (Music a -> Music b) -> (Tile a -> Tile b)
liftT f (Tile pr po m) = Tile pr po (f m)

-- | Project a tiled music value to a music value
-- taking only the values between the sinchronization marks
-- even when the undelying value is infinite.
-- Also, negative tiles produce no sound.
-- NOTE:__Updated from the paper__ to current Euterpea functions
-- 'cut' (for @takeM@) and 'remove' (for @dropM@).
tToM :: Tile a -> Music a
tToM (Tile pr po m) = cut (po - pr) (remove pr m)

playT :: Tile Pitch -> IO ()
playT = play . tToM

-- Example: "There is no Greater Love" by Isham Jones
pu1 = t a 5 en % t bf 5 en % t c 6 en

bar1'4 = co pu1 % t bf 5 qn % t a 5 qn % t g 5 qn % t d 5 qn % t f 5 qn % t ff 5 qn % t ef 5 qn % t bf 4 qn % t d 5 (wn + qn) % r dhn

pu2 = t d 5 qn % t a 5 qn % t af 5 qn

bar5'6 = co pu2 % t g 5 (wn + qn) % r dhn

pu3 = t g 5 qn % t d 6 qn % t df 6 qn

bar7'8 = co pu3 % t c 6 wn % r wn

tingl = bar1'4 % bar5'6 % bar7'8

-- | The tiled product with the silent tile allows the starting "pick-up" to be played
main :: IO ()
main = playT (r (preT tingl) % tingl)

-- Time transformations

-- | Move post mark by duration. Defines a group action of \( (\mathbb{Q},+,0) \)
-- on the set of tiles.
--
-- prop> resync 0 t == t
-- prop> resync a (resync b t) == resync (a + b) t
resync :: Dur -> Tile a -> Tile a
resync s (Tile pre post m) =
  let npost = post + s
   in if npost < 0
        then Tile (pre - npost) 0 (mDelay (-npost) m)
        else Tile pre npost m

-- | Move pre mark by duration. Defines a group action of \( (\mathbb{Q},+,0) \)
-- on the set of tiles.
--
-- prop> coresync 0 t == t
-- prop> coresync a (coresync b t) = coresync (a + b) t
coresync :: Dur -> Tile a -> Tile a
coresync s (Tile pre post m) =
  let npre = pre + s
   in if npre < 0
        then Tile 0 (post - npre) (mDelay (-npre) m)
        else Tile npre post m

-- | Parallel fork: Sincronize by placing the second tile pre mark
-- at the given duration /after/ the pre mark of the second tile
-- NOTE:__Corrected from the paper__.
--
-- It can derived, up to observation equivalence,
-- from silent tiles, the tiled product and reset function:
--
-- prop> insertT d t1 t2 `equiv` r d % re t2 % r (−d) % t1
insertT :: Dur -> Tile a -> Tile a -> Tile a
insertT d t1 t2 = coresync (-d) (re t2 % coresync d t1)

-- | Parallel join: Sincronize by placing the second tile post mark
-- at the given duration __before__ the post mark of the second tile
-- NOTE:__Corrected from the paper__.
--
-- It can derived, up to observation equivalence,
-- from silent tiles, the tiled product and co-reset function:
--
-- prop> coinsertT d t1 t2 `equiv` t1 % r d % co t2 % r (−d)
coinsertT :: Dur -> Tile a -> Tile a -> Tile a
coinsertT d t1 t2 = resync (-d) (resync d t1 % co t2)

-- | Stretch the underlying music value, preserving the distance between
-- sinchronization marks and the relative position
-- of the pre mark with the music.
-- This functions defines a group actions of \( (\mathbb{Q}^{+},*,1) \)
-- on the set of tiles, so that
--
-- prop> stretch 1 t == t
-- prop> stretch a (stretch b t) == stretch (a*b) t
stretch :: Dur -> Tile a -> Tile a
stretch r (Tile pre post m) =
  assert (r > 0) (Tile (pre * r) (pre * (r - 1) + post) (tempo (1 / r) m))

-- | Stretch the underlying music value, preserving the distance between
-- sinchronization marks and the relative position
-- of the post mark with the music.
-- This functions defines a group actions of \( (\mathbb{Q}^{+},*,1) \)
-- on the set of tiles, so that
--
-- prop> costretch 1 t == t
-- prop> costretch a (costretch b t) == costretch (a*b) t
costretch :: Dur -> Tile a -> Tile a
costretch r (Tile pre post m) =
  assert (r > 0) (Tile (post * (r - 1) + pre) (post * r) (tempo (1 / r) m))

-- Time stretching examples
-- REVIEW: check musical semantics for this rhythms
march = t c 4 qn % r qn % t g 4 qn % r qn

waltz = costretch (2 / 3) march

tumb = costretch (5 / 4) march

bass = liftT (instrument Percussion) (Tile 0 wn (perc AcousticBassDrum wn))

hiHat = liftT (instrument Percussion) (Tile 0 (1 / 8) (perc ClosedHiHat (1 / 8)))

bassL = bass %\ re bassL

hiHatL = repeatT 4 hiHat %\ re hiHatL

percL = re bassL % hiHatL

testW = playT (re bassL % tempoT (3 / 4) (re hiHatL) % repeatT 4 waltz)

testS = playT (re bassL % re hiHatL % repeatT 4 tumb)

repeatT :: Integer -> Tile a -> Tile a
repeatT n t = if n <= 0 then (r 0) else t % repeatT (n - 1) t

-- | Left restricted (tiled) product used for constructing infinite tiles.
-- A prototype of thid way of creating infinite tiles can be found on
-- the article on T-Calculus.
-- TODO: Check how this implementation strategy could fix the infinite tiles
-- on the TiledStream module.
(%\) :: Tile a -> Tile a -> Tile a
Tile pr1 po1 m1 %\ ~(Tile pr2 po2 m2) =
  Tile pr1 po1 (m1 :=: mDelay po1 (remove pr2 m2))

-- Frere Jacques
fj1 = t c 4 en % t d 4 en % t e 4 en % t c 4 en

fj2 = t e 4 en % t f 4 en % t g 4 qn

fj3 = t g 4 sn % t a 4 sn % t g 4 sn % t f 4 sn % t e 4 en % t c 4 en

fj4 = t c 4 en % t g 3 en % t c 4 qn

test1 = playT (re fj1 % fj2)

test2 = playT (fj1 % co fj2)

fj = repeatT 2 fj1 % re (repeatT 2 fj2 % repeatT 2 fj3 % repeatT 2 fj4)

test3 = playT fj

test4 = playT (fj % r 3)

test5 = playT (repeatT 4 fj % r 3)

-- | Shift the whole logic duration of the Tile by given duration.
-- @shiftT s@ is /functorial/ with respect to 're' and 'co'
--
-- prop> shiftT s (t1 % t2) == shiftT s t1 % shiftT s t2
-- prop> shiftT s (re t) == re (shiftT s t)
-- prop> shiftT s (co t) == co (shiftT s t)
--
-- Also, if
-- s >= preT t2 && durT t2 == 0
-- then
-- prop> shiftT (-s) (t1 % t2) == shiftT (-s) t1 %\ shiftT (-s) t2
shiftT :: Dur -> Tile a -> Tile a
-- TODO: resync and coresync conmute, another testing property
shiftT s t = resync s (coresync s t)

-- | Encoding of the iteration of a single tiled producct.
-- It corresponds to the fixpoint @x = t % re x@ for any finite tile @t@.
iterateT :: Tile a -> Tile a
iterateT y =
  let s = preT y
      -- NOTE the recursive call
      x = shiftT s $ shiftT (-s) y %\ shiftT (-s) (re x)
   in x

-- Examples
recT1 = iterateT tumb

testS1 = playT (re percL % recT1 % r 4)

-- | Fixpoint operator for /nice/ tile functions.
-- Used to produce solutions of the equation
-- \
fixT :: (Tile a -> Tile a) -> Tile a
fixT f =
  let pr = preT (f (r 0))
      po = postT (f (r 0))
      y = f (forceSync pr po y)
   in assert (durT y > 0 && pr - preT y >= 0 && po - pr == durT y) y

forceSync :: Dur -> Dur -> Tile a -> Tile a
forceSync npr npo ~(Tile pr po m) =
  if npr < npo
    then inv (forceSync npo npr (Tile po pr m))
    else
      Tile
        npr
        npo
        ( if npr < pr
            then remove (pr - npr) m
            else rest (npr - pr) :+: m
        )

-- | Prefix version of the tiled product with the reset function
-- for currying.
tileReProd :: Tile a -> Tile a -> Tile a
tileReProd t1 t2 = t1 % re t2

-- Examples
recT2 = fixT (tileReProd tumb)

testS2 = playT (re percL % recT2 % r 4)
