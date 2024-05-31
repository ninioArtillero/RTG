module Sound.RTG.Ritmo.Combinators where
import Sound.RTG.Ritmo.RhythmicPattern

-- Combinators for the creation and transformation of rhythmic patterns.
-- Some operations inspired by Laurie Spiegel basic transformations.

-- TODO: ¿Paralellization of patterns? Would depend on the implementation of concurrent streams.

{-

-- | Sequence. Plays each pattern every other cycle. TODO: needs to account for cycle
(|>) :: Rhythmic -> Rhythmic -> Rhythmic
r1 |> r2 =
  let p = pttrn r1 ++ pttrn r2
  in Rhythm {
    pttrn = p,
    clusters = mutualNNG p,
    meter = meter r1 + meter r2,
    orientation = Sign $ sign (orientation  r1) * sign (orientation r2)
    }

-- | Complement. Exchange Onsets and Rests (One and Zero).
co :: Rhythmic -> Rhythmic
co rhythm = rhythm { pttrn = map (\binary -> if binary == Zero then One else Zero) (pttrn rhythm) }

-- | Interpolate. Continuous transformation of patterns.
-- (/\)

-- | Diverge. Interpolate into complement.
-- (\/) = (/\) . co

-- | Reverse. Play pattern backwards, different from Inverse.
rev :: Rhythmic -> Rhythmic
rev rhythm = rhythm {
  pttrn = reverse $ pttrn rhythm,
  clusters = reverse . map reverse $ clusters rhythm
  }

-- | Binary sum
(<+>) :: Rhythmic -> Rhythmic -> Rhythmic
r1 <+> r2 =
  let p = zipWith (<>) (pttrn r1) (pttrn r2)
  in Rhythm {
    pttrn = p,
    clusters = mutualNNG p,
    meter = max (meter r1) (meter r2),
    orientation = case compare (meter r1) (meter r2) of
      GT -> orientation r1
      LT -> orientation r2
      EQ -> if orientation r1 == orientation r2 then orientation r1 else Sign (-1)
    }

-- TODO: ¿can euclidean rhythm generate all rhythms?
-- Euclidean rhythms generalize isochronous rhythms and evenly spacing. This might be enough.
-- And in this way rhythm generation might be abstracted.
-- Check this ideas after reading Toussaint chapters 20 and 21
-}
