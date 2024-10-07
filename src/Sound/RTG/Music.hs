-- | Music as polymorphic temporal media based on
-- Hudak, Paul. 2003. “An Algebraic Theory of Polymorphic Temporal Media.”
-- Research Report RR-1259. New Haven, CT: Yale University.
-- https://www.cs.yale.edu/homes/hudak-paul/CS431F06/ptm-tr-7-03.pdf.


module Sound.RTG.Music where

data Media a = Prim a | Media a :+: Media a | Media a :=: Media a deriving Show

data Note = Rest Dur | Note Pitch Dur

type Dur = Double
type Pitch = (NoteName, Octave)
type Octave = Int
data NoteName = Cf | C | Cs | Df | D | Ds | Ef | E | Es | Ff
              | F | Fs | Gf | G | Gs | Af | A | As | Bf | B | Bs deriving (Eq, Ord, Show)

type Music = Media Note

cf,c,cs,df,d,ds,ef,e,es,ff,f,fs,gf,g,gs,af,a,as,bf,b,bs :: Int -> Pitch
cf n = (Cf,n); c n = (C,n); cs n = (Cs,n)
df n = (Df,n); d n = (D,n); ds n = (Ds,n)
ef n = (Ef,n); e n = (E,n); es n = (Es,n)
ff n = (Ff,n); f n = (F,n); fs n = (Fs,n)
gf n = (Gf,n); g n = (G,n); gs n = (Gs,n)
af n = (Af,n); a n = (A,n); as n = (As,n)
bf n = (Bf,n); b n = (B,n); bs n = (Bs,n)
