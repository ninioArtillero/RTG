-- | Ritmos perfectamente balanceados
-- Basado en Milne et. al. 2015
-- "Perfect Balance: A Novel Principle for the Construction of
-- Musical Scales and Meters".
module PerfectBalance
  ( evenness
  , balance
  ) where

import           Data.Complex (Complex (..), magnitude)
import           Data.Ratio   (denominator, numerator, (%))
import           DynFlags     (GhcNameVersion (ghcNameVersion_programName))
import           Pattern

gcdRational :: Rational -> Rational -> Rational
gcdRational x y =
  gcd (numerator x) (numerator y) % lcm (denominator x) (denominator y)

gcdRationals :: [Rational] -> Rational
gcdRationals = foldr gcdRational 0

-- | Mínima subdivisión regular discreta del intervalo [0,1)
-- que contiene a un patrón.
chromaticUniverse :: Pattern Time -> Pattern Time
chromaticUniverse xs =
  let n = denominator $ gcdRationals xs in [ k % n | k <- [0 .. (n - 1)] ]

-- | Representa un patrón como lista de ceros y unos
-- que denotan, respectivamente, ataques y silencios
-- dentro del universo cromático del patrón.
indicatorVector :: Pattern Time -> [Int]
indicatorVector xs =
  [ if x `elem` stdForm xs then 1 else 0 | x <- chromaticUniverse xs ]

-- | Unidad imaginaria
i :: Complex Double
i = 0 :+ 1

-- | Mapea un patrón en el círculo unitario (del plano Complejo).
scaleVector :: Pattern Time -> [Complex Double]
scaleVector = map (exp . (2 * pi * i *) . fromRational)

-- | Coeficiente t de la Transformada de Fourier Discreta (DFT)
dft :: Int -> [Complex Double] -> Complex Double
dft t zs = sum terms / dimension
 where
  terms =
    map
        (\(n, z) ->
          z * exp
            ((-2) * pi * i * fromIntegral t * (fromIntegral n / dimension))
        )
      $ indexList zs
  dimension = fromIntegral (length zs)


indexList :: [b] -> [(Int, b)]
indexList = zip [0 ..]

-- | La magnitud del primer coeficiente de la DFT
-- mide la paridad de un patrón.
evenness :: Pattern Time -> Double
evenness = magnitude . dft 1 . scaleVector

-- | El balance se define como la diferencia entre 1 y la magnitud
-- del coeficiente 0 de la DFT.
balance :: Pattern Time -> Double
balance = (1 -) . magnitude . dft 0 . scaleVector

-- | Variante utilizando el indicatorVector. Posible optimización.
balance' :: Pattern Time -> Double
balance' pat =
  let indicator   = map fromIntegral $ indicatorVector pat
      elements    = fromIntegral $ length (stdForm pat)
      dimension   = fromIntegral $ length indicator
      scaleFactor = (dimension / elements)
  in  (1 -) . (scaleFactor *) . magnitude . dft 1 $ indicator
