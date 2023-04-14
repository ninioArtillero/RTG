-- | Definición del concepto de Patrón
-- con una librería de patrones predeterminados.
module Pattern where

import Data.List (nub, sort)
import RatioDecons (modOne)

-- | Se utiliza tiempo racional para aprovechar su correlación con el pensamiento musical y
-- para preservar la precisión, postergando los cálculos con flotantes.
type Time = Rational

type Pattern a = [a]

-- | La forma estandar de un patron de tiempo:
-- Valores en [0,1), ordenado ascendemente y sin elementos repetidos.
stdForm :: Pattern Time -> Pattern Time
stdForm = sort . nub . map modOne

-- | Patrones predeterminados.

-- | Escalas en temperamento justo de 12 tonos.
diatonic :: Pattern Time
diatonic = [0 / 12, 2 / 12, 4 / 12, 5 / 12, 7 / 12, 9 / 12, 11 / 12]

diminished :: Pattern Time
diminished = [0 / 12, 2 / 12, 3 / 12, 5 / 12, 6 / 12, 8 / 12, 9 / 12, 11 / 12]

wholeTone :: Pattern Time
wholeTone = [0 / 12, 2 / 12, 4 / 12, 6 / 12, 8 / 12, 10 / 12]

-- | Timelines distinguidos de 5 ataques y 16 pulsos.
-- Ver Cap. 7 en "The Geometry of Musical Rhythm", G. Toussaint.
shiko :: Pattern Time
shiko = [0 / 16, 4 / 16, 6 / 16, 10 / 16, 12 / 16]

clave :: Pattern Time
clave = [0 / 16, 3 / 16, 6 / 16, 10 / 16, 12 / 16]

soukous :: Pattern Time
soukous = [0 / 16, 4 / 16, 6 / 16, 10 / 16, 12 / 16]

rumba :: Pattern Time
rumba = [0 / 16, 3 / 16, 7 / 16, 10 / 16, 12 / 16]

bossa :: Pattern Time
bossa = [0 / 16, 3 / 16, 6 / 16, 10 / 16, 13 / 16]

gahu :: Pattern Time
gahu = [0 / 16, 3 / 16, 6 / 16, 10 / 16, 14 / 16]

-- | Esta patrón es el único perfectamente balanceado,
-- en un universo cromático de 30 vertices, que no se obtiene como la
-- suma de polígonos disjuntos.
amiotScale :: Pattern Time
amiotScale = [0 / 30, 6 / 30, 7 / 30, 13 / 30, 17 / 30, 23 / 30, 24 / 30]

-- | Para pruebas
firstQuart :: Pattern Time
firstQuart = [0 / 16, 1 / 16, 2 / 16, 3 / 16]

crowded :: Pattern Time
crowded = [1, 1, 1, 1]
