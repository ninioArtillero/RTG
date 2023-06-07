module Sound.RTG.Geometria.Euclidean where

type Euclidean =
  ( Int, -- pulsos
    Int, -- eventos
    Int  -- rotación
  )

-- La siguiente función auxiliar reduce la representación de un ritmo
-- de manera que para [(x,y,z) | x >=0, y >= 0, z >= 0] se cumpla
-- (1) x <= y
-- (2) z < y
-- La cantidad de eventos, representada por y, permanece inalterada,
-- a pesar de que el ritmo pueda ser representado en menos eventos,
-- para preservar todas las posibles rotaciones.
reduceR :: Euclidean -> Euclidean
reduceR (x, y, z) = (x', y', z')
  where
    x' =
      if x `mod` y == 0
        then y -- ritmos isócronos
        else x `mod` y
    y' = y
    z' = z `mod` y

-- Notemos que ritmos euclideanos que suenan igual pueden tener distinta representación
-- Una familia muy fácil de caracterizar es la de los ritmos isócronos:
-- (3,3), (3,6), (3,9), etc.
-- (5,5), (5,10), (5,15), etc.

-- TODO: reescribir usando guards
(>+<) :: Euclidean -> Euclidean -> Euclidean -- type declaration
(x, y, z) >+< (a, b, c) =
  -- function definition
  if (y /= 0) && (b /= 0)
    then -- output expression:

      ( (x + a) `mod` w, -- pulses
        w, -- events
        (z' + c') `mod` w -- rotation
      )
    else error "Zero event argument not allowed"
  where
    w = lcm y b
    z' = z * div w y
    c' = c * div w b

-- Es necesaria una representación homegénea de estructura rítmica para poder
-- aplicar un ritmo euclideano (o cualquier otro) como transformación genérica.

-- composeR :: Euclid -> Euclid -> Euclid
-- composeR = (foldl1 composeL) . (map reduceR) . (\x y -> [x,y]) -- Este patron se ve común, es instancia de qué?
