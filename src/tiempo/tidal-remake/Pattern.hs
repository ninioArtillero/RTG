-- Alex McLean reelabora Tidal desde cero, enfocándose sobre
-- los tipos de datos que determinan su diseño.
-- https://www.youtube.com/live/F2-evGtBnqQ?feature=share

-- Activar extensión del compilador para derivar (es decir, producir de manera
-- automática) instancias de la clase Functor.
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html
{-# LANGUAGE DeriveFunctor #-}

module Pattern where

import           Data.Fixed (mod')
import           Data.Ratio

-- TimeSpan
data Span = Span { begin :: Rational, end :: Rational}
  deriving (Show)
-- > Span {begin = 0 , end = 1}

-- Un evento se corresponde a un valor activo por una
-- cierta cantidad de timepo (Span)
data Event a = Event {active :: Span, value :: a}
  deriving (Show, Functor)
-- > Event{ active = Span {begin = 0 , end = 1}, value = "hola"}

-- Representación de un patrón: una función del tiempo a eventos
data Pattern a = Pattern {query :: Span -> [Event a]}
  deriving (Functor)

-- Ejemplo
-- Se implementa una diente de sierra asignando al evento
-- el valor intermedio entre los extremos del TimeSpan
saw :: Pattern Rational
saw = Pattern {query = f}
  where f (Span b e) = [Event
                        (Span b e)
                        (mod' (b + (e - b)/2) 1) -- módulo para acotar los valores
                       ]
-- > query saw (Span 0 1)
-- > query saw (Span 1 2)

-- Esto se puede generalizar así:
signal :: (Rational -> a) -> Pattern a
signal timeFunc = Pattern {query = f}
  where f (Span b e) = [Event (Span b e) (timeFunc (b + (e - b)/2))]
-- La función de tiempo se evalua en el punto medio del Span: es ahí donde
-- la señal continua (en este caso saw) es sampleada.

-- Así que podemos definir saw así:
saw' :: Pattern Rational
saw' = signal (\t -> mod' t 1)

-- saw va de 0 a 1. Para crear una versión polar:
-- Para esta definición usamos la instancia de functor que se derivó para
-- Pattern a, de manera que podamos aplicarle funciones a sus valores.
toPolar :: Num a => Pattern a -> Pattern a
toPolar pat = fmap (\v -> 2*v - 1) pat

saw2 :: Pattern Rational
saw2 = toPolar saw
-- > query saw2 (Span 0.25 0.25)
-- > query saw2 (Span 0 0)

-- Señal constante de un valor arbitrario
steady :: a -> Pattern a
steady val = signal (const val) -- la función const ignora su segundo argumento.
-- > query (steady "hello") (Span 0 0)
-- > query (steady "hello") (Span 0 1)

-- El silencio produce, como eventos, una lista vacía.
silence :: Pattern a
silence = Pattern (\_ -> [])
-- > query silence (Span x y) => []

-- | Determinar el pricipio de un ciclo de tiempo
sam :: Rational -> Rational
sam s = toRational $ floor s

spanCycles :: Span -> [Span]
spanCycles (Span b e)
  | e <= b = []
  | otherwise = []

{-
  if a==b then []
          else (Span a (a+1)) : spanCycles (Span (a+1) b)
-}

-- | Repite un valor discreto una vez por ciclo
--atom :: a -> Pattern a
--atom v
