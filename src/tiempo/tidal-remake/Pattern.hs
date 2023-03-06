-- Alex McLean reelabora Tidal desde cero, enfocándose sobre
-- los tipos de datos que determinan su diseño.
-- https://www.youtube.com/live/F2-evGtBnqQ?feature=share

-- Activar extensión del compilador para derivar (es decir, producir de manera
-- automática) instancias de la clase Functor.
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html
{-# LANGUAGE DeriveFunctor #-}

module Pattern where

import           Data.Fixed      (mod')
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes)
import           Data.Ratio
import           Prelude         hiding ((*>), (<*))

-- TimeSpan
data Span = Span { begin :: Rational, end :: Rational}
  deriving (Show)
-- > Span {begin = 0 , end = 1}

-- Un evento se corresponde a un valor activo por una
-- cierta cantidad de timepo (Span)
data Event a = Event {
                      -- whole permite registrar la duración completa/original
                      -- de un evento, mientras active se reparte en ciclos.
                      -- whole no aplica a señales.
                      whole  :: Maybe Span,
                      active :: Span,
                      value  :: a
                     }
  deriving (Show, Functor)
-- > Event{ active = Span {begin = 0 , end = 1}, value = "hola"}

-- Representación de un patrón como una serie de eventos
-- en un período de tiempo.
-- `query` permite acceder a la función que define el patrón
-- i.e. sacarlo del constructor para ver que eventos produce en
-- el tiempo dado.
data Pattern a = Pattern {query :: Span -> [Event a]}
  deriving (Functor)

-- Para acceder a los valores del patrón de manera conveniente
instance Show a => Show (Pattern a) where
  show pat = show $ query pat (Span 0 1) -- no hay llamada recursiva pues el show corresponde a diferentes tipos


data Value = S String
           | F Double
           | R Rational
        deriving (Show)

type ControlPattern = Pattern (Map.Map String Value)

-- Ejemplo
-- Se implementa una diente de sierra asignando al evento
-- el valor intermedio entre los extremos del TimeSpan
-- Usamos la notación del record syntax para ser más explícitos con
-- los significados de los argumentos del constructor.
saw :: Pattern Rational
saw = Pattern {query = f}
  where f (Span b e) = [Event {whole = Nothing,
                               active = (Span b e),
                               value = (mod' (b + (e - b)/2) 1) -- módulo para acotar los valores
                              }]
-- > query saw (Span 0 1)
-- > query saw (Span 1 2)

-- Esto se puede generalizar así:
signal :: (Rational -> a) -> Pattern a
signal timeFunc = Pattern {query = f}
  where f (Span b e) = [Event Nothing (Span b e) (timeFunc $ b + (e - b)/2)]
-- La función de tiempo se evalua en el punto medio del Span: es ahí donde
-- la señal continua (en el caso anterior `saw`) es sampleada.

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

-- | Producir el siguiente ciclo
nextSam :: Rational -> Rational
nextSam s = sam s + 1

-- | Separar el Span en los ciclos enteros que lo componen
spanCycles :: Span -> [Span]
spanCycles (Span b e)
  | e <= b = []
  | sam b == sam e = [Span b e] -- están en el mismo ciclo
  | otherwise =  Span b (nextSam b) : (spanCycles $ Span (nextSam b) e)


-- | Repite un valor discreto una vez por ciclo.
-- aquí es donde resulta clave tener el parámetro whole de Event
atom :: a -> Pattern a
atom v = Pattern f
  where f s = map spanToEvent (spanCycles s)
        spanToEvent s' = Event (wholeCycle $ begin s') s' v
        wholeCycle t = Just $ Span (sam t) (nextSam t)

-- | Produce un patrón que aplica a cada ciclo (Span)
-- un patrón distinto de la lista
slowcat :: [Pattern a] -> Pattern a
slowcat pats = Pattern f
  where f s = concatMap queryCycle $ spanCycles s
        queryCycle s = query  (pats !! (mod (floor $ begin s) n)) s
        n = length pats

-- Las siguientes funciones auxiliares permiten modificar
-- el tiempo de los patrones, es decir los valores del Span.
-- Con estas se implementa enseguida las funciones `fast` y `slow`

withEventTime :: (Rational -> Rational) -> Pattern a -> Pattern a
withEventTime timeFunc pat = Pattern f
  where f s = map (\e -> e {active = withSpanTime timeFunc $ active e,
                            whole = withSpanTime timeFunc <$> whole e
                           }) $ query pat s
-- IDIOMATISMO DE HASKELL:
-- El operador <$>, es sólo una forma infija de fmap: una función nos permite
-- aplicar otra a los valores del Maybe. Este tipo de datos tiene dos valores
-- data Maybe a = Nothing | Just a
-- Así, evitamos escribir algo como:
--
-- whole = if (whole e == Nothing)
--           then Nothing
--           else (Just $ withspanTime timeFunc $ fromJust (whole e))
--
-- En que tenemos que recuperar el valor del constructor Just, para después
-- aplicar la función y volverlo a meter al Just.

withQueryTime :: (Rational -> Rational) -> Pattern a -> Pattern a
withQueryTime timeFunc pat = Pattern f
  where f s = query pat $ withSpanTime timeFunc s

withSpanTime :: (Rational -> Rational) -> Span -> Span
withSpanTime timeFunc (Span b e) = Span (timeFunc b) (timeFunc e)

fast :: Rational -> Pattern a -> Pattern a
fast t pat = withEventTime (/t) -- ajuste de la duración
               $ withQueryTime (*t) pat -- más ciclos (para t>1)
-- la construcción de fast responde a la lógica de un átomo

slow :: Rational -> Pattern a -> Pattern a
slow t = fast (1/t)

-- | Produce un patrón que aplica cada patrón de la lista en el
-- tiempo dado con la misma duracion.
fastcat :: [Pattern a] -> Pattern a
fastcat pats = fast (toRational $ length pats) $ slowcat pats
-- > query (fastcat [atom 0.6, atom 0.4, saw]) (Span 0 1)

-- fastcat es uno de los ejemplos presentados en McLean 2020 ("Algorithmic Pattern")

-- Unas funciones convenientes:

fastAppend :: Pattern a -> Pattern a -> Pattern a
fastAppend a b = fastcat [a,b]

slowAppend :: Pattern a -> Pattern a -> Pattern a
slowAppend a b = slowcat [a,b]

-- | Sobreponer varios patrones
stack :: [Pattern a] -> Pattern a
stack pats = Pattern $ \s -> concatMap (\pat -> query pat s) pats


-- Objetivo: sumar patrones

instance Applicative Pattern where
  pure = steady -- otra alternativa es usar `atom`
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (<*>) = app sect' -- no hay problema con los casos extremos de sect',
                    -- son manejados por sect dentro de app

-- Aplica un patrón de funciones a un patrón de valores utilizando
-- la estructura dada por una función entre los valores whole de los eventos
app :: (Span -> Span -> Span) -> Pattern (a -> b) -> Pattern a -> Pattern b
app wf patf patv = Pattern f
    -- Necesitamos manipular valores Maybe debido a la función sect
    -- Para ello importamos `catMaybes` para obtener una lista de valores.
    where -- f :: Span -> [Event b]
          f s = concatMap (\ef -> catMaybes $ map (apply ef) evs ) efs
            where efs = query patf s
                  evs = query patv s
                  apply :: Event (a -> b) -> Event a -> Maybe (Event b)
                  apply ef ev = apply' ef ev (sect (active ef) (active ev))
                  apply' _ _ Nothing   = Nothing
                  apply' ef ev (Just s) = Just $ Event structWhole s (value ef $ value ev)
                    where structWhole = wf <$> whole ef <*> whole ev

-- | La intersección de dos TimeSpan
sect :: Span -> Span -> Maybe Span
sect a b = check $ sect' a b
  where check :: Span -> Maybe Span
        check (Span a b) | b <= a = Nothing
                         | otherwise = Just (Span a b)

sect' :: Span -> Span -> Span
sect' (Span b e) (Span b' e') = Span (max b b') (min e e')


-- Prueba en ghci:
-- > query ((+) <$> atom 0.5 <*> atom 2) (Span 0 1)

-- Con el primer operador mapeamos dentro de valor del atom
-- utilizando la funtorialidad de Pattern.
-- Así obtenemos (atom (+ 0.5)), mismo que aplicamos a (atom 2)
-- utilizando su instancia de Applicative.

-- Otras maneras de combinar patrones de funciones con patrones de valores
-- La primera mantiene la estructura de la izquierda (las fuciones), representada
-- en los valores de whole. La segunda hace lo correspondiente por la derecha.

(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<*) = app const

(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
(*>) = app (flip const)


-- Control Patterns

sound :: Pattern String -> ControlPattern
sound pat = (Map.singleton "sound" . S) <$> pat
-- > query (sound $ atom "hello") (Span 0 1)
-- o aprovechando la instancia de Show Pattern, podemos sólo escribir:
-- > sound $ atom "hello"

note :: Pattern Double -> ControlPattern
note pat = (Map.singleton "note" . F) <$> pat
-- > fast 2 $ sound $ atom 4

(#) :: ControlPattern -> ControlPattern ->  ControlPattern
a # b = Map.union <$> a <*> b
-- > fast 2 $ (sound $ atom "hello") # (note $ atom 4)

(|+|) :: Num a => Pattern a -> Pattern a ->  Pattern a
a |+| b = (+) <$> a <*> b

(|+) :: Num a => Pattern a -> Pattern a ->  Pattern a
a |+ b = ((+) <$> a) <* b

(+|) :: Num a => Pattern a -> Pattern a ->  Pattern a
a +| b = ((+) <$> a) *> b

-- Pruebas:
-- > pure 3 |+| pure 4
-- > atom 3 |+| (fastcat [atom 4, silence])
-- > atom 3 |+ (fastcat [atom 4, silence])
