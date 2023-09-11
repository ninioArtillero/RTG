# Representación Musical en Mazzola 2021

El modelo de Mazzola para una arquitectura conceptual de la música.

Referencias:
  
* Mazzola 2021 : A Computational Music Thery of Everything (artículo)
* Mazzola 2002 : The Topos of Music (libro)

```haskell
module Denotators where


-- Primero implementamos los 3 tipos de espacio compuesto.

-- Product/limit: Corresponde con los "product types"
-- Usaremos estos denotators para representar las notas en una partitura.
data Note = Silence { onset :: Onset, duration :: Duration }
          | Note { onset :: Onset, pitch :: Pitch, loudness :: Loudness, duration :: Duration }
            deriving Show

type Onset = Float
type Pitch = Integer
type Loudness = Float
type Duration = Float



-- Unions/coproducts: corresponde con los "sum types".
-- Mazzola 2021 utiliza las partes instrumentales de una orquestra como ejemplo.
-- La siguiente construcción piensa [a] como un tipo suma infinito.
-- Consideramos un número indeterminado (pero enumerable) de instrumentos.
data Instrument = Instrument Int
  deriving Show

type Orchestra = [Instrument]

-- Collection: se refiere a un conjunto de denotators
-- con un único espacio de coordenadas dado (Int, Float...)
-- Por ejemplo un acorde:
type Chord = [Note]
```
