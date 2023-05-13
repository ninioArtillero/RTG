# Distintas expresiones para la misma función

Referencia: HSoM p.97 sec. 5.5

Distintas transformaciones de la definición de la función `hList` que toma una
lista de _pitches_ (alturas representadas como parejas de nota y octava), y
produce una secuencia de notas armonizadas utilizando otra función `hNote`.

Este ejemplo puede hacernos apreciar la diversidad de formas en que un mismo procedimiento
puede ser expresado, dando una pista de cómo la abstracción funcional permite aproximarnos
a dicha definición con distintos alcances expresivos.

```haskell

-- Definición recursiva básica.
hList :: Dur -> [Pitch] -> Music Pitch
hList dur [ ] = rest 0
hList dur (p:ps) = hNote d p :+: hList d ps

-- El patrón recursivo básico detrás de esta definición es descrito
-- en Haskell como un "fold". Podemos evidenciarlo reescribiendo de la
-- siguiente manera:
hList :: Dur -> [Pitch] -> Music Pitch
hList dur pitches = foldr (\p acc -> hNote d p :+: acc) (rest 0) pitches


-- Este patrón recursivo se puede abstraer en dos partes utilizando un map
-- y una función "line" que traduce una lista en una composición secuencial.
hList :: Dur -> [Pitch] -> Music Pitch
hList d ps = let f p = hNote d p
             in line (map f ps)
             
-- Gracias al currying, podemos simplificar la definición de f y obtener
hList :: Dur -> [Pitch] -> Music Pitch
hList d ps = line (map (hNote d) ps)

-- Simplificando nuevamente, utilizando composición y currying para
-- expresar la función en estilo "point-free".
hList :: Dur -> [Pitch] -> Music Pitch
hList d = line . map (hNote d)

```
