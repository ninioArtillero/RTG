## Tidal Cycles

Tidal Cycles es un sistema para live coding de software libre y código abierto creado por Alex Mclean, quién lo describe como
"un lenguaje de dominio específico para patrones embebido en el lenguaje de programación Haskell,
constituido por una representación de patrones, una librería para generarlos y combinarlos, un programador de eventos (_scheduler_) y una interfaz de programación para live coding" [-@McLean2011ArtistProgrammers, p. 79].
Dicha interfaz se implementa en un _plugin_ que carga un archivo de configuración^[Llamado por defecto `BootTidal.hs` que configura el interprete y define algunas variables convenientes para el uso de la librería] y comunica al editor de texto con una sesión del intérprete de Haskell (`ghci`) en la que están cargados los módulos de Tidal expuestos. Los patrones generados son _streams_ de mensajes OSC.
Tidal no está diseñado para hacer síntesis de sonido, sino que se encarga únicamente del secuenciamiento y [_patronificación_](#patrones) de eventos.

En general, para el live coding musical, dichos mensajes son recibidos por
[SuperDirt](https://github.com/musikinformatik/SuperDirt): un motor de audio en SuperCollider que establece un marco para la creación de sintetizadores y reproducción de muestras
que permite procesar los mensajes de Tidal.

La conceptualización e implementación de los patrones en Tidal Cycles desborda el ámbito músical.
El sistema puede ser usado para crear patrones visuales [@McLean2020Algorithmic] y ha sido utilizando para livecodear patrones de tejido [@McLean2018Weaving]


Un patrón en Tidal es una función del tiempo.

### Uso de patrones  {#patrones}

Los patrones en Tidal Cycles se pueden describir a varios niveles.
El primero a nivel de usuario: un patrón es una secuencia de eventos y valores descritos en una cadena de texto o varias cadenas de texto,
que son combinadas y luego agendadas con respecto a un ciclo implícito.

El siguiente comando crea un patrón que reproduce dos aplausos de igual duración durante la duración del ciclo.
^[En el contexto de la función `sound`, la palabra `cp` refiere a una muestra de sonido accesible a SuperDirt]

```haskell
d1 $ sound "cp cp"
```

Esta notación permite describir polirritmos mediante la superposición de patrones . El siguiente código produce un polirritmo de dos contra cinco:

```haskell
d1 $ stack [
  sound "cp*2",
  sound "sn*5"
  ]
```

El ciclo implícito establece el marco de referencia para la programación de los eventos, respecto a un valor global de ciclos por segundo (CPS).  Este ciclo es compartido por todas las _órbitas_ (noción similar a la de "canal" en SuperDirt),
que son accesibles utilizando los identificadores `d1`, `d2`, `d3`, ...

La cadena de texto es leída con referencia a la sintaxis de la _mini-notation_, que permite subdividir cada evento hasta una profundidad arbitraria.

```haskell
--- Los eventos entre corchetes ocupan el mismo espacio del ciclo que el inicial
d1 $ sound "cp [cp sn cp]"
```

Para ser traducidos a un flujo de eventos, las cadenas que hemos dado como argumento a la función `sound` subdividen la unidad temporal del ciclo. La posición y nivel de anidación de cada valor  (`cp` o `sn`) determinan una subdivisión racional ^[Relativo a los número racionales, es decir, aquellos que se expresan como fracciones de números enteros.] de la unidad temporal del ciclo.

Es posible definir los valores de efectos mediante patrones, que se combinan a su vez con los eventos de sonido
mediante una variedad de operadores.

El operador `#` (alias de `|>`) toma la estructura de patrón de sonido (ubicado a la izquierda) y le asignando los valores de un patrón de control en función de su posición.

```haskell
-- los eventos en la segunda mitad del ciclo se reproducen al doble de velocidad
d1 $ sound "cp sn cp" # speed "1 2"
```

Tidal nos arroja la siguiente visualización del patrón al evaluarlo fuera del contexto de una órbita:

```haskell
-- evaluar esta linea
sound "cp sn cp" # speed "1 2"

-- muestra lo siguiente
(0>⅓)|s: "cp", speed: 1.0f
(⅓>½)-⅔|s: "sn", speed: 1.0f
⅓-(½>⅔)|s: "sn", speed: 2.0f
(⅔>1)|s: "cp", speed: 2.0f
```

Esta lista indica la secuencia de eventos, representados como segmentos del ciclo (`(0>⅓)`),
y el conjunto de valores  que les son asignados (`s: "sn", speed: 1.0f`).
El cuarto evento no produce ningún sonido y sólo registra las regiones en que se traslapan los valores.
Alternativamente podemos utilizar el operador `|>|`, que asigna los valores y combina las estructuras

```haskell
d1 $ sound "cp sn cp" |>| speed "1 2"
```

produciendo:

```
(0>⅓)|s: "cp", speed: 1.0f
(⅓>½)|s: "sn", speed: 1.0f
(½>⅔)|s: "sn", speed: 2.0f
(⅔>1)|s: "cp", speed: 2.0f
```

En este caso sí se reproducen los 4 eventos.
Además de pasar los valores, estos también se pueden combinar con operadores aritméticos.El siguiente ejemplo es reducido paso a paso de representaciones equivalentes del mismo patrón (gracias a la transparencia referencial).

```haskell
d1 $ sound "drum" |+| n "2 3" |+| n "4 5 6"

d1 $ sound "drum" |+| n "6 [7 8] 9"

-- La notación sonido:número índica que muestra se utiliza.
d1 $ sound "drum:6 [drum:7 drum:8] drum:9"
```

Los patrones en Tidal pueden ser transformados por una extensa variedad de funciones^[Mismas que se pueden encontrar en la documentación oficial en https://tidalcycles.org/docs/] que junto a la capacidad de combinar estructuras y valores permite crear patrones de elevada complejidad con relativamente poco código. Esto permite diluir rápidamente toda noción de repetición (aunque eventualmente sucede).

```haskell {#ejemploTidalHaskell}
d1
  $ almostNever (slow 2)
  $ every 4 (off 0.25 (+ n 2))
  $ jux rev
  $ superimpose (
    (+ squiz "3 7 3 9") .
    (# room "0.3 0.3 0.7") .
    (hurry "[0.35 | 0.25 | 0.10 0.35 | 0.25 0.75]")
    )
  $ s "sn cp sn"
```

Sólo por diversión podemos ver la representación de este patrón:

```
(0>¼)|pan: 0.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 3.0f
(0>¼)|pan: 1.0f, room: 0.7f, s: "sn", speed: 0.25f, squiz: 9.0f
(0>¼)|n: 2.0n (d5), pan: 0.0f, s: "sn"
(0>¼)|n: 2.0n (d5), pan: 0.0f, room: 0.7f, s: "sn", speed: 0.25f, squiz: 9.0f
(0>¼)|n: 2.0n (d5), pan: 1.0f, s: "sn"
(0>¼)|n: 2.0n (d5), pan: 1.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 3.0f
(0>⅓)|pan: 0.0f, s: "sn"
(0>⅓)|pan: 1.0f, s: "sn"
(¼>⅓)-½|pan: 0.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 7.0f
(¼>⅓)-½|pan: 1.0f, room: 0.7f, s: "sn", speed: 0.25f, squiz: 3.0f
(¼>½)|n: 2.0n (d5), pan: 0.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 3.0f
(¼>½)|n: 2.0n (d5), pan: 1.0f, room: 0.7f, s: "sn", speed: 0.25f, squiz: 9.0f
(¼>7/12)|n: 2.0n (d5), pan: 0.0f, s: "sn"
(¼>7/12)|n: 2.0n (d5), pan: 1.0f, s: "sn"
¼-(⅓>½)|pan: 0.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 7.0f
¼-(⅓>½)|pan: 1.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 3.0f
(⅓>⅔)|pan: 0.0f, s: "cp"
(⅓>⅔)|pan: 1.0f, s: "cp"
(½>7/12)-¾|n: 2.0n (d5), pan: 0.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 7.0f
(½>7/12)-¾|n: 2.0n (d5), pan: 1.0f, room: 0.7f, s: "sn", speed: 0.25f, squiz: 3.0f
(½>⅔)-¾|pan: 0.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 3.0f
(½>⅔)-¾|pan: 1.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 7.0f
½-(7/12>¾)|n: 2.0n (d5), pan: 0.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 7.0f
½-(7/12>¾)|n: 2.0n (d5), pan: 1.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 3.0f
(7/12>11/12)|n: 2.0n (d5), pan: 0.0f, s: "cp"
(7/12>11/12)|n: 2.0n (d5), pan: 1.0f, s: "cp"
½-(⅔>¾)|pan: 0.0f, room: 0.7f, s: "sn", speed: 0.25f, squiz: 3.0f
½-(⅔>¾)|pan: 1.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 7.0f
(⅔>1)|pan: 0.0f, s: "sn"
(⅔>1)|pan: 1.0f, s: "sn"
(¾>11/12)-1|n: 2.0n (d5), pan: 0.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 3.0f
(¾>11/12)-1|n: 2.0n (d5), pan: 1.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 7.0f
(¾>1)|pan: 0.0f, room: 0.7f, s: "sn", speed: 0.25f, squiz: 9.0f
(¾>1)|pan: 1.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 3.0f
(11/12>1)|n: 2.0n (d5), pan: 0.0f, s: "sn"
¾-(11/12>1)|n: 2.0n (d5), pan: 0.0f, room: 0.7f, s: "sn", speed: 0.25f, squiz: 3.0f
(11/12>1)|n: 2.0n (d5), pan: 1.0f, s: "sn"
¾-(11/12>1)|n: 2.0n (d5), pan: 1.0f, room: 0.3f, s: "sn", speed: 0.25f, squiz: 7.0f
```

### Representación de patrones

En un segundo nivel tenemos los patrones entendidos desde su implementación computacional.
A nivel del código, los patrones son representados por funciones puras. @McLean2011ArtistProgrammers los define como:

```haskell
data Pattern a =
  Pattern {at :: Behaviour a, period :: Period}

type Behaviour a = Int -> [Maybe a]

type Period = Maybe Int
```

A diez años del desarrollo de Tidal,  @McLean2021TidalCycles refiere la siguiente representación:

```haskell
data Span = Span { begin :: Rational, end :: Rational}
  deriving (Show)
-- Ejemplo > Span {begin = 0 , end = 1}

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

-- Ejemplo > Event{ active = Span {begin = 0 , end = 1}, value = "hola"}

-- Un patrón es presentado como una serie de eventos
-- en un período de tiempo.
-- El campo `query` permite acceder a la función que define el patrón
-- i.e. sacarlo del constructor para ver que eventos produce en
-- el tiempo dado.
data Pattern a = Pattern {query :: Span -> [Event a]}
  deriving (Functor)
```
