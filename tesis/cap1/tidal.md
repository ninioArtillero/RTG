## Tidal Cycles

En esta sección haremos un análisis de [Tidal Cycles](https://tidalcycles.org/) (en adelante abreviado sólo como Tidal)
como un instrumento musical expresivo en relación a las abstracciones en que se basa su diseño.

Tidal Cycles es un sistema para live coding de software libre y código abierto creado por Alex Mclean, quién lo describe como
"un lenguaje de dominio específico para patrones embebido en el lenguaje de programación Haskell,
constituido por una representación de patrones, una librería para generarlos y combinarlos,
un programador de eventos (_scheduler_) y una interfaz de programación para live coding"
[-@McLean2011ArtistProgrammers, p. 79].
A continuación se describen cada uno de estos componentes.

Los _patrones_ en Tidal son la unidad musical y computacional fundamental;
se trata de la abstracción que configura tanto el diseño del sistema como su uso.
Describiremos los patrones en Tidal Cycles en tres niveles conceptuales: [usuario](#uso-de-patrones), implementación y especificación.
La [representación de patrones](#representación-de-patrones) utiliza los principios de la [programación reactiva funcional](#frp).
En Haskell el modelado idiomático de problemas parte la representación de valores apropiados al dominio utilizando el sistema de tipos.
Este modelado corresponde al nivel de la implementación.
Por su parte podemos, aprovechando las ventajas de la retrospectiva, podemos remitir la especificación al concepto de [patrón algorítmico](#patrón-algorítmico).
Como una definición común a los tres niveles, podemos decir que un patrón es _una función del tiempo en un conjunto de eventos_.

La interfaz de programación para live coding se manifiesta como un _plugin_ que carga un archivo de configuración^[Llamado por defecto `BootTidal.hs`.
Este configura el interprete y define algunas variables convenientes para el uso de la librería.]
y comunica al editor de texto con una sesión del intérprete de Haskell (`ghci`) en la que están cargados los módulos expuestos de Tidal.

El programador de eventos es un modulo encargado crear _streams_ con paquetes de mensajes OSC que representan los eventos.
Una _time-stamp_ es asignada a cada evento para coordinar su reproducción en el tiempo.
Este proceso depende de un tiempo de latencia para que el procesador
pueda agendar los eventos contenidos en dichos paquetes [ver @Blackwell2022Live, chap. 6],
sentando una cota inferior en milisegundos para el tiempo de acción de evaluar código durante una interpretación.

Tidal no está diseñado para hacer síntesis de sonido, sino que se encarga únicamente del secuenciamiento y _patronificación_ de eventos.^[La patronificación es la traducción literal del uso de la palabra _pattern_ como verbo, en lugar de sustantivo. El concepto de patrón algorítmico justifica el uso de este término como se verá más adelante.]
En general, para su uso musical, dichos mensajes son recibidos por
[SuperDirt](https://github.com/musikinformatik/SuperDirt): un motor de audio en SuperCollider que establece un marco para la creación de sintetizadores y reproducción de muestras
diseñado para procesar los mensajes de Tidal.

La conceptualización e implementación de los patrones en Tidal Cycles desborda el ámbito músical.
El sistema puede ser usado para crear patrones visuales [@McLean2020Algorithmic] y ha sido utilizando para livecodear patrones de tejido [@McLean2018Weaving].

### Uso de patrones

A nivel del usuario un patrón es una secuencia de eventos con valores descritos por una o varias cadenas de texto
que son combinadas y luego agendadas con respecto a un ciclo temporal implícito.

El siguiente comando crea un patrón que reproduce un aplauso y una tarola con una duración de medio ciclo por evento.
^[En el contexto de la función `sound`, las palabras en su argumento refieren muestras de sonido accesibles o sintetizadores definidos en SuperDirt.]

```haskell
d1 $ sound "cp cp"
```

El ciclo implícito establece el marco de referencia para la programación de los eventos,
respecto a un valor global en ciclos por segundo (CPS).
Este ciclo es compartido por todas las _órbitas_ (noción similar a la de "canal" en SuperDirt),
que son accesibles utilizando los identificadores `d1`, `d2`, `d3`, ...
Definir la duración del ciclo en 1 segundo hace que cada evento del patrón anterior
tenga una duración de medio segundo:

```haskell
setcps 1
```

Podemos utilizar la fórmula $(\text{BPM}/60/\text{PULSOS})$ para determinar el valor de ciclos por segundo
con respecto a un valor de pulsos por minuto y a un número de pulsos por ciclo.
El valor por defecto en Tidal es $(135/60/4) = 0.5625$.

Podemos describir polirritmos mediante la superposición de patrones.
El siguiente código produce un polirritmo de dos contra cinco:

```haskell
d1 $ stack [
  sound "cp*2",
  sound "sn*5"
  ]
```

La sintaxis de la _mini-notation_ aplicada a las cadenas de texto es el método para describir los patrones de sonidos o parámetros.
Esta notación permite subdividir cada evento hasta una profundidad arbitraria.
Los eventos en cada nivel de anidación (delimitados por corchetes) ocupan el mismo tiempo.
El siguiente patrón asigna duraciones de $1/2$, $1/6$, $1/12$, $1/12$ y $1/6$ de ciclo a sus eventos respectivamente:

```haskell
d1 $ sound "cp [cp [sn cp] sn]]"
```

Las cadenas que se dan como argumento a la función `sound` subdividen la unidad temporal del ciclo para ser traducidos a un flujo de eventos.
La posición y nivel de anidación de cada valor (aqui `cp` o `sn`) determinan una subdivisión racional ^[Relativo a los número racionales, es decir, aquellos que se expresan como fracciones de números enteros.] de la unidad temporal del ciclo.

Es posible definir los valores de efectos mediante patrones, que se combinan a su vez con los eventos de sonido
mediante una variedad de operadores.

El operador `#` (alias de `|>`) toma la estructura de patrón de sonido (ubicado a la izquierda) y le asignando los valores de un patrón de control en función de su posición.

```haskell {#ejemplo-para-transformar}
-- Los eventos en la segunda mitad del ciclo
-- se reproducen al doble de velocidad
d1 $ sound "sn cp sn" # speed "1 2"
```

Evaluar un patrón fuera del contexto de una órbita imprime una lisa lista
de los contenidos del paquete OSC del patrón.

```haskell
sound "sn cp sn" # speed "1 2"

-- Produce la siguiente salida en la consola:
(0>⅓)|s: "sn", speed: 1.0f
(⅓>½)-⅔|s: "cp", speed: 1.0f
⅓-(½>⅔)|s: "cp", speed: 2.0f
(⅔>1)|s: "sn", speed: 2.0f
```

Esta lista indica la secuencia de eventos, representados como segmentos del ciclo (`(0>⅓)`),
y el conjunto de valores  que les son asignados (`s: "sn", speed: 1.0f`).
Aquí el tercer evento no produce ningún sonido y sólo registra las regiones en que se traslapan los valores.
Alternativamente podemos utilizar el operador `|>|`, que asigna los valores y combina las estructuras

```haskell
d1 $ sound "sn cp sn" |>| speed "1 2"

-- Produce:
(0>⅓)|s: "sn", speed: 1.0f
(⅓>½)|s: "cp", speed: 1.0f
(½>⅔)|s: "cp", speed: 2.0f
(⅔>1)|s: "sn", speed: 2.0f
```

En este caso sí se reproducen los 4 eventos.
Además de pasar los valores, estos también se pueden combinar con operadores aritméticos.
El siguiente ejemplo es reducido paso a paso de representaciones equivalentes del mismo patrón (gracias a la transparencia referencial).

```haskell
d1 $ sound "drum" |+| n "2 3" |+| n "4 5 6>

d1 $ sound "drum" |+| n "6 [7 8] 9"

d1 $ sound "drum:6 [drum:7 drum:8] drum:9"

-- La notación <sonido:número>
-- índica que muestra se utiliza.
```

Los patrones en Tidal pueden ser transformados por una extensa variedad de funciones^[Mismas que se pueden encontrar en la documentación oficial.] que, junto a la capacidad de combinar estructuras y valores, permite crear patrones de elevada complejidad con relativamente poco código. Esto permite diluir rápidamente toda noción de repetición (aunque eventualmente sucede). Podemos transformar el [ejemplo anterior](#ejemplo-para-transformar) así:

```haskell {#ejemploTidalHaskell}
d1
  $ almostNever (slow 2)
  $ every 4 (off 0.25 (+ n 2))
  $ jux rev
  $ superimpose (
    (+ squiz "3 7 3 9") .
    (# room "0.3 0.3 0.7") .
    (hurry
   "[0.35|0.25|0.10 0.35|0.25 0.75]")
    )
  $ s "sn cp sn"
```

Escuchar este patrón da una idea muy precisa del poder expresivo de Tidal,
pero en honor al presente medio podemos en su lugar atestiguar la representación de este patrón:

```haskell
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

Esta y subsiguientes implementaciones están influenciadas por la [FRP](#frp).
En este caso el tiempo es representado por los enterios `Int`, pensando el flujo de tiempo como una serie de pulsos isócronos discretos.

A diez años del desarrollo de Tidal,  @McLean2021TidalCycles refiere la siguiente representación:

```haskell
data Span = Span { begin :: Rational, end :: Rational}
  deriving (Show)

data Event a = Event {
                      whole  :: Maybe Span,
                      active :: Span,
                      value  :: a
                     }
  deriving (Show, Functor)

data Pattern a = Pattern {query :: Span -> [Event a]}
  deriving (Functor)
```

El uso de números racionales para representar el tiempo tiene la ventaja de que permite preservar la presición de los cálculos.

### Patrón algorítmico

TODO
