## Programación Reactiva Funcional {#frp}

La programación reactiva funcional (FRP por sus siglas en inglés) es un método
para el diseño de bibliotecas en que el dominio de aplicación depende intrínsecamente de un parámetro continuo de tiempo.
Esto quiere decir que la [denotación](#semántica-denotacional) del tiempo son los números reales ($\mathbb{R}$).
Junto a una denotación clara y concisa de la especificación de la API, este constituye el núcleo de la FRP [@Elliott2015Essence].

El continuo de los números reales escapa a la representación digital, pero esto no impide que la especificación de la biblioteca parta de ello.
La FRP en sí es una prueba de esta idea.
Es en el contexto de su implemetnación que necesitamos recurrir tipos de datos numéricos _densos_,
es decir que tienen o se aproximan a un resolución infinita,
tales como puntos flotantes (`Float` y `Double` en Haskell) o racionales (`Rational`, que corresponden a fracciones de números enteros).
La elección del tipo de dato utilizado para el cálculo es independiente de la especificación de la biblioteca.
Es en la implementación donde sus peculiaridades se manifiestan en penalizaciones computacionales distintas.
El análisis de la complejidad y costo computacional de los detalles de implementación escapa a los propósitos de esta tesis.
^[Para un tratamiento introductorio exhaustivo se puede consultar [REF](#ref).]
Para explicar como es posible utilizar el continuo en la especificación,
@Elliott2015Essence menciona que la FRP es al tiempo lo que las _vector graphics_ (gráficas vectoriales) al espacio.
Esto permite a los objetos una gran _compositionality_ (componibilidad).
En el caso de una imagen representada por un mapa de pixeles fijo, el resultado de rotarla o expandirla no tendría por que coincidir con la retícula pixeles inicial.
Cada transformación requeriría pasos de interpolación (aproximación) haciendo que se acumulen errores.
En las gráficas vectoriales, es hasta el último momento en que los valores son requeridos que la interpolación es ejecutada,
por lo que este principio se conecta directamente con la [evaluación floja](#lazy).

El origen del concepto se remonta a la _Functional Reactive Animation_ de @Elliott1997Functional,
cuyo propósito fue la creación de "abstracciones componibles" para la programación de animaciones.
Para @Elliott1997Functional una animación es una función del tiempo (continuo) valuada en algún tipo de imágenes.
Las animaciones son ejemplo de una abstracción más general que llaman _behavior_ (comportamiento), que se denota con funciones del tiempo en algún tipo arbitrario.

    data Behavior a = Behavior {at :: Time -> a}

Por ejemplo, el _behavior_ más simple es el tiempo mismo y como tal corresponde a la función identidad.

    time :: Behavior Time
    time t = t

La clave de este método se encuentra en la abstracción de la reactividad,
es decir la capacidad de los comportamientos de ser modificados por acciones externas a ellos,
códificada en la función `untilB`.

    untilB :: Behavior a -> Event (Behavior b) -> Behavior b
    at (b `untilB` e) t = if (t <= t0) then at b t else at b' t
      where (t0,b') = occ e

El comportamiento resultante de esta función coincide con el comportamiento inicial `b` hasta que se da el evento `e` en el tiempo `t0`, momento en que cambia al comportamiento `b'` contenido en el evento.
Esta función depende de otra abstracción fundamental que llaman _event_ (evento).

    data Event a = Event { occ :: (Time,a) }

Esta formulación de la FRP aparenta proponer una API _ad hoc_; en la formulación posterior de @Elliott2009Pushpull se evidencia que muchas de los elementos de la API son consecuencia de abstracciones más generales incluidas en Haskell a manera de [clases de tipos](#type-classes).
