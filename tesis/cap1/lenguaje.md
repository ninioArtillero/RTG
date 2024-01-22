# Lenguajes de Programación

En esta sección se define y discute la noción de lenguaje de programación.

Un lenguaje de programación es una abstracción sobre el _hardware_ de una máquina
diseñada para realizar cálculos. A diferencia de un lenguaje natural, es decir aquel
que aprendemos como parte de procesos de socialización, un lenguaje de programación está
descrito en su totalidad su sintaxis (que especifican las cadenas de símbolos
válidas) y de semántica (que determinan la manera en que se hacen los cálculos).

En el _hardware_ se encuentran circuitos organizados como compuertas lógicas que
representan valores binarios de cero y uno. Estas compuertas se componen en un sistema de numeración binario
que permiten representar estados y operaciones, datos y algoritmos; esta representación corresponde al lenguaje de máquina.
El modelo de la máquina universal de Turing demuestra una cinta (tan larga como se requiera) con casillas discretas de números binarios
junto con un lector capaz de leer y modificar uno por uno, basta
para la representación de cualquier proceso computable; más adelante discutiremos contrastaremos este modelo con
el del cálculo lamba, pues la conceptualización misma del proceso de computo tiene implicaciones en el diseño de lenguajes de programación.

La representación en lenguaje de máquina de procedimiento sencillos como "duplicar un número" es en general ilegible y complicada.
Debido a esto se inventó el lenguaje ensamblador como un primer nivel de abstracción. En el lenguaje ensamblador se abrevian algunas de las
operaciones básicas utilizando un diccionario de caracteres.

Los lenguajes de programación que son utilizados hoy en día por la industria y la
programación creativa son considerados de alto nivel. Cada nuevo nivel de abstracción acerca el lenguaje de programación
al lenguaje natural, permitiendo al programador expresar su intención de manera más directa. Subir en nivel de abstracción
implica que delegamos al _compilador_ la labor de eficientizar el uso de recursos.
Todo el código se destila eventualmente en lenguaje de máquina, aunque a veces pasa por diversos niveles intermedios
antes de ello. En el caso de Erlang o Java, el código de _compila_ a un _bytecode_ que
es interpretado por una máquina virtual. En otras ocasiones el código
de un lenguaje de alto nivel es compilado a otro lenguaje de alto nivel.

En analogía con la composición musical, podemos ubicar (quizás) el nivel más bajo en las oscilaciones de presión sonora
asociadas al proceso físico del sonido. El compositor no trabaja necesariamente al nivel de dichas oscilaciones.
La composición sucede a distintos niveles de abstracción, típicamente en el de las notas e instrumentos.
Las primeras presuponen un sistema que abstrae la frecuencia fundamental de las oscilaciones y las representa por alturas.
Los instrumentos codifican, junto a las anotaciones para el intérprete de la partitura, la multidimensionalidad del timbre.
Aquí el compositor no se ocupa directamente del nivel de presión sonora, la composición espectral y el volumen: podemos decir que estos descriptores se
realizan, o compilan, durante la interpretación. En contraste las composiciones espectralistas trabajan con el nivel timbre: ya sea como
material para la ejecución instrumental o en la síntesis de sonido.

En la práctica del live coding se utilizan lenguajes de programación en tiempo real para manipular los distintos niveles de abstracción.
Este uso es análogo al de un instrumento musical, salvo por el particular efecto de ser también partitura.
Mientras un instrumento se entiende de manera clásica en términos de las notas que se ejecutan en el mismo como sus unidades elementales,
las unidades de un lenguaje de programación incluye las transformaciones de gestos musicales completos.
En principio esto se puede hacer con cualquier lenguaje de programación.
**En esta tesis me interesa explorar el uso de lenguajes diseñados específicamente para la creación musical**.

## Implementación de un lenguaje para la música

En general un lenguaje de programación para live coding está escrito en otro lenguaje de programación. Esto puede darse por diversas vías:

1. Mediante una biblioteca de programación. En este caso el nuevo lenguaje hereda la sintaxis y semántica
   del lenguaje anfitrión.
2. Cómo una abstracción completamente nueva. En este caso, la implementación del lenguaje se hace con otro lenguaje. Cabe mencionar que hay lenguajes
   que son capaces de compilarse a si mismos a través de un proceso que se conoce como _bootstraping_.

El objetivo de esta sección será distinguir ambas aproximaciones.
