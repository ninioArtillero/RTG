## Lenguajes de Programación

En esta sección se define y discute la noción de lenguaje de programación.

En términos generales un lenguaje de programación es una abstracción sobre el _hardware_ de una máquina
diseñada para realizar cálculos. A diferencia de un lenguaje natural, es decir aquel
que aprendemos como parte de procesos de socialización, un lenguaje de programación está
descrito completamente por sus reglas de sintaxis (que especifican las cadenas de símbolos
válidas) y semántica (que determinan la manera en que se hacen los cálculos).

En el _hardware_ se encuentran circuitos electrónicos compuestos de compuertas lógicas que
manipulan valores binarios de cero y uno. 
Estas compuertas son las encargadas de transformar el fenómeno continuo y analógico del voltaje en información digital. 
Los circuitos conforman un sistema de numeración binario
que permite representar estados y operaciones, datos y algoritmos; 
esta representación corresponde al lenguaje de máquina.
La forma en que un sistema de numeración binaria sirve de soporte a la computación tiene su origen en el modelo de la máquina universal de Turing.
Este consiste de una cinta (tan larga como se requiera) con casillas discretas con valores binario y un lector capaz de modificar una a la vez. 
Más adelante se discutirá el modelo alternativo del cálculo lamba en el contexto de la programación funcional para ver que **la conceptualización del proceso de computo tiene implicaciones en el diseño de lenguajes de programación**.

La representación en lenguaje de máquina de procedimientos sencillos como "duplicar un número" es en general ilegible y complicada.
Eventualmente se inventó el lenguaje ensamblador como un primer nivel de abstracción. 
En el lenguaje ensamblador se abrevian algunas de las operaciones básicas utilizando un diccionario de caracteres.

Los lenguajes de programación que son utilizados hoy en día en la industria y la programación creativa son considerados de alto nivel. 
Intuitivamente cada paso hacia un nivel de [abstracción](#abstracción) superior acerca el lenguaje de programación al lenguaje natural, permitiendo al programador expresar su intención de manera más directa.
Una de las consecuencias de subir niveles de abstracción es que labores como la administración de recursos de memoria son delegadas al compilador.
Todo el código se destila eventualmente en lenguaje de máquina, aunque a veces pasa por diversos niveles intermedios. 
En el caso de Erlang o Java, el código se compila a un _bytecode_ que es interpretado por una máquina virtual.
En otras ocasiones el código de un lenguaje de alto nivel es compilado a otro lenguaje de alto nivel.

Tomando una aproximación análoga a la composición musical se podría ubicar, por ejemplo, el nivel más bajo de abstracción en las oscilaciones de presión sonora asociadas al proceso físico del sonido.
El compositor no trabaja necesariamente al nivel de dichas oscilaciones; 
de hacerlo la especificación de una obra sería de una complejidad absurda.
Necesitaría formular una función que representara la variación de presión sonora en cada momento del tiempo de la obra.
Esta tendría que incluir toda la información concerniente a al timbre de los instrumentos, dinámica de la ejecución y propiedades acústicas del espacio. 
La obra nunca podría ser interpretada en rigor, debido a que su representación contiene hasta el más mínimo detalle de un suceso sonoro en un espacio. La interpretación en cualquier tiempo dado diferiría siempre del contenido de la función.

La composición sucede a distintos niveles de abstracción, típicamente en el de las notas e instrumentos.
Las primeras presuponen un sistema que abstrae la frecuencia fundamental de las oscilaciones y las representa por alturas.
Los instrumentos codifican, junto a las anotaciones para el intérprete de la partitura, la multidimensionalidad del timbre.
Aquí el compositor no se ocupa directamente del nivel de presión sonora, la composición espectral y el volumen:
estos descriptores se realizan, o compilan, durante la interpretación.
Por otro lado, existen composiciones espectralistas que utilizan la descomposición espectral del sonido como materia prima para especificar la ejecución instrumental. 

### Lenguaje de dominio específico

En general un lenguaje de programación para música es considerado un lenguaje de dominio específico.
Esto se refiere a que dicho lenguaje está diseñado para un campo de aplicación particular,
que en este caso sería la música.

En general, la implementación de un nuevo lenguaje de dominio específico obedece a una de las siguientes estrategias:

1. Mediante una biblioteca de programación. En este caso el nuevo lenguaje hereda la sintaxis y semántica del lenguaje anfitrión.
2. Cómo una abstracción completamente nueva. En este caso, la implementación del lenguaje se hace con otro lenguaje. Cabe mencionar que hay lenguajes
   que son capaces de compilarse a si mismos a través de un proceso que se conoce como _bootstraping_.

El objetivo de esta sección será distinguir ambas aproximaciones.
