# Abstracción y expresividad  {#cap2}

En este capítulo se indagará en la acepción de los términos “abstracción” y “expresividad” en los contextos de la ciencia de la computación y la composición musical.
El objetivo es delimitar un _espacio conceptual_ transdiciplinario que permita tratar los lenguajes de programación como instrumentos musicales.

## Un espacio conceptual para la expresividad funcional

Para la noción de “espacio conceptual” se utiliza la formulación de @Wiggins2015marco en el contexto de la creatividad computacional explorativa; se entiende como un conjunto de conceptos definido por criterios de pertenencia, estrategias de generación y una función de valor. Esto se puede entender como un tipo de semiósfera [@Lotman1996Acerca].

Para esta operación teórico-conceptual se tomará el concepto de “nivel semántico” que encontramos en lingüística y en la teoría de lenguajes de programación.
Básicamente, se identifica un nivel semántico con un capa de abstracción.
El caso de Haskell y Tidal Cycles estudiado en el capítulo anterior ofrecerá ejemplos pertinentes.

La expresividad de un lenguaje de programación es abordada por @Felleisen1990expressive; utilizaremos esta aproximación para analizar algunas nociones formales e intuitivas de la expresividad musical en la literatura.

Este marco permitirá describir la concepción, especificación y desarrollo de la biblioteca RTG partiendo de la representación de la entidad musical en Haskell.
El mismo será también aplicable de manera general para el diseño de otros lenguajes para la música y, en particular, el live coding (cuando partimos de la [FRP](#frp)). 

Finalemente, emergerá el concepto de “expresividad funcional” como criterio de diseño para lenguajes orientados a la música.

## Tidal Cycles como sistema expresivo

La experiencia que he tenido como usuario de [Tidal Cycles](https://tidalcycles.org) (TC) me ha llevado a considerarlo un instrumento expresivo para la improvisación musical. Sin embargo, la naturaleza de esta expresividad es distinta que la que otorgamos a un instrumento de cuerda frotada. Mi investigación parte de esta intuición hacia la propuesta conceptual de una expresividad funcional como estrategia de diseño y al desarrollo de un lenguaje que explore el espacio de ritmos a través de la geometría.

TC basa su diseño en la noción de patrón y ciclo como fundamentos de la entidad musical. 
Al escribir un patrón y ejecutarlo, el intérprete produce una secuencia de instrucciones en bucle a un motor de audio.[^interprete]

[^interprete]: El interprete es un compilador interactivo: un programa que se encarga de traducir el código fuente de un lenguaje de programación en instrucciones de para el procesador en tiempo real. En términos generales podemos pensar la compilación como la traducción del código a un lenguaje de nivel inferior. A nivel más bajo, el código se expresa en lenguaje binario y finalmente en cálculos realizados mediante compuertas lógicas electrónicas determinadas por un conjunto de instrucción que dependen de la arquitectura del procesador.

En términos generales cada nuevo patrón, o la modificación de uno ya existente, se monta sobre el proceso de ejecución al inicio de un nuevo ciclo. 
Esto permite acoplar los patrones y dar (o quitar) coherencia rítmica a su combinación rápida y efectivamente de manera previsible, factor que resulta crucial en algunas presentaciones en vivo.
TC incluye una colección de funciones que transforman el material musical:
invertir, acelerar, ralentizar y desplazar los patrones respecto de un ciclo, aplicar efectos y combinar patrones entre sí mediante una suerte de aritmética de patrones.
En TC los parámetros de todas estas transformaciones son ingresados con la misma sintaxis que se utiliza para patrones de eventos sonoros,[^mininotation] permitiendo un complejo entretejido de patrones capaz de desplegar un amplio espectro de resultados musicales.[^ejemplosTC]
En mi tesis pretendo demostrar que este tipo de instrumento textual despliega su potencia expresiva a partir de su abstracción de la entidad musical y de las funciones que la transforman. 

[^mininotation]: A esta sintaxis se le conoce como _mini-notation_: \url{https://tidalcycles.org/docs/reference/mini_notation/}

[^ejemplosTC]: Ver las presentaciones con TC de Atsushi Tadokoro, Akihiro Kubota, Moxus y Alex McLean (Yaxu) en DOMMUNE - Tokyo x Yorkshire exchange transmission 2018: \url{https://youtu.be/dIpzU71LAQQ}

@McLean2020Algorithmic, creador de TC, define “patrón algorítmico” como “la percepción de la actividad sistemática” produciendo una noción extendida de algoritmo en que los procedimientos definidos no resuelven un problema específico y pueden no terminar, contrario a lo que implica el término en la ciencias de la computación [ver @OlivaresSoria2022Principios].

Un patrón algorítmico refiere a maneras formalizadas de hacer que son percibidas en los resultados [@McLean2020Algorithmic, p. 266].
Otro elemento significativo en este mismo artículo es la mención de la Functional Reactive Programming (programación reactiva funcional) como fundamento para la implementación computacional del concepto general de patrón en TC. Esta es un conjunto de abstracciones para el tratamiento de comportamientos continuos basado en la semántica denotacional y la programación funcional [@Elliott1997Functional; @Elliott2009Pushpull].[^absTiempo] En TC esto significa que los patrones están implementados como funciones puras del tiempo.[^pureza] Por su parte la programación funcional es un paradigma que surge en el contexto de la teoría de lenguajes de programación que prescribe un estilo para la escritura de programas. Su propósito es expandir la capacidad de abstracción y modularización mediante características como el uso de funciones de orden superior (funciones de funciones) y restricciones como la inmutabilidad de las variables [@Hughes1989Why]. Refiero al lector al artículo citado para una descripción detallada de la programación funcional. Este paradigma es relevante para el ámbito creativo por los avances que propone en el diseño de lenguajes de programación de alto nivel, que acercan al “artista-programador” [@McLean2011ArtistProgrammers, p. 11] a la capacidad de especificar el qué en lugar del cómo al componer sus obras.^[Aquí me refiero al contraste entre programación imperativa y programación declarativa. Ver \url{https://www.ionos.es/digitalguide/paginas-web/desarrollo-web/programacion-declarativa/}]

[^absTiempo]: La abstracción del tiempo como parámetro de los comportamientos continuos y la manera en que el código se actualiza en “tiempo real” se ha hecho de diversas maneras en los sistemas de live coding. Esta tiene implicaciones en la implementación y en la interacción con el usuario [@Blackwell2022Live, cap. 6].

[^pureza]: En el contexto de la programación funcional la pureza no corresponde a un criterio de exclusión, sino a una caracterización formal: Una función pura es aquella que no tiene efectos secundarios y cuya salida está completamente determinada por los datos de entrada.

El lenguaje de programación Haskell es de particular importancia en este contexto pues fue concebido por un comité internacional como un estándar para la investigación académica en programación funcional pura y de evaluación floja [@Hudak2007history]. Por otro lado, la elección del mismo como lenguaje anfitrión para TC parece responder a criterios generales. De acuerdo a @Hudak2018Haskell, este lenguaje tiene características que lo hacen propicio para la música: 

>Utilizar un lenguaje de alto nivel para expresar ideas musicales no es algo nuevo, por supuesto. Pero Haskell es único en su insistencia en la pureza (sin efectos secundarios), y esto por sí solo lo hace especialmente adecuado para expresar ideas musicales. Al enfocarnos en lo que es una entidad musical en lugar de cómo crearla, permitimos que las ideas musicales tomen su forma natural como expresiones de Haskell. Los múltiples mecanismos de abstracción de Haskell nos permiten escribir programas de música por computadora que son elegantes, concisos y a la vez poderosos (Hudak y Quick 2018, xi).

La “abstracción” es un concepto central para mi investigación y una actividad fundamental para la práctica de la programación de software en que “el propósito de abstraer no es ser vago, sino crear un nuevo nivel semántico en el que uno pueda ser absolutamente preciso” [@Dijkstra1972humble]. En esta investigación aprovecharé las características de Haskell como entorno para estudiar cómo es abstraída la entidad musical, el nivel semántico que surge de dicha abstracción y sus ofrecimientos como instrumento musical. 
Resulta crucial notar que en el live coding el instrumento y la partitura se funden en código fuente, mismo que tiene la capacidad de expresar más que sólo instrucciones computacionales y algoritmos. Es por ello que la noción de expresividad relevante tendrá que conjugar aspectos sonoros y textuales. @Hudak2018Haskell opinan que “el código suele representar el proceso de pensamiento del autor, su intención musical y decisiones artísticas” (3). Esto lo mencionan en el contexto de la composición algorítmica, sin embargo es también relevante como elemento performativo del live coding donde el código fuente, sumado al resultado sonoro y su evolución en el tiempo, expresa un proceso retroalimentativo de escucha, pensamiento y escritura.

## La abstracción de la entidad musical

En este punto surgen preguntas como: ¿Qué papel juega la abstracción en la música interpretada a partir de la escritura de código? ¿Un nuevo nivel semántico tiene efectos en la expresividad musical?
En la búsqueda por comprender el rol expresivo del mecanismo de abstracción y su función en el diseño de lenguajes para live coding, he emprendido el estudio de otras abstracciones de la entidad musical en bibliotecas de Haskell. [Euterpea](https://www.euterpea.com/), creada por Paul Hudak,[^hudak] es una biblioteca para composición algorítmica y síntesis de sonido cuya entidad musical se define a partir de la composición secuencial y en paralelo de notas y silencios con anotaciones similares a las de una partitura [@Hudak2018Haskell, p. 30]. Por otro lado, Conductive es un sistema de live coding diseñado por Renick Bell que utiliza la metáfora de un conductor de orquesta en su diseño [@Blackwell2022Live, p. 55]. En [Conductive](https://hackage.haskell.org/package/conductive-base/) la entidad musical esta constituída por procesos autónomos organizados en “ambientes musicales” constituidos por “intérpretes” y “partituras”.[^renick] Esto me lleva a mi pregunta central: ¿Cómo se refleja la abstracción de la entidad musical en los resultados sonoros y expresividad de un lenguaje para live coding?^[Esta “correspondencia” tendría el carácter de analogía: una relación lógico-estructural de un campo a otro. Esta es la tercera en la escala de conexión entre campos disciplinares propuesta por @OlivaresSoria2022Principios [p. 27]: símil, metáfora, analogía y equivalencia.]

[^hudak]: Paul Hudak fue, junto como Philip Wadler, el editor en jefe del primer reporte del lenguaje Haskell en 1990 [@Hudak2007history]. Además de científico de la computación, Hudak era un intérprete de jazz interesado en la composición de música por computadora. Su libro “The Haskell School of Music” fue publicado póstumamente tras su muerte en 2015.

[^renick]: Ver las presentación de Renick Bell utilizando Conductive en DOMMUNE - Tokyo x Yorkshire exchange transmission 2018: https://youtu.be/dIpzU71LAQQ

## Expresividad funcional

La idea de la expresividad funcional consiste, en términos prácticos, en la manipulación del material musical mediante un lenguaje de transformaciones. Esta aproximación es propuesta por Spiegel (1981), quien plantea que “la descripción de la música en términos de un vocabulario conceptual orientado a la transformación ayudará a desarrollar un vocabulario y una sintaxis más apropiados para la descripción, comprensión y creación de experiencias en el tiempo, para todas las artes temporales autorreferenciales, de las cuales la música es un ejemplo muy puro”. Para propósitos de esta tesis, entenderé este “vocabulario-sintaxis más apropiado” como un medio sofisticado de exploración de niveles semánticos musicales.
