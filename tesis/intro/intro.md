# Introducción {.unnumbered}

La presente investigación está influenciada por el descubrimiento del _live coding_ durante mis estudios de maestría. 
En esta práctica encontré un convivencia armoniosa de mis intereses e inclinaciones. 
Mi pensamiento matemático encuentra satisfacción en la retroalimentación entre la escritura de código,
la creación de estructuras abstractas y la generación de sonido. 
Además, he disfrutado enormemente el encontrar nuevos problemas por resolver involucrado con el uso
de herramientas que no esconden los principios de su funcionamiento: los conejos blancos saltan por doquier felices. 
Resuena con mi inclinación política el hecho de que la comunidad del _live coding_ lleva en su seno los principios del software libre y de código abierto: en resonancia con la búsqueda de conocimiento como una práctica liberadora y esencialmente cooperativa.

Durante mis estudios de maestría en tecnología musical, como resultado de la introspección y
el diálogo con mi tutor el Dr. Jorge David García, decidí abordar aspectos políticos y poéticos de mi práctica como productor musical utilizando los conceptos de remix y sampleo. 
Estos me llevaron a tratar aspectos derivados de consideraciones
tecnológicas relacionadas con la grabación (pensada como escritura) y el medio digital.

Simultáneamente elaboré, para mis adentros, una representación _naive_ del fenómeno musical en
términos de dimensiones horizontal y vertical. Tradicionalmente asociadas a la melodía y armonía,
caractericé estas dimensiones como ritmo y sampleo.
Así, concebí la recontextualización de sonidos de acuerdo a disposiciones específicas
en el tiempo como fundamento de una práctica compositiva inspirada en el remix. 

Mis dudas respecto a la dimensión vertical, es decir la recontextualización de sonidos,
no están agotadas. Sin embargo, es mi intención abordar en esta investigación la dimensión
en la dimensión horizontal del "ritmo". 
Como punto de partida, partimos de una pregunta inocente: 
¿Cómo acomodar los sonidos en el tiempo? 
Esta pregunta tiene una variedad no acotada de respuestas en función tanto de las 
prácticas y ámbitos musicales, como de las consideraciones artísticas.
Mi respuesta se considera el uso de estructuras temporales predeterminadas, con capacidad
generativa en su combinación y transformación a manera de un lenguaje de alto nivel,
de manera similar a como concebí el sampleo.

Como primer ejemplo de esto encontramos al entorno Tidal Cycles, 
que lleva la generación de patrones rítmicos al ámbito textual.
Su uso me hizo interesarme por el live coding como un medio de improvisación y composición,
en que un lenguaje de alto nivel pudiera ser puesto en práctica.

Mi interés por los sistemas computacionales y la práctica de la programación, me alentaron a perseguir un estudio de las profundidades de Tidal.
Esto me ha llevado a estudiar el lenguaje en que está embebido, Haskell.
La historia de este lenguaje es relevante, pues se trata de una de las implementaciones más
representativas del modelo computacional del cálculo lambda en un lenguaje de programación.
Esto tiene fuertes implicaciones en la manera en que se piensa acerca de los programas, una
aproximación más cercana a las matemáticas.

Cabe resaltar que se me ha hecho evidente una relativa divergencia de las prácticas de la matemáticas y la programación. Un programador no necesariamente sabe matemáticas avanzadas, mientras que un matemático no necesariamente es un buen programador.
Empero, espero que mi visión aporte una luz intersticial.

Esta investigación tiene como objetivo indagar el papel del pensamiento matemático en el diseño de lenguajes para programación musical.

El objeto de estudio de este proyecto está en la intersección de dos aspectos de un conjunto de lenguajes de dominio específico (DSL por sus siglas en inglés) orientados a la generación de patrones rítmicos musicales. Estos aspectos son:

1. Las características de su implementación computacional.
2. Sus cualidades y ofrecimientos como herramientas de creación musical.

En otras palabras, **el objeto de estudio es el vínculo entre el diseño de estos lenguajes y las formas de creación que posibilitan**.

Como casos de estudio present un conjunto de lenguajes implementados como librerías de Haskell: Tidal Cycles, Conductive, Euterpea y ritmoTG. 
Cada lenguaje partiendo de una conceptualización difente de la __entidad musical_ enraizado en su diseño. 
Mi interés está en las estrategias de diseño de estos lenguajes y su relación con el uso que agencian como herramientas para la creación musical.

## Preguntas {.unlisted .unnumbered}

Un conjunto de preguntas detonantes para la investigación. En algunos casos ya han respuestas parciales que necesitan ser formalizadas en el manuscrito.

> ¿Cuáles parámetros matemáticos he identificado para la generación de ritmos cíclicos?

1. Balance: Mide cuanto difiere el centro de masa de un patrón circular del centro del círculo (cero da balance perfecto). Relacionado con el coeficiente de grado cero en la transformada discreta de Fourier para el vector de eventos.
2. Regularidad (*evenness*): Mide la cercanía a la regularidad perfecta (*perfect evenness*): cuando los elementos del patrón están separados por un mismo intervalo. Relacionada con el coeficiente de grado uno de la transformada de Fourier. Estadísticamente, este valor es equivalente la diferencia entre la varianza circular y la unidad. La regularidad perfecta implica balance perfecto (Milne et al. 2015).
3. Buena forma: sólo existen dos diferentes intervalos entre los eventos.
4. Sub-periodicidad: cuando un subconjuto del patrón genera a la totalidad del mismo.

> ¿Qué geometría tiene el espacio de patrones en Tidal Cycles?

Los patrones rítmicos en Tidal son funciones puras del tiempo que producen eventos (valuados en general en un tipo numérico).
Las funciones de segundo orden, transforman patrones en patrones.
Para poder caracterizar la geometría que emerge en Tidal, es fundamental identificar el conjunto completo de estas transformaciones.
Posteriormente es posible, en principio, definir un *grupo* de transformaciones a partir del cuál caracterizar la geometría.

> ¿Cómo el cálculo lambda permite representar la evolución de patrones rítmicos?

Esto se relaciona con la FRP: un modelo para representar la continuidad del tiempo y la reactividad de un comportamiento a eventos.

> ¿Cómo representar un ritmo como un ente evolutivo?

> ¿Existe una relación entre el cálculo lambda y el programa de Erlangen?

La idea central del cálculo lambda es que todo computo se puede representar mediante funciones puras. Por otro lado el programa de Erlangen propone que la geometría está caracterizada un grupo de transformaciones, es decir, un conjunto de funciones puras.


> ¿Cuál es el aspecto matemático que quiero experimentar musicalmente?

Abordar la composición desde la lógica de la geometría mediante el uso de patrones algorítmicos.

## Casos de estudio {.unlisted .unnumbered}

La tesis abordará un estudio de las siguientes librerías de Haskell para la creación musical:

* [Tidal Cycles](https://tidalcycles.org/) : Creada por Alex McLean y desarrollada por un amplio grupo de colaboradores. Tiene el concepto de _patrón algorítmico_ como semántica de las expresiones.
* [Euterpea](https://www.euterpea.com/): Librería de Haskell diseñada por Paul Hudak y Donya Quick, basada en los conceptos centrales de la música occidental y los principios de la programación funcional. Esta librería es acompañante del libro [The Haskell School of Music](https://www.cs.yale.edu/homes/hudak/Papers/HSoM.pdf) 
* [Conductive](https://hackage.haskell.org/package/conductive-base): Escrita por Renick Bell, utiliza la metáfora de un conductor de orquesta. Recientemente fue traducida a una librería de JavaScript llamada [Konduktiva](http://konduktiva.org/).

La elección de los lenguajes es fruto de mi investigación reciente y obedece a los siguientes motivos: 

1. Creadas en Haskell aprovechando principios que se desprenden de la programación funcional: compilador capaces de interpretar código de alto nivel de abstracción en operaciones numéricas eficientes, estilo declarativo/denotativo, funciones a todos los niveles de abstracción, sin _efectos secundarios_, con _evaluación floja_ (para estructuras de datos infinitas), abstracciones matemáticas de patrones computacionales (teoría de categorías).

2. Son parte de una genealogía que relaciona la programación funcional con la programación multimedia:

    |                | **Paul Hudak** $\longrightarrow$ | **Conal Elliott** $\longrightarrow$ | **Alex McLean **   |
    |----------------|---------------------------------:|:-----------------------------------:|:-------------------|
    | **Concepto**   |  Lenguajes multimedia en Haskell | Programación Reactiva Funcional     | Patrón Algorítmico |
    | **Aplicación** |                         Euterpea | Motion Graphics Libraries           | Tidal Cycles       |

3. Su código es abierto.

4. Amplia documentación (disponibilidad de referencias): libros, comunidades virtuales, artículos académicos, etc.

Otros casos que quedan excluidos, que constituyen referentes respecto a cuestiones de diseño generales:

* [Extempore](https://extemporelang.github.io/): Es un lenguaje de programación cuyo compilador acepta expresiones válidas de dos lenguajes, Scheme (Lisp) y xtlang (lenguaje tipo C propio de Extempore), con el objetivo de permitir flexibilidad dinámica (expresividad) y manejo numérico de bajo nivel. 
* [Sonic Pi](https://sonic-pi.net/): Lenguaje diseñado para la enseñanza de la programación y como instrumento musical para músicos profesionales. Su sintaxis está basada en Ruby.

La comparación se enfocará principalmente a nivel de la _especificación_ del lenguaje para
identificar elementos de diseño que impacten positivamente en la expresividad del lenguaje
como herramienta creativa o instrumento musical. Paralelamente
un análisis de su _implementación_ tiene el objetivo de servir de referencia para el trabajo propio. 

## Método

Como mencioné al introducir el objeto de investigación, y se evidencio al plantear las preguntas anteriores, trabajaré sobre dos aspectos de un grupo de lenguajes. Para conveniencia de la presente sección podemos llamarlos el aspecto tecnológico y el aspecto musical.

El estudio del código implica su lectura y su uso. El aspecto musical será abordado mediante una investigación documental en los trabajos de compositores y Live Coders que permita ahondar en las capacidades expresivas de estos lenguajes y entender sus límites. El aspecto tecnológico requiere el análisis de las funciones presentes en estos lenguajes y el desarrollo de otras resultantes de la exploración musical. Esto constituiría un aporte tecnológico tangible a la práctica del Live Coding y una retribución a la comunidad detras del mantenimiento y evolución de estos lenguajes.

Un elemento fundamental para el desarrollo de estás tecnologías es la apertura y liberación del código como un filosofía promovida por la [Free Software Foundation](https://www.gnu.org/philosophy/philosophy.html): se trata de proyectos personales o colaborativos de investigación y desarrollo tecnológico que son sujetos a la experimentación y aporte de terceros. Esta participación comunitaria es incentivada por la posibilidad legal de reapropiarse del código para crear versiones alternativas o implementar en otros proyectos o productos, comerciales o sin fines de lucro. Gracias a su licencia de software libre de Tidal Cycles es posible estudiar a profundidad su código.

En última instancia el objeto de estudio será abordado desde la geometría con la intención de proponer una estructura formal para su análisis y clasificación. El objetivo de esta aproximación es desarrollar una conceptualización que permita nuevas formas de pensar el Live Coding y la programación de herramientas y lenguajes musicales.

