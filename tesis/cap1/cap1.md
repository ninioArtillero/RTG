# Título tentativo {#titulo}

*Interfaces geométricas en lenguajes funcionales de dominio específico para la generación de patrones rítmicos*

Algunos ejemplos de citación. Como dicen @McLean2020Algorithmic y @Spiegel1981Manipulations, la generación de patrones nos permite trabajar a un mayor nivel de abstracción.

## Preguntas de investigación

En esta sección desarrollaré algunas preguntas que delimitan el alcance de esta investigación:

> ¿Qué modelos matemáticos existen implementados en estos lenguajes para la generación de ritmos? ¿Qué otros no han sido implementados?

Esta pregunta surge de un ejemplo que considero clave para la perspectiva y marco teórico de este trabajo de tesis. En su artículo de 2005 ["The Euclidean Algorithm Generates Tradicional Musical Rhythms"](http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf), Godfried Toussaint describe una implementación del algoritmo de Euclides que permite generar una familia de patrones, en la que se incluyen ritmos de diversas tradiciones como casos particulares. Estos ritmos son ejemplos de lo que Toussaint denomina como "timelines": estructuras rítmicas, típicamente tocadas por instrumentos de percusión, que caracterizan algunos estilos de música popular y tradicional. Este ejemplo muestra como un modelo matemático-computacional puede ayudar a organizar datos musicológicos y, al ser implementado, producir una herramienta para la creación musical. Este algoritmo está implementado en varios contextos, por ejemplo: en la sintaxis de patrones en Tidal, en algunos plugins de secuenciamiento y en el [paquete Bjorklund](https://github.com/redFrik/Bjorklund) para el lenguaje de programación SuperCollider.

En Tidal Cycles y otros lenguajes de dominio específico relacionados, que por conveniencia provisionalmente llamaré "lenguajes tipo Tidal", los patrones están  implementados como funciones del tiempo. La manipulación de estas funciones de tiempo se da a su vez a través de otras funciones que los transforman. Esta caracterización permite abordar el espacio de estas funciones y sus transformaciones como una geometría. Aquí "geometría" se toma en un sentido derivado del [programa de Erlangen](https://en.wikipedia.org/wiki/Erlangen_program): el conjunto de invariantes bajo un grupo de transformaciones. Esta conceptualización  permite extender la noción de geometría más allá del estudio de figuras en el espacio euclidiano. En este documento no abordaré la definición formal de "grupo de transformaciones", mismo que fue un concepto central en mi [tesis de licenciatura](https://repositorio.unam.mx/contenidos/ficha/328736). Por el momento me limitaré a mencionar que la relevancia de este concepto es que posibilita hacer un estudio abstracto de estos lenguajes como entornos geométricos, de los que se desprende la siguiente pregunta:

> ¿Cómo describir la geometría implícita en los lenguajes tipo Tidal? Y ¿qué implica esta estructura geométrica respecto al diseño y uso de estos lenguajes?

Esta pregunta apunta directamente a la hipótesis de trabajo que menciono en la siguiente sección: la existencia de un vínculo entre el pensamiento matemático-geométrico y la creación musical en el contexto del Live Coding. En el caso particular de Tidal Cycles me interesa hacer un estudio geométrico que describa a nivel formal las estructuras que produce y un estudio computacional (*i.e.* desde la ciencia de la computación) de las abstracciones que le permiten ejecutar ideas musicales. Este estudio empieza necesariamente por las características de Haskell, en el cual Tidal está embebido, de las cual interesan principalmente dos:


1. Es un lenguaje de *programación funcional pura*, la cual es un paradigma de programación basado en el [*cálculo lambda*](https://en.wikipedia.org/wiki/Lambda_calculus): una abstracción de los procesos computacionales a través del lenguaje de las funciones y su composición en que las estructuras de datos no son modificadas. En la práctica esto implica que el código de programas funcionales no lleva la cuenta de estados, sino que se trata de una concatenación de trasformaciones.
2. Tiene *evaluación floja* que permite, por ejemplo, definir estructuras de datos infinitas sobre las que la solicitud de una función permite un cálculo finito (al que están sujetos todos los algoritmos computacionales para considerarse efectivos). La evaluación floja significa que no se realiza ninguna operación hasta que se pide un resultado explícitamente.

Sobre esta línea, el estudio de la implementación de lenguajes tipo Tidal parte de las siguientes preguntas:

> ¿Qué características tiene la representación de patrones rítmicos o temporales en la programación funcional? ¿De qué depende?

> ¿Qué ventajas supone este paradigma de programación en el diseño de lenguajes? ¿Cuáles son sus peculiaridades en el Live Coding?

