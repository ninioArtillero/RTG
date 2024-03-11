# Ritmo, tiempo y geometría

En este capítulo se discutirá el aspecto matemático-geométrico de RTG. Ahondaremos en el programa de Erlangen y posibles rutas de investigación futura.

## Espacio de los ritmos

RTG se propone como un instrumento expresivo para live coding enfocado a la generación de patrones rítmicos.
En otras palabras, se trata de una herramienta para explorar el espacio de ritmos a través de la geometría.
Aquí aludimos a la noción de exploración de un espacio conceptual dado por @Wiggins2015marco.

## Ritmo y Geometría

Los "ritmos euclidianos" son ejemplo que considero clave para la perspectiva y marco teórico de este trabajo de tesis. @Toussaint2005Euclidean describe una implementación del algoritmo de Euclides que permite generar una familia de patrones, en la que se incluyen ritmos de diversas tradiciones como casos particulares. Estos ritmos son ejemplos de lo que Toussaint denomina como "timelines": estructuras rítmicas, típicamente tocadas por instrumentos de percusión, que caracterizan algunos estilos de música popular y tradicional. Este ejemplo muestra como un modelo matemático-computacional puede ayudar a organizar datos musicológicos y, al ser implementado, producir una herramienta para la creación musical. Este algoritmo está implementado en varios contextos, por ejemplo: en la sintaxis de patrones en Tidal, en algunos _plugins_ de secuenciamiento y en el [paquete Bjorklund](https://github.com/redFrik/Bjorklund) para el lenguaje de programación SuperCollider.

## Tiempo y Geometría

Como vimos en el primer capítulo, en Tidal Cycles los patrones están implementados como funciones del tiempo.
La manipulación de estas funciones de tiempo se da a su vez a través de otras funciones que los transforman.
Esta caracterización permite abordar el espacio de estas funciones y sus transformaciones como una geometría.
Aquí "geometría" es entendida en el sentido del [programa de Erlangen](https://en.wikipedia.org/wiki/Erlangen_program) como el conjunto de invariantes bajo un [grupo](#grupo).
Esta conceptualización permite extender la noción de geometría más allá del estudio de figuras planas o sólidas en el espacio euclidiano.
Por el momento me limitaré a mencionar que la relevancia de este concepto es que posibilita hacer un estudio abstracto de estos lenguajes como entornos geométricos, de los que se desprende la siguiente preguntas:

> ¿Cómo describir la geometría implícita en los lenguajes tipo Tidal? Y ¿qué implica esta estructura geométrica respecto al diseño y uso de estos lenguajes?

Esta pregunta apunta a una hipótesis de trabajo: la existencia de un vínculo entre el pensamiento matemático-geométrico y la creación musical en el contexto del Live Coding.
Esto implica un estudio geométrico que describa a nivel formal las estructuras que produce.
Para se ocupará el estudio computacional que se abordó en el [capítulo 1](#cap1) y el uso de abstracciones para la manipulación y creación de música.

> ¿Qué características tiene la representación de patrones rítmicos o temporales en la programación funcional? ¿De qué depende?
