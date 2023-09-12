# Ritmo, tiempo y geometría

En este capítulo se discutirá el aspecto matemático-geométrico de RTG. Ahondaremos en el programa de Erlangen y posibles rutas de investigación futura.


## Espacio de los ritmos

El RTG intenta ser un instrumento expresivo para live coding, así como una herramienta para explorar el espacio de ritmos a través de la geometría. Aquí aludimos a la noción de exploración de un espacio conceptual dado por @Wiggins2017marco.

## Ritmo y Geometría

Los "ritmos euclidianos" son ejemplo que considero clave para la perspectiva y marco teórico de este trabajo de tesis. @Toussaint2005Euclidean describe una implementación del algoritmo de Euclides que permite generar una familia de patrones, en la que se incluyen ritmos de diversas tradiciones como casos particulares. Estos ritmos son ejemplos de lo que Toussaint denomina como "timelines": estructuras rítmicas, típicamente tocadas por instrumentos de percusión, que caracterizan algunos estilos de música popular y tradicional. Este ejemplo muestra como un modelo matemático-computacional puede ayudar a organizar datos musicológicos y, al ser implementado, producir una herramienta para la creación musical. Este algoritmo está implementado en varios contextos, por ejemplo: en la sintaxis de patrones en Tidal, en algunos plugins de secuenciamiento y en el [paquete Bjorklund](https://github.com/redFrik/Bjorklund) para el lenguaje de programación SuperCollider.

## Tiempo y Geometría

Como vimos en el primer capítulo, Tidal Cycles los patrones están  implementados como funciones del tiempo. La manipulación de estas funciones de tiempo se da a su vez a través de otras funciones que los transforman. Esta caracterización permite abordar el espacio de estas funciones y sus transformaciones como una geometría. Aquí "geometría" es entendida en el sentido del [programa de Erlangen](https://en.wikipedia.org/wiki/Erlangen_program) como el conjunto de invariantes bajo un grupo de transformaciones. Esta conceptualización permite extender la noción de geometría más allá del estudio de figuras planas o sólidas en el espacio euclidiano. En este documento no abordaré la definición formal de "grupo de transformaciones", mismo que fue un concepto central en mi [tesis de licenciatura](https://repositorio.unam.mx/contenidos/ficha/328736). Por el momento me limitaré a mencionar que la relevancia de este concepto es que posibilita hacer un estudio abstracto de estos lenguajes como entornos geométricos, de los que se desprende la siguiente pregunta:
> ¿Cómo describir la geometría implícita en los lenguajes tipo Tidal? Y ¿qué implica esta estructura geométrica respecto al diseño y uso de estos lenguajes?

Esta pregunta apunta directamente a la hipótesis de trabajo que menciono en la siguiente sección: la existencia de un vínculo entre el pensamiento matemático-geométrico y la creación musical en el contexto del Live Coding. En el caso particular de Tidal Cycles me interesa hacer un estudio geométrico que describa a nivel formal las estructuras que produce y un estudio computacional (*i.e.* desde la ciencia de la computación) de las abstracciones que le permiten ejecutar ideas musicales. Este estudio empieza necesariamente por las características de Haskell, en el cual Tidal está embebido, de las cual interesan principalmente dos:


1. Es un lenguaje de *programación funcional pura*, la cual es un paradigma de programación basado en el [*cálculo lambda*](https://en.wikipedia.org/wiki/Lambda_calculus): una abstracción de los procesos computacionales a través del lenguaje de las funciones y su composición en que las estructuras de datos no son modificadas. En la práctica esto implica que el código de programas funcionales no lleva la cuenta de estados, sino que se trata de una concatenación de trasformaciones.
2. Tiene *evaluación floja* que permite, por ejemplo, definir estructuras de datos infinitas sobre las que la solicitud de una función permite un cálculo finito (al que están sujetos todos los algoritmos computacionales para considerarse efectivos). La evaluación floja significa que no se realiza ninguna operación hasta que se pide un resultado explícitamente.

Sobre esta línea, el estudio de la implementación de lenguajes tipo Tidal parte de las siguientes preguntas:

> ¿Qué características tiene la representación de patrones rítmicos o temporales en la programación funcional? ¿De qué depende?

> ¿Qué ventajas supone este paradigma de programación en el diseño de lenguajes? ¿Cuáles son sus peculiaridades en el Live Coding?




Aún no se encuentra apropiadamente delimitado. Sin embargo presento algunos referentes que utilizaré para su construcción. Recordemos que los ritmos tienen maneras generativas de ser producidos [ver @Toussaint2005Euclidean, cap. 1; @Elliott1997Functional].

Esta investigación tiene las siguientes ramificaciones sobre el estudio de estos lenguajes de programación de patrones rítmicos:

1. Los aspectos tecnológicos en los que se fundamenta su diseño.
	1. Haskell, la programación funcional y la Functional Reactive Programming (FRP)
	2. Interfaz textual para la generación de patrones algorítmicos (concepto propuesto por Alex McLean).
2. Las derivaciones e implicaciones que tiene en la teoría musical occidental sobre el ritmo.
3. Su caracterización como implementación de modelos matemáticos y de estructura geométrica.

Para tratar los modelos matemáticos y geométricos del ritmo, así como su implementación, se construirá la parte matemático-musical-computacional del marco teórico a partir de libros recientes que abordan diversas aristas del tema. Estas fuentes me permitirán un contexto amplio para acotar conceptos como "ritmo", "metro", "modelo" y "patrón" en el contexto de implementaciones computacionales:

+ [Toussaint, G. T. (2020). The geometry of musical rhythm: What makes a “good” rhythm good?](https://www.taylorfrancis.com/books/9781351247771)
+ [Boenn, G. (2018). Computational Models of Rhythm and Meter. Springer International Publishing](https://doi.org/10.1007/978-3-319-76285-2)
+ [The Musical-Mathematical Mind. Patterns and Transformations (2017)](https://doi.org/10.1007/978-3-319-47337-6)

Sobre la relaciones entre la creación-generación de obra y el uso de lenguajes de programación (en particular programación funcional), el concepto de "patrón algorítmico" propuesto por Alex McLean se presenta como un primer elemento para esta parte del marco teórico. Resulta particularmente importante por su relación con el diseño de Tidal. Ademas hay un foro en internet en el cual se está conformado una comunidad de investigación alrededor del concepto:

+ [McLean, A. (2020). Algorithmic Pattern. Proceedings of the International Conference on New Interfaces for Musical Expression, 6](https://www.nime.org/proceedings/2020/nime2020_paper50.pdf)
+ [Tesis de Doctorado de McLean (2011)](https://slab.org/writing/thesis.pdf)
+ [Algorithmic Pattern Forum](https://forum.algorithmicpattern.org/)

Por ultimo, el programa de Erlangen es el componente principal de la parte geométrica de la investigación. [Este artículo](https://flm-journal.org/Articles/4C154BFE42449B32FFD61852283E7E.pdf) da una breve introducción a partir de su origen histórico.

## Matemáticas

Podemos tener ecuaciones matemáticas dentro del texto de la siguiente manera:

$$n \in \mathcal{N}$$

## Estado del arte
Platicas recientes con colegas del Seminario Permanente de Tecnología Musical me llevaron a identificar a un grupo de investigación dirigido por David Ogborn en la universidad MacMaster en el que participan los mexicanos Alejandro Franco, Luis Navarro y Jessica Rodriguez. Este grupo de investigación esta trabajando en el desarrollo de lo que llamo "lenguajes tipo Tidal", mismos que están siendo implementados en la plataforma de live coding en red [estuary](https://estuary.mcmaster.ca/), en la que se pueden utilizar desde el navegador de internet.

La siguientes ligas llevan a los repositorios de los correspondientes proyectos. En ellos basta leer el texto de la página principal para una descripción general. Se trata de proyectos de DSLs para la generación de patrones:

1. [Tidal Cycles](https://github.com/tidalcycles/Tidal)
2. [timeNot](https://github.com/AFrancoB/timeNot)
3. [Nanc-in-a-can Canon Generator](https://github.com/nanc-in-a-can/canon-generator)
4. [seis8s](https://github.com/luisnavarrodelangel/seis8s)
5. [TransMit](https://github.com/jac307/TransMit)
6. [CQenze](https://github.com/essteban/CQenze)

Siguiento esta [liga](#titulo) podemos ver el título tentativo.

Las notas al pie son una forma excelente de separar información del cuerpo del texto.[^1]
Y lo podemos hacer tantas veces como sea necesario.[^veces] También podemos aprovechar otras funciones para escribir notas al pie. ^[Como hacerlo de manera "inline" utilizando una sintaxis especial que no permite notas con saltos de línea. Aunque esta sintaxis es propia de Pandoc.]

[^1]: Por ejemplo, en este caso trasladamos esta información.

[^veces]: Y agregar más información relevante.
