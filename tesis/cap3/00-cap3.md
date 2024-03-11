# RTG: Una biblioteca de transformaciones

RTG se trata, en primera instancia, de una herramienta para la generación y manipulación de estructuras rítmicas. Me interesa explorar estructuras geométricas con cualidades rítmicas notables,[^buenosRitmos] a través de un método igualmente geométrico. RTG incluirá funciones generadoras de patrones rítmicos, utilizando algoritmos de fundamento geométrico como los ritmos euclidianos de Toussaint (2005) o los ritmos “balanceados” y “bien-formados” de Milne et al. (2016). En segundo lugar, estos patrones deben poder transformarse uno a otro mediante una operación que convierta un patrón rítmico arbitrario en una transformación de patrones.[^operación] Se utilizará provisionalmente [SuperDirt](https://github.com/musikinformatik/SuperDirt/), el motor de audio de Tidal Cycles, para reproducir muestras de sonido sobre los patrones generados. En los términos presentados, se trata de una herramienta para la exploración del espacio de las estructuras rítmicas.

[^buenosRitmos]: La búsqueda de “buenos ritmos” está en el corazón de la investigación de Toussaint (2020), misma que se enfoca en las relaciones entre los aspectos geométricos y computacionales del ritmo.

[^operación]: La definición de una “operación” general capaz de adaptarse a los diferentes tipos de patrón o estructura rítmica corresponde al uso de una type class (clase de tipos) y la definición de instancias de esta para cada tipo de patrón. Las type classes son uno de los principales aportes de Haskell a la teoría de lenguajes de programación (Wadler y Blott 1989; Hudak et al. 2007). Ejemplo de dicha operación para el caso particular de los ritmos euclidianos: \url{https://gist.github.com/ninioArtillero/d57269464bd24600593e16530ca59cf7}

Se seguirán los siguientes principios provenientes de la teoría de la computación: 

* Estilo de programación funcional para probar la correctud del código.
* Programación funcional reactiva para trabajar la semántica del tiempo continuo. 
* El diseño denotativo, que parte de una especificación detallada y procura el uso de abstracciones matemático-computacionales para su implementación.
* Modelos generativos geométricos como funciones básicas del lenguaje.

Con esta librería propongo una semántica que permita la traducción del pensamiento
geométrico-espacial en la generación de patrones rítmicos mediante texto. 
Es decir, trabajará al nivel de la organización de materiales
sonoros, y no al nivel de la síntesis de sonido.


1. Implementar RTG (Ritmo, Tiempo y Geometría): una biblioteca de programación con el concepto de transformación geométrica como abstracción de la entidad musical. Tendrá dos aportes cuyo sustento es el objetivo que unifica la tesis:

   1. Evidenciar como el uso de principios de la programación funcional aporta al diseño de una librería creativa que favorezca la expresividad.

   2. Establecer un vínculo teórico y práctico entre la geometría y el ritmo.



