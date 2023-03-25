# Objeto y objetivo

## Objeto de estudio

La tesis abordará un estudio de las siguientes librerías de Haskell para la creación musical:

* [Tidal Cycles](https://tidalcycles.org/) : Creada por Alex McLean y desarrollada por un amplio grupo de colaboradores. Tiene el concepto de _patrón algorítmico_ como semántica de las expresiones.
* [Euterpea](https://www.euterpea.com/): Librería de Haskell diseñada por Paul Hudak y Donya Quick, basada en los conceptos centrales de la música occidental y los principios de la programación funcional. Esta librería es acompañante del libro [The Haskell School of Music](https://www.cs.yale.edu/homes/hudak/Papers/HSoM.pdf) 
* [Conductive](https://hackage.haskell.org/package/conductive-base): Escrita por Renick Bell, utiliza la metáfora de un conductor de orquesta. Recientemente fue traducida a una librería de JavaScript llamada [Konduktiva](http://konduktiva.org/).

La elección de los lenguajes es fruto de mi investigación reciente y obedece a los siguientes motivos: 

1. Creadas en Haskell aprovechando principios que se desprenden de la programación funcional: compilador capaces de interpretar código de alto nivel de abstracción en operaciones numéricas eficientes, estilo declarativo/denotativo, funciones a todos los niveles de abstracción, sin _efectos secundarios_, con _evaluación floja_ (para estructuras de datos infinitas), abstracciones matemáticas de patrones computacionales (teoría de categorías).
2. Son parte de una genealogía que relaciona la programación funcional con la programación multimedia:
    $$
    \begin{aligned}
    &\text{Paul Hudak} &\longrightarrow &\text{Conal Elliot} 
    &\longrightarrow &\text{Alex McLean} \\
    &\text{Programación Funcional en Haskell} &\longrightarrow &\text{Functional Reactive Programming}
    &\longrightarrow &\text{Patrón Algorítmico} \\
    &\text{Euterpea} &\ &\text{Motion Graphics Libraries} &\ &\text{Tidal Cycles}
    \end{aligned}
    $$
3. Su código es abierto.
4. Amplia documentación (disponibilidad de referencias): libros, comunidades virtuales, artículos académicos, etc.

Otros casos que quedan excluidos, que constituyen referentes respecto a cuestiones de diseño generales:

* [Extempore](https://extemporelang.github.io/): Es un lenguaje de programación cuyo compilador acepta expresiones válidas de dos lenguajes, Scheme (Lisp) y xtlang (lenguaje tipo C propio de Extempore), con el objetivo de permitir flexibilidad dinámica (expresividad) y manejo numérico de bajo nivel. 
* [Sonic Pi](https://sonic-pi.net/): Lenguaje diseñado para la enseñanza de la programación y como instrumento musical para músicos profesionales. Su sintaxis está basada en Ruby.

La comparación se enfocará principalmente a nivel de la _especificación_ del lenguaje para
identificar elementos de diseño que impacten positivamente en la expresividad del lenguaje
como herramienta creativa o instrumento musical. Paralelamente
un análisis de su _implementación_ tiene el objetivo de servir de referencia para el trabajo propio. 


## Objetivo de la investigación

El desarrollo de una librería para la creación musical, escrita en Haskell, que haga un aporte
en 2 aspectos cuya prueba es el objetivo central de la tesis:

1. Evidenciar como el uso de principios de la programación funcional aporta al diseño de una API creativa,
    permitiendo que los lenguajes de programación tengan una expresividad similar a 
    un instrumento musical.
2. Establecer un vínculo teórico y práctico entre la geometría y el ritmo.


Se seguirán los siguientes principios provenientes de la teoría de la computación: 

* Estilo de programación funcional para probar la correctud del código.
* Programación funcional reactiva para trabajar la semántica del tiempo continuo. 
* El diseño denotativo, que parte de una especificación detallada y procura el uso de abstracciones matemático-computacionales para su implementación.
* Modelos generativos geométricos como funciones básicas del lenguaje.

Con esta librería propongo una semántica que permita la traducción del pensamiento
geométrico-espacial en la generación de patrones rítmicos mediante texto. 
Es decir, trabajará al nivel de la organización de materiales
sonoros, y no al nivel de la síntesis de sonido.

