# Lenguajes de programación para la música {#cap1}

En este capítulo haré una breve introducción a los aspectos tecnológicos del _live coding_, para contextualizar la caracterización de sus intrumentos: los lenguajes de programación. 
Discutiremos el funcionamiento de las librerías Conductive, Euterpea, Tidal Cycles y RTG como ejemplos de librerías destinadas a la creación musical.

Haré una distinción cuidadosa de los siguientes conceptos técnicos (con traslapes no triviales): API, lenguaje de programación, sistema de live coding, lenguaje de dominio específico (embebido) y biblioteca de programación.


## Live Coding

El _live coding_, como práctica musical, consiste en escribir y modificar código fuente en vivo para producir sonido. Al ser simultáneamente escritura y producción de sonido, el _live coding_ difumina la distinción tradicional entre compositor e interprete, entrando en un diálogo con la computadora. Para una introducción detallada al live coding recomiendo consultar a @VillasenorRamirez2017Live y @Blackwell2022Live.


## Lenguajes de dominio específico embebido


Un _domain specific language_(DSL o lenguaje de dominio específico) es un lenguaje de programación especializado para un dominio de aplicación. Cómo ejemplo clásico está el lenguaje SQL para bases de datos.
DSL se define en contraposición a lenguajes de uso general (GPL) que están diseñados para cumplir cualquier función. Entre los GPL se cuentan lenguajes como Common Lisp, Python y Haskell.

Una subconjunto de DSLs son los _markup language_, como LaTeX, HTML y Markdown, utilizados para dar formato a textos.

Un _embbeded domain specific language_ (eDSL) es un DSL programado al interior de otro lenguaje de programación.

Tidal Cycles (Tidal) es un lenguaje de dominio específico embebido en el lenguaje de programación funcional Haskell. En términos técnicos, [Tidal es una librería de tipos y funciones en Haskell](https://hackage.haskell.org/package/tidal) que es cargada en el compilador interactivo GHCi. Las líneas (o bloques) de código de Tidal son enviadas como mensajes a GHCi. En este contexto, GHCi es esencialmente un programa que traduce código escrito en Tidal a mensajes de control. Por defecto estos mensajes son recibidos y ejecutados por [SuperDirt](https://github.com/musikinformatik/SuperDirt) que es el sistema generador de sonido diseñado para Tidal. En resumen, "Tidal Cycles" tiene las siguientes connotaciones:

1. Un lenguaje de dominio específico
2. Una librería de tipos y funciones
3. Sistema modular integrado por un editor de texto, un interprete del lenguaje y un generador de sonido.

Debido a su condición de sublenguaje, Tidal hereda la estructura sintáctica y capacidades de Haskell. Esta estructura se observa en el uso de operadores binarios como  "+" o en la evaluación de funciones: separando los argumentos por espacios o usando el operador "$" que evalúa una función tomando todo lo que está a la derecha como argumento. Se pueden usar otros elementos del lenguaje para operar sobre las funciones de Tidal, como expresiones lambda, asignación de variables, uso de condicionales o creación de nuevas funciones.

## Introducción a Haskell

```haskell {#mycode .numberLines}
-- Pattern match
sumarTres n = n + 3

-- Lambda
sumarTres = \n -> n + 3

-- Section
sumarTres = (+3)
```

Los [tres ejemplos anteriores](#mycode) son definiciones equivalentes de la misma función. Estas se pueden escribir, correspondientemente, como "" :

1. La función `sumarTres`{.haskell} aplicada a `n`{.haskell} produce `n+3`{.haskell}.
2. La función `sumarTres`{.haskell} es una función que aplicada a `n`{.haskell} produce `n+3`{.haskell}.
3. La función `sumarTres`{.haskell} es la función _más tres_.
