# Objeto de estudio

El objeto de estudio de este proyecto está en la intersección de dos aspectos de un conjunto de lenguajes de dominio específico (DSL por sus siglas en inglés) orientados a la generación de patrones rítmicos musicales. Estos aspectos son:

1. Las características de su implementación computacional.
2. Sus cualidades y ofrecimientos como herramientas de creación musical.

En otras palabras, **el objeto de estudio es el vínculo entre el diseño de estos lenguajes y las formas de creación que posibilitan**.

Un lenguaje de dominio específico (DSL) es un lenguaje de programación especializado para un dominio de aplicación. Como ejemplos están LaTex, para la composición de documentos de texto. DSL se define en contraposición a lenguajes de uso general (GPL) que están diseñados para cumplir cualquier función. Entre los GPL se cuentan lenguajes como Common Lisp, Python y Haskell.

El ejemplo emblemático del conjunto de lenguajes de dominio específico relevantes a este proyecto es [Tidal Cycles](http://tidalcycles.org/), abreviado como Tidal. Se trata de un lenguaje utilizado para *Live Coding*, práctica en la que se escribe o manipula código computacional para producir música. En este [video](https://youtu.be/-QY2x6aZzqc) de trece minutos Alex McLean muestra una selección de presentaciones musicales para ilustrar el panorama del Live Coding y elabora unos ejemplos de secuencias en Tidal, del cual es creador y principal desarrollador. Tidal permite la generación de patrones musicales con muy pocas líneas de código. Estos patrones incrementan en complejidad rápidamente a partir de la composición de *funciones de transformación*.

Tidal es un lenguaje de dominio específico embebido en el lenguaje de programación funcional Haskell. En términos técnicos, [Tidal es una librería de tipos y funciones en Haskell](https://hackage.haskell.org/package/tidal) que es cargada en el compilador interactivo GHCi. Las líneas (o bloques) de código de Tidal son envíadas como mensajes a GHCi. En este contexto, GHCi es esencialmente un programa que traduce código escrito en Tidal a mensajes de control. Por defecto estos mensajes son recibidos y ejecutados por [SuperDirt](https://github.com/musikinformatik/SuperDirt) que es el sistema generador de sonido diseñado para Tidal. En resumen, "Tidal Cycles" tiene las siguientes connotaciones:

1. Un lenguaje de dominio específico
2. Una librería de tipos y funciones
3. Sistema modular integrado por un editor de texto, un interprete del lenguaje y un generador de sonido.

Debido a su condición de sublenguaje, Tidal hereda la estructura sintáctica y capacidades de Haskell. Esta estructura se observa en el uso de operadores binarios como  "+" o en la evaluación de funciones: separando los argumentos por espacios o usando el operador "$" que evalúa una función tomando todo lo que está a la derecha como argumento. Se pueden usar otros elementos del lenguaje para operar sobre las funciones de Tidal, como expresiones lambda, asignación de variables, uso de condicionales o creación de nuevas funciones.

```haskell {#mycode .numberLines startFrom="100"}
sumarTres :: Num a => a -> a
sumarTres n = n + 3
```

Cómo podemos ver en la [segunda línea](#mycode), la función es definida mediante _pattern matching_.

| También podemos especificar que un texto
|       sea formateado exactamente como se escribe.
|          Esto es útil para, por ejemplo, escribir direcciones.

