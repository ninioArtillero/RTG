# Functional Reactive Programming

Es un paradigma de programación, basado en la programación funcional de Alonso Church y Haskell Curry, orientado al diseño de librerías de programación a partir de la *denotacion* (*especifícación* del lenguaje, de manera independiente de la implementación).

Este concepto se encuentre en los fundamentos computacionales de Tidal Cycles como lenguaje de programación.

De acuerdo su creador, [Conal Elliot](https://youtu.be/j3Q32brCUAI), se basa principalmente en dos principios:

1. Denotación simple y precisa.
2. Tiempo continuo.

El primero refiere al uso de estructuras algebraicas en la creación de lenguajes de programación.
En Haskell se utilizan a traves de tipos abstractos modelados en las *typeclasses*.
Esto asegura que es posible probar propiedades generales de la especificación del lenguaje y verificar su implementación.
Las forma moderna en que se establece la FRP está fundamentada en las características de Haskell como lenguaje de programación.

El uso de tiempo continuo (*flows*) en lugar de tiempo discreto (*streams*), es el elemento clave de este paradigma.
Depende de la implementación específica de los números reales.
Actualmente en Haskell se utiliza el tipo *Double*, que implíca pérdidas en precisión.
