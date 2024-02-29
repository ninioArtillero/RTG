## Programación Reactiva Funcional {#frp}

La programación reactiva funcional (FRP por sus siglas en inglés) es una metodología
para el diseño de sistemas que evolucionan en el tiempo basada en el uso de una
[denotación](#denotación) clara y concisa [@Elliot2015Essence].

Su formulación inicial utilizó Haskell, aunque el concepto trasciende el uso de un lenguaje de programación específico.
y estaba enfocada en abstracciones apropiadas para permitir la programación funcional de animaciones [@Elliot1997Functional].
Posteriormente la FRP fue reformulada utilizando las abstracciones más generales incluidas en Haskell como [clases de tipos](#type-classes).

En la FRP la especificación de la librería toma el tiempo como un parámetro valuado en los números reales.
Si bien el continuo de los números reales no puede ser representado en la computadora digital, 
este es aproximado por los tipos datos _densos_ tales como puntos flotantes (`Float` y `Double` en Haskell)
o racionales (`Rational`, que corresponden a fracciones de números enteros).
La elección del tipo de dato utilizado para el cálculo es relevante en la implementación: 
es hasta el último momento en que los valores son requeridos que el mismo se ejecuta,
por lo que esta aproximación está relacionada con la [evaluación floja](#lazy).


Term 1

:   Definition 1

Term 2 with *inline markup*

:   Definition 2

        { some code, part of Definition 2 }

    Third paragraph of definition 2.


