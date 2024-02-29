## Haskell

### Características centrales del lenguaje

#### Evaluación floja {#lazy}

La evaluación floja o evaluación normal es una estrategia en que se reduce primero la
expresión exterior y las expresiones interiores (o sub-expresiones) sólo son evaluadas cuando sus valores son requeridos.
El estudio de la evaluación floja fue uno de lo motores centrales que impulsaban la investigación en lenguajes funcionales,
misma que dio pie a la creación de haskell [@Hudak2007history].
Una de sus consecuencia centrales es que permite que se puedan manipular estructuras de datos infinitas,
dando como consecuencia la posibilidad de diseñar funciones con mayor capacidad de modularización [@Hughes1989Why].

El modelo del cálculo lambda utiliza tradicionalmente este tipo de evaluación [referencia]..

#### Clases de tipos {#type-classes}

En retrospectiva de sus creadores, serían las _type classes_ (clases de tipos) el aporte más notable de Haskell al
diseño de lenguajes de programación [@Hudak2007history;@PeytonJones2017Escape].
