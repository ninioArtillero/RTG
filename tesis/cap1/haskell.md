## Haskell

Haskell es un lenguaje de programación funcional [puro](#pure) con [evaluación floja](#lazy) y un sistema de tipos [Hindley-Milner](#hindley-milner) con [clases de tipos](#type-classes).

A diferencia de otros lenguajes cuyo origen y diseño se encontraron sujetos a condiciones de la industria y el mercado,
la creación de Haskell responde a la necesidad de un grupo de académicos por tener un estándar que permitiera compartir y comparar sus investigaciones en programación funcional pura con evaluación floja.
Entre sus metas fundacionales están dos que siguen vigentes y han hecho de Haskell un lenguaje de vanguardia: servir de plataforma para la investigación en lenguajes de programación y estar disponible de manera libre .
Desde su nacimiento a finales de los años ochenta su desarrollo ha sido excepcional.
Durante sus poco más de treinta años de existencia ha desbordado lentamente el ámbito académico hacia aplicaciones en la industria. Destacando por las garantizas formales que permiten, por ejemplo, de probar la correctud de un programa y lo sucinto y expresivo de su código [@Hudak2007history].

Recíprocamente, esto ha significado una presión de mejora constante para sus implementaciones, de la que GHC (_Glasgow Haskell Compiler_) es el estándar _de facto_. 
El aumento de usuarios, servicios y aplicaciones (que dependen de la estabilidad del lenguaje) ha hecho aparecer una tensión creativa entre el impulso innovador y la necesidad práctica en el seno de una comunidad resiliente al cambio [@PeytonJones2017Escape].

Para la presente tesis el foco está en la expresividad del lenguaje más que en la capacidad de probar propiedades de los programas. 
A saber, ambos factores son consecuencia del formalismo que sustenta al lenguaje y sus prácticas idiomáticas.
En esta cuestión, seguimos la pista sentada por Hudak:

> Using a high-level language to express musical ideas is, of course, not new. 
> But Haskell is unique in its insistence on purity (no side effects), and this alone makes it particularly suitable for expressing musical ideas. 
> By focusing on what a musical entity is, rather than on how to create it, we allow musical ideas to take their natural form as Haskell expressions. 
> Haskell’s many abstraction mechanisms allow us to write computer music programs that are elegant, concise, yet powerful. [-@Hudak2018Haskell, p. xi]

En esta sección se describirán las características del lenguaje de programación Haskell 
haciendo énfasis en sus implicaciones para la creación de librerías de programación para la música.

_**NOTA: ¿Cuál es el criterio de selección de estas características?
Tiene que ver con el capítulo de expresividad y abstracción.**_

### Syntaxis y funciones

La clave para entender la sintaxis de Haskell es que la aplicación de funciones tiene la máxima precedencia.
Esto quiere decir que el lenguaje agrupa las evaluaciones de funciones, dándoles preferencia por sobre otros operadores.
Por ejemplo, al definir la función `squareSum` con la función `square` y el operador de suma no se requiere ningún paréntesis.

```haskell
square x = x * x

--x^2 + y^2
squareSum x y = square x + square y
```

Similarmente, la precedencia de los operadores aritméticos se corresponde con la precedencia de las operaciones matemáticas.
Por ello, el anterior ejemplo de puede reescribir de la siguiente manera:

```haskell
squareSum x y = x * x + y * y
```

Esta sintaxis resuena con la aplicación de funciones matemáticas abstractas, típicamente escritas como $f(x,y)$.
El paréntesis y las comas entre argumentos son reemplazados por espacios.

La aplicación de funciones se realiza de izquierda a derecha y de afuera hacia adentro, en semejanza con el [cálculo lambda](#lambda).

### Funciones puras {#pure}

Una función pura es aquella para la que un valor de entrada produce siempre el mismo valor de salida.
Por si mismo esto da al código una ventaja que favorece su reutilización:
los detalles de implementación de una función pura resultan superfluos para cualquier programa o función que haga uso de ella (o, "que la llame").
Quién la llama sólo se interesa por los valores de salida que produce en base a las entradas,
de forma que lo que sea que la función en cuestión hace en sus adentros es irrelevante.

Una definción equivalente a la anterior es que una función pura es aquella que no tiene _efectos secundarios_.
Esto significa que no modifica ningún valor externo a ella durante la producción de su salida.
Los programas que utilizamos en el día a día tienen un valor para nosotros por su capacidad de
crear y modificar archivos de texto, mostrar imágenes en pantalla, ingresar valores en bases de datos y demás efectos secundarios.
Un programa funcional puro, siendo incapaz de esto, nos resultaría completamente inútil.
^[Irónicamente los efectos secundarios son los de valor primario para unx usuarix.]
Pero este no es el caso; por dos razones.
La primera es que la impureza no es del todo excluída.
Lo que sucede en la práctica es que la parte impura del programa, correspondiente a la modificación de estados, es reducida al mínimo.
La segunda es que, de hecho, existe una elegante solución que hace puro lo impuro.
Para ello se utiliza el concepto de [mónada](#contexto), implementado en Haskell como una [clase de tipos](#type-classes).
La idea intuitiva detrás del uso de una mónada para modelar la modificación de estados puede parecer un artilugio:
pensar la totalidad del mundo como parte de la entrada de la función.
En principio esto querría decir que un programa funcional puro es incapaz de hacer trabajo alguno;
Las funciones puras reducen la superficie de error en el código evitando que el uso de estado compartido en la lógica interna de un programa:
compartir estado hace muy difícil pensar sobre un programa complicado, siendo necesario garantizar que no haya conflicto entre las funciones que acceden a un mismo valor.
En particular esto implica que las variables, sea que representen valores atómicos o estructuras de datos, son siempre constantes;
por si mismo este principio impide el uso de _loops_ para la modificación iterativa de una variable, técnica central para la programación imperativa.

En resumen, el código de programas funcionales no trata de llevar la cuenta de estados
sino que expresa el computo como una concatenación de trasformaciones.

### Evaluación floja {#lazy}

2. Tiene *evaluación floja* que permite, por ejemplo, definir estructuras de datos infinitas sobre las que la solicitud de una función permite un cálculo finito (al que están sujetos todos los algoritmos computacionales para considerarse efectivos). La evaluación floja significa que no se realiza ninguna operación hasta que se pide un resultado explícitamente.

La evaluación floja o evaluación normal es una estrategia en que se reduce primero la
expresión exterior y las expresiones interiores (o sub-expresiones) sólo son evaluadas cuando sus valores son requeridos.
El modelo del cálculo lambda utiliza tradicionalmente este tipo de evaluación [referencia].

El estudio de la evaluación floja fue uno de lo motores centrales que impulsaban la investigación en lenguajes funcionales,
misma que dio pie a la creación de haskell [@Hudak2007history].
Una de sus consecuencia centrales es que permite que se puedan manipular estructuras de datos infinitas,
dando como consecuencia la posibilidad de diseñar funciones con mayor capacidad de modularización [@Hughes1989Why].
La estructuras de datos infinitas no producen un cálculo interminable,
por el contrario no se calcula nada sobre ellas hasta que otra función requiere valores contenidos en la misma.
Es decir, el cálculo se hace únicamente bajo demanda.

La evaluación floja es ambivalente respecto a la eficiencia.
Por un lado, evita realizar el calculo de expresiones que no se ocupan durante el procedimiento.
Por el otro, para funcionar implementa un sistema de promesas llamado _thunks_, que en casos
de cálculos muy grandes puede saturar rápidamente la memoria.


### Tipos

Haskell es de tipado estático, lo que quiere decir que cada _expresión_ tiene un _tipo_.
^[Una expresión en un lenguaje tipado es cualquier conjunto de símbolos correspondiente a un valor de un tipo específico. Similar al [cálculo lambda](#lambda) el  cálculo puede considerarse una sucesión de reducciones hasta una formal normal.]

### Sistema de tipos Hindley-Milner {#hindley-milner}

### Clases de tipos {#type-classes}

En retrospectiva de sus creadores, serían las _type classes_ (clases de tipos) el aporte más notable de Haskell al
diseño de lenguajes de programación [@Hudak2007history;@PeytonJones2017Escape].

Las clases de tipos implementan la abstracción de comportamientos.
Por ejemplo, está la clase de los números `Num` que incluye a todos los tipos de datos que se pueden sumar y multiplicar.
O la clase de todos los tipos de datos que se pueden ordenar `Ord`.

En algunos casos puede resultar trivial. Lo que no es trivial es la capacidad de reutilizar código en diversos contextos.

#### Funtor, Aplicativo y Mónada {#contexto}

Modelan la existencia de un contexto computacional.
