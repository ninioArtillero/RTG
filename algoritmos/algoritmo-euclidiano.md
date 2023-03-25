# 1. El algoritmo de Euclides

En términos generales el algoritmo de Euclides es un proceso para calcular el máximo común divisor de una pareja de números enteros ( $\mathbb{Z}$ );
el planteamiento no tiene una significación directa para los números racionales ( $\mathbb{Q}$ )
o reales ( $\mathbb{R}$ ), donde contamos con una resolución infinita que permite dividir de manera exacta cualquier pareja de números.

Este es uno de los algoritmos documentados más antiguos; aparece en las primeras dos proposiciones del libro VII de "Los Elementos" de Euclides, escrito alrededor del 300 a.C.

Consiste en realizar una división con residuo (o algoritmo de la división) en cada iteración, tomando como *dividendo* y *divisor* del siguiente el *divisor* y *residuo* de la anterior, respectivamente, hasta llegar a una expresión con residuo cero. El resultado del algoritmo es el último residuo distinto de cero o, equivalentemente, el divisor de dicha última expresión con residuo cero.

Por ejemplo, para la pareja 16 y 24 el algoritmo procede como sigue:

$$\begin{aligned}
24 = 16(1) + 8\\
16 = 8(2) + 0\\
\end{aligned}$$

Dando como resultado 8, como el máximo divisor común.

Para el caso de la pareja 16 y 38 tenemos:

$$
\begin{aligned}
38 &= 16(2) + 6\\
16 &= 6(2) + 4\\
6 &= 4(1) + 2\\
4 &= 2(2) + 0\\
\end{aligned}
$$

En este caso el resultado es $2$.

Se puede visualizar el algoritmo de Euclides como la cobertura sucesiva de un rectángulo (en que la pareja representa las medidas de los lados) con cuadrados de tamaño máximo en cada paso.

## 1.1. Algoritmo de la división
La división con residuo (o algoritmo de la división) nos dice que para cualquier pareja de números enteros $m$ y  $k$ existe un único par de enteros $q > 0$ y $r < m$ tales que:

$$m = k(q) + r$$

Donde llamamos
* Cociente = $q$
* Residuo = $r$
* Dividendo = $m$
* Divisor = $k$.

Para el caso de la pareja 7 y 24, como divisor y dividendo respectivamente, obtenemos

$$ 24 = 7(3) + 3$$

O, intercambiado los papeles de la pareja, encontramos

$$7 = 24(1) -17$$

Esto aplica igualmente para números negativos y nos permite definir el módulo para cualquier dos pares de enteros.

## 1.2. Expresión recursiva

El algoritmo de Euclides tiene una elegante forma compacta utilizando una expresión recursiva.

Para dos enteros $m$ y $k$, donde $m > k$, definimos (en pseudo-código):

```
k,m en INT

EUCLID(k,m)
if k == 0
  then -> return m
  otherwise -> EUCLID(m mod k, k)
```

Con $k=16$ y $m=38$, los pasos del algoritmo recursivo son: `EUCLID(16,38) -> EUCLID(6,16) -> EUCLID(4,6) -> EUCLID(2,4) -> EUCLID(0,2) -> 2`

Notemos que $m \mod k$ es precisamente el residuo de dividir $m$ entre $k$, de donde se sigue su identidad con la descripción original del algoritmo.

En Common LISP, por ejemplo, este algoritmo se expresa así:

```[Common Lisp]
(defun euclid (k m)
  (if (equal k 0)
    m
    (euclid (mod m k) k)))
```

En Haskell

```[Haskell]
euclid :: Integral a => (a, a) -> a
euclid (0, m) = m
euclid (k, m)=euclid ((m `mod` k), k)
```

## 1.3. Expresión por sustracción

Una expresión alternativa del algoritmo es utilizando la resta de manera recursiva (en lugar de la división, utilizada implícitamente en la función `mod`). En pseudocódigo:

```
k,m en INT

EUCLIDs(k,m)
  if k == m
    then -> return k
  if k > m
    then -> EUCLIDs(k - m,m)
    otherwise -> EUCLIDs(k, m - k)
```

En Haskell:

```[Haskell]
 euclid :: Integral a => (a, a) -> a
 euclid (k, m)
    | k == m = k
    | k > m = euclid (k - m, m)
    | otherwise = euclid (k, m - k)
 ```

# 2. Complejidad computacional

En la descripción y análisis de algunos algoritmos hallaremos comúnmente "la notación de la gran O" (_Big O notation_) en representación de la complejidad computacional de una función o algoritmo que permite clasificarlo de acuerdo a su tiempo de operación y uso de memoria.
Pensando $n$ como la cantidad de datos ingresados a la función,
la notación de la gran O (escrita como $\mathcal{O}$),
nos índica una cota superior a la cantidad de operaciones necesarias para completar el algoritmo considerando el crecimiento de $n$.

Podemos denotar por $\mathcal{O}(1)$ a un proceso con una complejidad constante. Por ejemplo, la siguiente función (en pseudocódigo) realiza la misma cantidad de operaciones independientemente del tamaño de su argumento

```
X en List(INT)

firstNumberOne(X)
  if X(0)=1
    then -> true
    otherwise -> false
```

En el caso del algoritmo de Euclides (sustractivo) tenemos una complejidad del orden de $\mathcal{O}(n)$,
que significa que en el peor de los casos la cantidad de operaciones se incrementa linealmente con los datos ingresados.
Para verlo consideremos la pareja de enteros $(k,m)$ con $m>k$.
En cada iteración del algoritmo el valor de $m$ baja sustrayendo $k$ hasta que es menor que este y se invierten los roles, de donde se intuye que la cantidad de iteraciones se ve directamente afectada por la diferencia de los dos números, $n = m-k$.
Un caso representativo del máximo de pasos para completar el algoritmo es `EUCLIDs(1,m)`, donde tenemos un total de $m$ pasos.
Por lo que el número de iteraciones es asíntóticamente proporcional a $n=m-1$.

Un caso óptimo de funciones que se escalan bien con el incremento de sus entradas está representado por $\mathcal{O}(\log n)$, que denota un crecimiento logarítmico.
Por otro lado, el crecimiento exponencial o factorial, respectivamente $\mathcal{O}(2^n)$
y $\mathcal{O}(n!)$, serían casos en que el algoritmo tiene una fuerte limitación al escalar con una mayor cantidad de datos.

# 3. El algoritmo de Bjorklund

Consideremos el problema de distribuir parejamente 5 eventos en un espacio de 13 intervalos de tiempo de la misma duración. Esto se puede representar con un conjunto de 13 secuencias de 1 bit, correspondientes a 5 unos (los eventos) y 8 ceros (los intervalos de "silencio").

`[1] [1] [1] [1] [1] [0] [0] [0] [0] [0] [0] [0] [0]`

En este distribuimos los ceros con los unos para formar secuencias de 2 bits,  quedando un residuo de 3 secuencias de 1 bit:

`[1 0] [1 0] [1 0] [1 0] [1 0] [0] [0] [0]`

Repetimos el proceso, distribuyendo los ceros del residuo para formar secuencias de 3 bits, obteniendo ahora un residuo de dos secuencias de 2 bits:

`[1 0 0] [1 0 0] [1 0 0] [1 0] [1 0]`

Finalmente, se reparten nuevamente las secuencias del residuo para obtener:

`[1 0 0 1 0] [1 0 0 1 0] [1 0 0]`

Llegado el momento en que el residuo consiste de una sola secuencia, se considera terminado el algoritmo. La secuencia que corresponde a la distribución pareja de 5 pulsos en 13 intervalos corresponde a la concatenación de las secuencias al terminar el algoritmo, obteniendo en este caso:

`[1 0 0 1 0 1 0 0 1 0 1 0 0]`

Denotamos esta secuencia como `(5,13)`. El proceso es idéntico para el caso en que se tienen más unos que ceros, en cuyo caso los primeros residuos serán unos. Por ejemplo para obtener `(7,13)`, el algoritmo termina en la primera iteración:

```
[1] [1] [1] [1] [1] [1] [1] [0] [0] [0] [0] [0] [0]

[1 0] [1 0] [1 0] [1 0] [1 0] [1 0] [1]
```
Dando por resultado:

`[1 0 1 0 1 0 1 0 1 0 1 0 1]`

## 3.1. Equivalencia con Euclides

Observemos la secuencia de pasos del algoritmo de Bjorklund para 5 y 13, representado por las 3 secuencias de un bit `[1] [1] [1] [1] [1] [0] [0] [0] [0] [0] [0] [0] [0]`, y en seguida los correspondientes al algoritmo de Euclides para evidenciar que ambos presentan la misma _estructura_:

Bjorklund

```

[1 0] [1 0] [1 0] [1 0] [1 0] [0] [0] [0]

[1 0 0] [1 0 0] [1 0 0] [1 0] [1 0]

[1 0 0 1 0] [1 0 0 1 0] [1 0 0]

```

Euclides

```
13 = 5(2) + 3

5= 3(1) + 2

3= 2(1) + 1

```

# 4. Ritmos Euclidianos

Los ritmos euclidianos fueron definidos por [Toussaint (2005)](zotero://open-pdf/library/items/AAJJ3G3T) como una familia de patrones rítmicos (representados por vectores binarios) generados por el algoritmo de Björklund (matemáticamente equivalente al algoritmo de Euclides).
Este algoritmo distribuye de la forma más pareja posible un determinado número de ventos en un universo discreto, finito y homogéneo, mismo que es representado como un subconjunto del círculo ( $\mathbb{S}^1$ ).
Esto equivale a determinar una cuantización del tiempo, seleccionando un número finito de posiciones temporales, en las que se distribuyen lo más "parejamente posible" un determinado número de eventos sonoros.
La investigación de Toussaint muestra una sorprendente ubicuidad de este modelo en la música tradicional alrededor del mundo.

Este modelo inspiró la orientación de mi reciente proyecto de investigación de doctorado, en donde me encuentro estudiando otros modelos generativos, principalmente geométricos, para ritmos y patrones.

## 4.1. Representaciones

Denotamos a los ritmos euclidianos utilizando $E(k,m)$, con $k < m$,
donde $k$ corresponde a la cantidad de eventos sonoros y $m$ a la cantidad de puntos en el universo discreto.
Podemos formalizar la representación circular representando este universo discreto como las $m$ raíces complejas de la unidad: $z=\sqrt[m]{1}$
con $z\in\mathbb{C}$.

Estos ritmos tienen varias representaciones notacionales, por ejemplo, el ritmo $E(3,8)$:

Serie binaria: `[1 0 0 1 0 0 1 0]`

Notación musicológica, equivalente a la binaria pero más amigable visualmente: `[x . . x . . x .]`

*Inter onset intervals* (IOI), cada número corresponde a la distancia interválica entre los eventos: `[3 3 2]`

## 4.2. Rotación

El desplazamiento del inicio del patrón, o equivalentemente la rotación del círculo en el que representamos el espacio de intervalos, nos da como resultado variaciones de lo que podemos considerar una familia de ritmos.

Por ejemplo, $E(3,8,1)$ es igual a

`[0 1 0 0 1 0 0 1]`

# 5. Ejemplos

Tidal Cycles tiene integrados los ritmos euclideanos dentro de la "mini-notation".
Por ejemplo, la siguiente línea de código produce un _loop_ donde la primera mitad reproduce un aplauso con ritmo euclidiano $E(3,8)$,
seguido de un bombo con ritmo $E(5,8)$:

`d1 $ s "cp(3,8) bd(5,8)"`

En SuperCollider se puede usar el patron Pbjorklund2, instalando el quark "Bjorklund". [Este repositorio](https://github.com/theseanco/howto_co34pt_liveCode) es una introducción al live coding en SuperCollider con una sección sobre ritmos euclidianos.
