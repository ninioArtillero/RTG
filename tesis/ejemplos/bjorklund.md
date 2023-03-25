# Algoritmo de Bjorklund

Este algoritmo es descrito por [@Toussaint2005Euclidean] para generar la familia 
de ritmos euclidianos. 
A continuación presento su implementación en distintos lenguajes.

## Haskell

El algoritmo de Bjorklund es implementado por una función recursiva. La función `euclideanPattern` la invoca para
producir un ritmo euclidiano.

```haskell
euclideanPattern :: Int -> Int -> [Int]
euclideanPattern onsets pulses = concat $ bjorklund front back
  where front = replicate onsets [1]
        back = replicate (pulses - onsets) [0]

bjorklund :: [[Int]] -> [[Int]] -> [[Int]]
bjorklund front back =
  if (length back) > 1
    then bjorklund newFront newBack
    else front ++ back
  where newFront = zipWith (++) front back
        newBack = diffList front back

-- AUXILIARES

-- Produce una lista con los elementos no emparejables.
diffList :: [a] -> [a] -> [a]
diffList xs ys
  if lx > ly 
    then drop ly xs
    else = drop lx ys
  where lx = length xs
        ly = length ys

```

## Swift

Esta es una adaptación de la implementación de [Jeff Holtzkener](https://medium.com/code-music-noise/euclidean-rhythms-391d879494df), la cual sirvió de referencia para mi implementación en Haskell.
Tanto Swift como Haskell son estáticamente tipado y capaces de inferir tipos. Ambos lenguajes utilizan la notación de flecha `->`, aunque en diferentes niveles: 
en Haskell para la declaración del tipo de la función y en Swift como parte de la sixtaxis de la definición de la función.
Aquí el modificador `private` es parte del [sistema de control de acceso de Swift](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/accesscontrol/#Function-Types)
e indica que la función `bjorklund` es local únicamente al ámbito de su declaración. Aquí el papel de `zipWith (++)` para generar `newFront` lo juega un _while-loop_, que como efecto colateral
modifica `back` (en términos del código anterior esto equivaldría a utilizar `newBack = fronto ++ back` en la llamada recursiva).

```swift
func euclideanPattern (onsets: Int, pulses: Int) -> [Int] {
    let front:[[Int]] = Array(repeating: [1], count: onsets)
    let back:[[Int]] = Array(repeating: [0], count: pulses - onsets)
    return bjorklund(front: front, back: back).flatMap {$0}
}

private func bjorkLund (front: [[Int]], back: [[Int]]) -> [[Int]] {
    var back = back
    var front = front
    
    guard back.count > 1 else { return front + back }
    
    var newFront = [[Int]]()
    while front.count > 0 && back.count > 0 {
        newFront.append(front.popLast()! + back.popLast()!)
    }
    
    // back fue actualizado por el loop anterior debido a popLast
    return bjorklund(front: newFront, back: front + back)
}
```

## JavaScript

El siguiente código es una traducción directa de mi implementación en Haskell. Utiliza arreglos (_Arrays_): la estructura de datos en JavaScript que se corresponde con las listas en Haskell.
En general el código de JavaScript describe un programa como la ejecución secuencial de instrucciones (_statements_). Sin embargo, las declaraciones de funciones 
son accesibles a cualquier instrucción en el ámbito (scope) global, independientemente de su ubicación, gracias a que JavaScript las iza ([_hoist_](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions#calling_functions)).

```javascript

function euclideanPattern(onsets, pulses) {
  const front = Array(onsets).fill([1]);
  const back = Array(pulses - onsets).fill([0]);
  const bjorklundResult = bjorklund(front, back);
  return bjorklundResult.flat();
}

function bjorklund(front, back) {
  if (back.length > 1) {
    const newFront = zipWith(append, front, back); 
    const newBack = diffList(front, back);
    return bjorklund(newFront, newBack);
  } else {
    return front.concat(back);
  }
}

// AUXILIARES

function diffList(xs, ys) {
  const lx = xs.length;
  const ly = ys.length;
  if (lx > ly) {
    return xs.slice(ly);
  } 
  else {
    return ys.slice(lx);
  }
}

// Las siguientes son funciones de orden superior para combinar dos Arrays.
// Simulan el funcionamiento de las correspondientes funciones de la librería estándar de Haskell.

function zip(xs, ys) {
  let lx = xs.length;
  let ly = ys.length;
  if(lx <= ly) {
    return xs.map((x,i) => [x,ys[i]]);
  }
  else {
    return ys.map((y,i) => [xs[i],y]);
  }
}

function zipWith (f, xs, ys) {
  return zip(xs, ys).map(([x,y]) => f(x,y)); 
}


function append(xs, ys) { xs.concat(ys) };

// Utilizando zipWith con append como primer argumento, podemos combinar las listas
// manteniendo un sólo nivel de anidación en las llamadas recursivas de bjorklund.


```

La siguiente es una adaptación de la [implementación de Michael Kontogiannis](https://github.com/mkontogiannis/euclidean-rhythms). 
Aquí se actualizan variables mediante _loops_ para lograr el mismo efecto en un estilo imperativo más idiomático de JavaScript (aunque, en realidad, este 
código es compatible con TypeScript). Otra diferencia es que todas las definiciones e instrucciones están contenidas en el cuerpo de la función `euclideanPattern`. 

```typescript

function euclideanPattern(onsets: number, pulses: number) {
  // Manejo de casos extremos
  if (onsets < 0 || pulses < 0 || pulses < onsets) {
    return [];
  }

  // Inicializar Arrays 
  let front = new Array(onsets).fill([1]);
  let back = new Array(pulses - onsets).fill([0]);

  let frontLength = front.length;
  let minLength = Math.min(frontLength, back.length);

  let loopThreshold = 0;

  // Repetir hasta que algún array tenga longitud mayor que 1
  while (minLength > loopThreshold) {
    // El loopThreshold es cero sólo en la primera repetición.
    if (loopThreshold === 0) {
      loopThreshold = 1;
    }

    // Recorre la longitud del array más corto, concatenando elementos de ambos.
    // Similar a zipWith(append, xs, ys), con la diferencia de que actualiza 
    // (algunos) elementos de front en lugar de construir un nuevo array. 
    for (let x = 0; x < minLength; x++) {
      front[x] = [...front[x], ...back[x]];
    }

    // Si front es el mas corto este se queda como antes y
    // se actualiza back descartando los elementos sobrantes.
    if (minLength === frontLength) {
      back = back.slice(minLength);
    }
    // En caso contrario, actualizar front para incluir sólo los sub-arreglos 
    // construidos por el for-loop anterior y
    // actualizar back como el arreglo de los elementos restantes. 
    else {
      back = front.slice(minLength);
      front = front.slice(0, minLength);
    }
    
    // Actualizar variables de control
    frontLength = front.length;
    minLength = Math.min(frontLength, back.length);
  }

  // Construir el arreglo final 
  const pattern: number[] = [...front.flat(), ...back.flat()];

  return pattern;
}
```
