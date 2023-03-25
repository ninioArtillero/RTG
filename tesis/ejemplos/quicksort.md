# Algoritmo Quicksort

Es bien conocido y se encuentra [ampliamente documentado](https://en.wikipedia.org/wiki/Quicksort): 
permite ordenar una lista de elementos
en un tiempo computacional promedio de $\mathcal{O}(n \log n)$
Se trata de un proceso recursivo cuya operación básica consiste 
en seleccionar un elemento pivote y dividir la lista en dos: 
los elementos menores y los elementos mayores (que el pivote).

## Haskell

En este caso tenemos un estilo declarativo en la construcción
de las listas para la llamada recursiva (que utiliza la definición de listas por comprensión)
y en el uso de la expresión _let_. Este es un ejemplo paradigmático que muestra
la elegancia expresiva de Haskell que se encuentra en el libro [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/recursion#quick-sort).

```haskell
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let pivot = [x]
      smallerSort = quicksort [ y | y <- xs, y < x ]
      biggerSort  = quicksort [ y | y <- xs, y >= x ]
  in  smallerSort ++ pivot ++ biggerSort
```


## TypeScript

El siguiente código, [escrito por el _copilot_ de Github](https://youtu.be/RDd71IUIgpg?t=52),
ejemplifica un estilo imperativo más idiomático de JavaScript. 

```typescript
function quicksort(arr: number[]): number[] {

    if (arr.length <= 1 {
        return arr;
    }

    const pivot = arr[0];
    const left = [];
    const right = [];

    for (let i = 1; i < arr.length; i++) {
        if (arr[i] < pivot) {
            left.push(arr[i]);
        } else {
            right.push(arr[i]);
        }

    }
    
    return [...quicksort(left), pivot, ...quicksort(right)];
}

```

