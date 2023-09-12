import Control.Concurrent
import Control.Monad (forever, replicateM_)
import System.Environment
import System.IO

-- USO DEL PROGRAMA --

-- Este programa se llama con argumentos numéricos de la siguiente forma:
-- runhaskell numberStream.hs cyc x y ...
-- Donde cyc es decimal y (x,y,...) son números enteros.
-- Imprime "x" x veces en cyc segundos, y simultáneamente
-- imprime "y" y veces en cyc segundos.

-- Para compilarlo correr
-- ghc numberStream.hs
-- y, desde la misma ubicación, llamarlo de la siguiente forma
-- ./numberStream 60 5 7

-- Con estos argumentos se imprime, al stdout, "5" cinco veces
-- y "7" siete veces en un minuto repartidos parejamente.
-- Se puede introducir una cantidad arbitraria de argumentos.
-- El primero determina la duración del ciclo en segundos y los siguientes
-- las subdivisiones del mismo ciclo que correran en paralelo.

main = do
  -- En un programa en Haskell sólo se ejecutan aquellas acciones
  -- que se encuentren dentro de la variable main.
  -- Llamamos acciones a los valores de tipo <<IO a>> (donde "a" es una variable de tipo).
  -- <<IO>> es una constructor de tipos, es decir <<IO a>> es un tipo de valores.
  -- A este constructor se le llama la mónada input-output.
  -- En Haskell esta sirve para separar la pureza de las funciones
  -- abstrayendo los efectos secundarios.

  -- Para pasar el resultado directo a stdout
  hSetBuffering stdout NoBuffering

  args <- getArgs -- obtenemos los argumentos del programa en una lista de Strings
  let cyc = read (head args) :: Float -- el primer argumento determina la duración del ciclo en segundos
      numbers = map read (tail args) :: [Int] -- lo siguientes argumentos son los números a imprimir
      numQuantity = length numbers -- cantidad de números a imprimir

  -- 'MVar a' es un tipo de datos (polimórfico) que implementa
  -- una "variable mutable" que contiene un valor de tipo 'a'.
  -- Se trata de una una ubicación de memoria
  -- que se utiliza para sincronizar procesos concurrentes.
  var <- newEmptyMVar

  -- La siguiente linea hace prácticamente todo el trabajo del programa
  -- por lo que desarrollaré su significado en seguida.
  mapM_ (forkIO . forever . numberStream var cyc) numbers

  -- mapM_ aplica una función que produce una acción (su primer argumento)
  -- a toda una lista de valores (su segundo argumento, numbers),
  -- produciendo una lista de acciones que realiza en secuencia.
  -- Además descarta el valor de retorno de dicho grupo de acciones.

  -- En este caso el primer argumento de mapM_, (forkIO . forever . numberStream var cycle),
  -- es la función que resulta de la composición (operador .) de tres funciones.
  -- En una composicion se evalua primero la función del extremo derecho y el valor resultante
  -- se le pasa a la siguiente función (un tipo de pipeline).

  -- La primera función de la composición es "numberStream var cyc".
  -- Se trata de un ejemplo del "currying": la aplicación parcial
  -- de una función de muchos argumentos (numberStream, definida abajo, de 3 argumentos)
  -- nos da una funcion de menos variables (numberStream var cycle,
  -- toma un entero como argumento). Esta guarda un entero en la variable mutable.

  -- La siguiente función, forever, produce una acción
  -- que repite su argumento (otra acción) indefinidamente
  -- Finalmente la función forkIO crea un nuevo thread para su argumento (una acción)

  -- El resultado es tantos threads escribiendo simultáneamente a la misma variable inmutable
  -- como números tenemos de argumentos.

  -- Esto presenta un problema: debido a que la variable sólo puede contener un valorm,
  -- esta estrategia impide la simultaneidad (que sucedería, por ejemplo, cuando un número
  -- es múltiplo de otro).

  -- Mientras esto sucede el thread principal imprime los valores en tandas (o lo intenta)
  -- tan pronto como están disponibles.
  forever $ printResults numQuantity var

-- Aquí termina el código con efectos secuntarios (main).
-- A continuación defino las funciones (puras) auxiliares.

printResults :: Int -> MVar String -> IO ()
printResults n var =
  replicateM_
    n
    (takeMVar var >>= putStr)

numberStream :: MVar String -> Float -> Int -> IO ()
numberStream var cyc num = do
  putMVar var (show num) -- función que añade datos al MVar y lo bloquea
  threadDelay $ eventDuration cyc num -- determina la pausa del thread (antes de poder volverse a ejecutar)

eventDuration :: Float -> Int -> Int
eventDuration cyc num = round (milliSecondsPerCycle / cyclePartition)
  where
    milliSecondsPerCycle = cyc * 1000000
    cyclePartition = fromIntegral num -- hacerlo Float para usarlo en la división
