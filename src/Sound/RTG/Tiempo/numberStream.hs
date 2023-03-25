import           Control.Concurrent
import           Control.Monad      (forever, replicateM_)
import           System.Environment
import           System.IO

-- USO DEL PROGRAMA --

-- Este programa se llama con argumentos numéricos de la siguiente forma:
-- runhaskell concurrent-mvar.hs 60 5 7

-- Para compilarlo correr
-- ghc concurrent-mvar.hs
-- y, desde la misma ubicación, llamarlo de la siguiente forma
-- ./concurrent-mvar 60 5 7

-- Con estos argumentos se imprime, al stdout, "5" cinco veces
-- y "7" siete veces en un minuto repartidos parejamente.
-- Se puede introducir una cantidad arbitraria de argumentos. para
-- El primero determina la duración del ciclo en segundos y los siguientes
-- las subdivisiones del mismo ciclo que correran en paralelo.


main = do
  -- En un programa en Haskell sólo se ejecutan aquellas acciones
  -- que se encuentren dentro de la variable main.
  -- Llamamos acciones a los valores de tipo <<IO a>> (donde a es una variable de tipo).
  -- <<IO>> es una type class, es decir <<IO a>> es un tipo de valores.
  -- A esta clase se le llama la mónada input-output.
  -- Esto permite separar la parte pura del código
  -- de aquella que tiene efectos secundarios.

  -- Pasar el resultado directo a stdout
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

  -- La función mapM_ abstrae una acción que toma como argumento un número
  -- para aplicarla a toda una lista de números.
  -- En concreto, mapM_ toma como primer argumento
  -- una función que produce una acción y la aplica a cada elemento
  -- de una lista (en este caso 'numbers'),
  -- olvidando el valor de retorno de dicha acción.
  -- El resultado es una secuencia de acciones.

  -- En este caso el primer argumento de mapM_, (forkIO . forever . numberStream var cycle),
  -- describe la composición de tres funciones, usando el operador (.)
  -- En un composicion se evalua primero la función del extremo derecho y el valor resultante
  -- se le pasa a la siguiente función.

  -- La última función de la composición es "numberStream var cyc".
  -- Se trata de un ejemplo del "currying": la aplicación parcial
  -- de una función de muchos argumentos (numberStream, definida abajo, de 3 argumentos)
  -- nos da una funcion de menos variables (numberStream var cycle,
  -- toma un sólo argumento).

  -- La siguiente función, forever, produce una acción
  -- que repite su argumento (otra acción) indefinidamente
  -- Finalmente la función forkIO crea un nuevo thread para su argumento (una acción)

  forever $ printResults numQuantity var

-- Aquí termina el código con efectos secuntarios (main).
-- A continuación defino las funciones (puras) auxiliares.

printResults :: Int -> MVar String -> IO ()
printResults n var = replicateM_ n
                      (takeMVar var >>= putStr)

numberStream :: MVar String -> (Float -> Int -> IO ())
numberStream var cyc num = do
  putMVar var (show num) -- función que añade datos al MVar y lo bloquea
  threadDelay $ eventDuration cyc num -- determina la pausa del thread antes de volverse a ejecutar


eventDuration :: Float -> Int -> Int
eventDuration cyc num = round ( milliSecondsPerCycle / cyclePartition )
      where milliSecondsPerCycle = cyc * 1000000
            cyclePartition = fromIntegral num -- hacerlo Float para usarlo en la división
