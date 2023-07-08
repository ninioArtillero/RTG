# Ritmo, Tiempo y Geometría

Una investigación que explora la relación entre la programación funcional, el pensamiento geométrico y los patrones rítmicos.

Se compara un conjunto de lenguajes para la programación musical embebidos en Haskell para estudiar la abstracción computacional del fenómeno musical y que implica está abstracción en la expresividad (musical).

Este repositorio tiene el propósito de documentar el proceso de investigación
y servir de plataforma para el desarrollo de la librería **RTG**.

## Metas

1. Crear un lenguaje de dominio especifico embebido en Haskell, basado en la noción de transformación geométrica, para la generación de patrones rítmicos. 
   1. Definir operaciones con efectos perceptuales significativos, es decir, que trasciendan la aleatoriedad en favor de una coherencia estructural entre los argumentos del código y los resultados sonoros (ver Toussaint 2020).
   1. Permitir el uso de grupos abstractos para generar patrones con una relevancia que vaya más allá de la metáfora (Soria 2022): proyectar estructuras espaciales en secuencias temporales.
2. Explorar las nociones de _expresividad_ del código relevantes para el ámbito de la programación al vuelo (live coding).
3. Desarrollar un sistema que permita la transformación simultánea de varios niveles estructurales de la música (tentativo).

## Críticas pertinentes y respuestas provisionales

> La investigación se ha alejado de la práctica artística y el desarrollo técnologico, enfocándose en la teoría.
> ¿Cuál es su aspecto central o principal aporte?

El principal aporte se encuentra en el desarrollo tecnológico. 
RTG es una propuesta innovadora para la creación musical con código.

> Parece no haber una distinción clara entre el _live coding_ y la composición algorítmica.
> ¿Cuál es el ámbito específico de RTG?


> ¿Qué importancia tiene usar Haskell para la implementación de RTG?
> En principio se podría usar cualquier lenguaje de programación.

Respecto al aspecto teórico: 
estoy investigando las aseveraciones de Paul Hudak respecto al poder expresivo de Haskell para la música (y en general para la programación multimedia y creación de lenguajes de dominio específico).
Este aspecto teórico podría abordarse sólo periféricamente en la tesis,
pues depende de un estudio más extenso de los lenguajes existentes.

Respecto a la implementación de RTG:
Al estar embebido en Haskell, RTG no es independiente de este.
La sintaxis y poder expresivo de Haskell son parte integral de RTG.
