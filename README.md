# Ritmo, Tiempo y Geometría

Una investigación que explora la relación entre la programación funcional, el pensamiento geométrico y los patrones rítmicos.

Se compara un conjunto de lenguajes para la programación musical embebidos en Haskell para estudiar la abstracción computacional de la _entidad musical_ y sus implicaciones en la _expresividad_ (musical).

Este repositorio tiene el propósito de documentar el proceso de investigación, componer el texto de la tesis y servir de plataforma para el desarrollo de la librería **RTG**.

## Metas

1. Crear un lenguaje de dominio especifico embebido en Haskell, basado en la noción de transformación geométrica, para la generación de patrones rítmicos. 
   1. Definir operaciones con efectos perceptuales significativos, es decir, que trasciendan la aleatoriedad en favor de una coherencia estructural entre los argumentos del código y los resultados sonoros (ver Toussaint 2020).
   1. Permitir el uso de grupos abstractos para generar patrones con una relevancia que vaya más allá de la metáfora (Soria 2022): proyectar estructuras espaciales en secuencias temporales.
2. Explorar las nociones de _expresividad_ del código relevantes para el ámbito de la programación al vuelo (live coding).
3. Desarrollar un sistema que permita la transformación simultánea de varios niveles estructurales de la música (tentativo).

## Críticas pertinentes y respuestas provisionales

> La investigación aborda aspectos relacionados con las matemáticas, las ciencias de la computación, la práctica artística y el desarrollo tecnológico
> ¿Cuál es su aspecto central o principal aporte? ¿Qué papel juegan estos ámbitos?

El papel de cada ámbito se puede aclarar con un artificio: presentaré sus roles como etapas secuenciales.

**Ciencias de la computación**

La primera etapa de este proyecto se conformó por un clavado profundo en la teoría de lenguajes de programación a través del estudio de Haskell.
Durante este proceso he identificado la genealogía de mi investigación en la interrelación del trabajo de Alex McLean, Conal Elliot y Paul Hudak,
cuyo hilo conductor es el uso de la _programación funcional_ en la práctica creativa y creación multimedia.
De este tema se desprende el campo de estudio en que he situado esta investigación.[^1]
El fruto central de esta etapa ha sido la identificación de los conceptos de _abstracción_ y _expresividad_ como puntos de articulación de mi marco teórico.

[^1] Cuyos esfuerzos de legitimación se han concentrado en la _Workshop on Functional art, music, modeling and design_ (FARM), celebrado anualmente en el marco de la _International Conference on Functional Programming_.

**Desarrollo tecnológico**

El principal aporte de mi investigación es RTG como una propuesta innovadora para la creación musical con código.
En esta segunda etapa, me concentraré por programar el _core_ de RTG:

1. Implementación de ritmos euclidianos
1. Implementación de ritmos balanceados
1. Implementación de ritmos bien-formados
1. Definir instancias de grupo (una _type class_) para los diferentes tipos de patrones rítmicos
1. Definir una sintaxis para asignar muestras de sonido a los patrones.
1. Programar una comunicación con SuperDirt para sonificar los patrones.

Al definir instancias de grupo para cada tipo se imposibilita, por ejemplo, transformar un ritmo euclidiano con un ritmo balanceado.
Una primera aproximación es definir un único tipo general en que cada patrón rítmico esté implementado con una función.

**Práctica artística**

La definición de instancias de grupo es el punto en que se determina el efecto de la noción de transformación.
Corresponde a definir, para cada tipo de patrón rítmico, la operación que permite combinarlos.
La retroalimentación con la práctica artística, propia y de colegas, será fundamental para ajustar estas definiciones para lograr resultados interesantes.

Mi intención es continuar trabajando en RTG como herramienta central de mi práctica de _live coding_.

**Matemáticas**

La inspiración de RTG está en el _programa de Erlangen_. Este programa de investigación matemática propone el estudio de la geometría
a través del estudio de los grupos de transformaciones (o _simetrías_).
La última etapa corresponde a la exploración de las posibilidades matemáticas de RTG.
De momento queda fuera del alcance de la tesis.
La idea es que RTG sirva para proyectar estructuras espaciales en patrones rítmicos desplegados en el tiempo.
Similarmente, grupos abstractos generales podrían ser usados para generar nuevas estructuras rítmicas.


> Parece no haber una distinción clara entre el _live coding_ y la composición algorítmica.
> ¿Cuál es el ámbito específico de RTG?

Se trata principalmente de una librería para _live coding_. 
Las librerías y sistemas de _live coding_ pueden ser usadas como herramientas en la composición. 
La distinción clave está en el requerimiento de que su interfaz (API) sea capaz de producir y modificar el sonido en tiempo real (o de comunicarse con un sistema capaz de ello).

De momento estoy pensando la expresividad en el contexto de la interpretación en vivo. Habrá una distinción fundamental entre
la expresividad de un lenguaje de programación y la expresividad de un lenguaje de _live coding_ que tendré que desarrollar en la tesis.


> ¿Qué importancia tiene usar Haskell para la implementación de RTG?
> En principio se podría usar cualquier lenguaje de programación.

Respecto al aspecto teórico: 
Me interesa poner a prueba las aseveraciones de Paul Hudak respecto al poder expresivo de Haskell para la música (y en general para la programación multimedia y creación de lenguajes de dominio específico).
Este aspecto podría abordarse periféricamente en la tesis pues depende de un estudio más extenso de los lenguajes existentes.

Respecto a la implementación de RTG:
Al estar embebido en Haskell, RTG no es independiente de este.
La sintaxis y poder expresivo de Haskell son parte integral de RTG.
