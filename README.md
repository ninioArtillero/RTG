# Ritmo, Tiempo y Geometría

"ritmoTG" es una investigación que explora la relación entre la abstracción computacional, el pensamiento geométrico y los patrones rítmicos.

Se estudia un conjunto selecto de lenguajes para la programación musical embebidos en Haskell para comprender la abstracción computacional de la _entidad musical_ y sus implicaciones en la _expresividad_ (musical).

Este repositorio tiene el propósito de documentar el proceso de investigación, contener el texto de la tesis y servir de plataforma para el desarrollo de **RTG**: una biblioteca de Haskell para la generación y manipulación de patrones rítmicos con el concepto de _geometría_ como abstracción central de su interfaz.

## Instalación

Este proyecto utiliza el administrador de paquetes [Nix](https://nixos.org/manual/nix/stable/) para lograr la reproducibilidad del entorno de desarrollo, necesario para interactuar con la biblioteca, de manera multi-plataforma. Otra ventaja en este contexto es que permite abstraer la instalación de [_haskell_](https://www.haskell.org/downloads/) y reducir la compilación de las dependencias gracias al [caché de binarios de Nix](https://cache.nixos.org), que cubre gran parte de los [paquetes de su repositorio](https://search.nixos.org/packages). A continuación se revisa la instalación de los componentes requeridos.

1. [Instalar Nix Package Manager](https://nixos.org/download). Nix provee unos scripts de instalación que se pueden ejecutar con los siguientes comandos:
   1. En Linux: `sh <(curl -L https://nixos.org/nix/install) --daemon`.
   1. En MacOS: `sh <(curl -L https://nixos.org/nix/install)`.
   1. En Windows es necesario tener [WSL](https://learn.microsoft.com/es-es/windows/wsl/install) con [systemd activado](https://devblogs.microsoft.com/commandline/systemd-support-is-now-available-in-wsl/): `sh <(curl -L https://nixos.org/nix/install) --daemon`.
1. Instalar SuperDirt (motor de audio).
   1. Instalar [SuperCollider](https://supercollider.github.io/downloads.html) y, para tener todos los sintetizadores predefinidos, los [sc3-plugins](https://supercollider.github.io/sc3-plugins/). Ambos son accesibles desde el administrador de paquetes en varias distribuciones de Linux.
   1. Con el interprete de SuperCollider (`sclang`) andando, ejecutar `Quarks.checkForUpdates({Quarks.install("SuperDirt", "v1.7.3"); thisProcess.recompile()})`.
1. Clonar este repositorio.
1. Desde la raíz del repositorio ejecutar `nix-shell`. Nix procedera a descargar todas las dependencias de la biblioteca (este proceso puede tardar un poco y se hace sólo en la primera invocación). Si termina exitosamente, iniciará un ambiente de _shell_ con las herramientas y dependencias necesarias para RTG.
1. En seguida, ejecutar `cabal build` para compilar la biblioteca. Si hay error, favor de [levantar un issue](https://github.com/ninioArtillero/ritmoTG/issues/new/choose) con el _output_.
1. Finalmente `cabal repl` inicia una sesión del interprete (`ghci`) con la biblioteca cargada. Ahora se puede interactuar con las funciones exportadas por cada uno de los módulos.
1. Salir del interprete con `:quit` y del _nix-shell_ con `exit`.

## Uso

### Preámbulo

Abrir SuperCollider (o `sclang` desde una terminal) y ejecutar `SuperDirt.start` para iniciar el motor de audio y cargar las muestras. En caso de problemas de reproducción, se puede utilizar [este archivo de configuración](https://raw.githubusercontent.com/musikinformatik/SuperDirt/develop/superdirt_startup.scd) para iniciar SuperDirt.

Abrir una terminal en el directorio raíz del repositorio, iniciar una `nix-shell` y enseguida un `cabal repl`.

### Utilizar la biblioteca

Con la instancia de SuperDirt andando podemos ejecutar `play 0.4 (indicatorVector diatonic)` en el _repl_.

TODO: Se espera la aparición de mensajes `late` en SuperCollider. Falta implementar el buffering  de eventos.

TODO: Falta independizar los _streams_ de eventos. De momento es necesario interrumpir el proceso tecleando `Ctrl+c` para ejecutar otro patrón.

### Funciones implementadas de la biblioteca

Utilizando la función `indicatorVector` se pueden transformar los patrones definidos en el módulo `Sound.RTG.Ritmo.Pattern` en secuencias binarias que `play` puede procesar. La sintaxis de play es `play <ciclos-por-segundo> <patron-binario>`.

`play 0.5 (indicatorVector gypsy)`

`play 0.5 (indicatorVector clave)`

Se pueden producir y tocar ritmos euclidianos utilizando `euclideanPattern`:

`play 0.6 (euclideanPattern 5 8)`

`play 0.4 (euclideanPattern 7 12)`, equivalente a `play 0.4 (indicatorVector diatonic)`

También podemos calcular el balance y la paridad (de acuerdo a Milne et. al 2015) de los patrones:

`balance diatonic`
> `λ> 0.9617215439384111`

`balance crowded`
> `λ> 0.0` (totalemente imbalanceado)

`balance clave`
> `λ> 0.9835215599415212`

`balance wholeTone`
> `λ> 0.9999999999999999` (`1`, perfectamente balanceado)

`evenness diatonic`.
> `λ> 0.9888464387394665`

`evenness crowded`
> `λ> 4.020306624191591e-17` (`0`, totalmente disparejo)

`evenness clave`
> `λ> 0.9877149016523218`

`evenness wholeTone`
> `λ> 1.0`

## Metas

1. Crear una biblioteca de Haskell, basada en la noción de geometría que se desprende del [Programa de Erlangen](https://es.wikipedia.org/wiki/Programa_de_Erlangen), para la generación de patrones rítmicos. 
   1. Definir familias de patrones rítmicos a partir de modelos y algoritmos geométricos (el ejemplo paradigmático son los ritmos euclidianos).
   1. Proveer una estructura de grupo para cada familia de patrones rítmicos, mediante una operación binaria que cumpla los axiomas de grupo abstracto. 
   1. Desarrollar procedimientos y operaciones que produzcan efectos musicales significativos, es decir, que trasciendan la arbitrariedad en favor de una coherencia estructural entre los argumentos del código y los resultados sonoros.
2. Explorar las nociones de _expresividad_ del código relevantes para el ámbito de la programación al vuelo (live coding).
3. Desarrollar la biblioteca en un lenguaje de dominio específico embebido en que su acción se extienda a otras dimensiones musicales (altura, espectro, timbre y estructura).

## Críticas pertinentes y respuestas provisionales

> La investigación aborda aspectos relacionados con las matemáticas, las ciencias de la computación, la práctica artística y el desarrollo tecnológico
> ¿Cuál es su aspecto central o principal aporte? ¿Qué papel juegan estos ámbitos?

El papel de cada ámbito se puede aclarar con un artificio: presentaré sus roles como etapas secuenciales.

### Ciencias de la computación

La primera etapa de este proyecto se conformó por un clavado profundo en la teoría de lenguajes de programación a través del estudio de Haskell.
Durante este proceso he identificado la genealogía de mi investigación en la interrelación del trabajo de Alex McLean, Conal Elliot y Paul Hudak,
cuyo hilo conductor es el uso de la _programación funcional_ en la práctica creativa y creación multimedia.
De este tema se desprende el campo de estudio en que he situado esta investigación.[^1]
El fruto central de esta etapa ha sido la identificación de los conceptos de _abstracción_ y _expresividad_ como puntos de articulación de mi marco teórico.

[^1]: Cuyos esfuerzos de legitimación se han concentrado en la _Workshop on Functional art, music, modeling and design_ (FARM), celebrado anualmente en el marco de la _International Conference on Functional Programming_ (ICFP).

### Desarrollo tecnológico

El principal aporte de mi investigación es RTG como una propuesta innovadora para la creación musical con código.
En esta segunda etapa, me concentraré por programar el _core_ de RTG:

1. Implementación de ritmos euclidianos
1. Implementación de ritmos balanceados
1. Implementación de ritmos bien-formados
1. Definir instancias de grupo (_type class_ definida en `Data.Group`) para los diferentes _tipos_ de patrones rítmicos
1. Definir una sintaxis para asignar muestras de sonido a los patrones.
1. Programar una comunicación con SuperDirt para sonificar los patrones.

Al definir instancias de grupo para cada tipo se imposibilita, por ejemplo, operar un ritmo euclidiano con un ritmo balanceado.

Una aproximación alternativa es definir un único tipo de datos que unifique a las familias de patrones rítmicos.

### Práctica artística

La definición de instancias de grupo es el elemento característico de la biblioteca.
Corresponde a definir, para cada tipo de patrón rítmico, una operación que permite combinarlos.

Por otro lado la sintaxis de la biblioteca debe ser ergonómica y expresiva: permitir dinamismo en la generación, combinación y transformación de los patrones rítmicos en la composición y en la interpretación en vivo, de una forma previsible y coherente con la escucha.

La retroalimentación con la práctica artística, propia y de colegas, será fundamental para ajustar estas definiciones para lograr resultados interesantes en estos aspectos.

### Matemáticas

La inspiración de RTG está en el _programa de Erlangen_. Este programa de investigación matemática propone el estudio de la geometría
a través del estudio de los grupos de transformaciones (o _simetrías_).
La última etapa corresponde a la exploración de las posibilidades matemáticas de RTG.
De momento queda fuera del alcance de la tesis.
La idea es que RTG sirva para proyectar estructuras espaciales en patrones rítmicos desplegados en el tiempo.
Similarmente, grupos abstractos generales podrían ser usados para generar nuevas estructuras rítmicas.

### Otras preguntas

> Parece no haber una distinción clara entre el _live coding_ y la composición algorítmica.
> ¿Cuál es el ámbito específico de RTG?

Se trata principalmente de una biblioteca para _live coding_. 
Las biblioteca y sistemas de _live coding_ pueden ser usadas como herramientas en la composición. 
La distinción clave está en el requerimiento de que su interfaz (API) sea capaz de producir y modificar el sonido en tiempo real (o de comunicarse con un sistema capaz de ello).

De momento estoy pensando la expresividad en el contexto de la interpretación en vivo. Habrá una distinción fundamental entre
la expresividad de un lenguaje de programación y la expresividad de un lenguaje de _live coding_ que tendré que desarrollar en la tesis.


> ¿Qué importancia tiene usar Haskell para la implementación de RTG?
> En principio se podría usar cualquier lenguaje de programación.

Me interesa poner a prueba las aseveraciones de Paul Hudak respecto al poder expresivo de Haskell para la música y, en general, para la programación multimedia y creación de lenguajes de dominio específico.
Este aspecto podría abordarse sólo periféricamente en la tesis, pues depende de un estudio más extenso de los lenguajes existentes y su comparación.

Haskell es un lenguaje de origen académico, profundamente matemático y enfocado en la innovación. Constituye una plataforma ideal para el estudio de la abstracción computacional.

Respecto a la implementación de RTG:
Al estar embebido en Haskell, RTG no es independiente de este.
La sintaxis y poder expresivo de Haskell se vuelven parte integral de RTG.
