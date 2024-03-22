# Ritmo, Tiempo y Geometría

RTG es una bibliotega de Haskell para la creación de patrones rítmicos musicales con el concepto de _geometría_ como abstracción central de su API. Como parte de mi investigación doctoral, es una prueba de concepto en el uso de las capacidades de abstracción de Haskell para el diseño de lenguajes de dominio específico para la música.

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
