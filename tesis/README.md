# Markdown como fuente

Esta tesis está escrita utilizando [Pandoc-Markdown](https://pandoc.org/MANUAL.html#pandocs-markdown),
variante de markdown con una gran cantidad de extensiones para dar más flexibilidad.
Pandoc permite traducir la misma fuente texto a formatos como
HTML, XML, DOCX, EPUB, ODT, LaTeX y PDF.


## Build

Las instrucciones en `Makefile` automatizan la llamada a Pandoc para
producir el texto en formato ODT (para revisión con control de cambios y comentarios) y PDF (para impresión)
a partir de los archivos de texto en _markdown_.

Para ello basta con ejecutar el comando `make` desde esta ubicación.[^1]

Es necesario instalar las siguientes dependencias:

* Pandoc. [Instrucciones oficiales de instalación](http://pandoc.org/installing.html).
  * MacOS: Se recomienda utilizar el administrador de paquetes [Homebrew](http://brew.sh/). Correr `brew update` en la terminal y en seguida `brew install pandoc`.
  * Windows: Se recomienda utilizar el administrador de paquetes [Chocolatey](https://chocolatey.org/). Correr en terminal `choco install pandoc`.
  * Linux: usar el administrador de paquetes de la distribución.
* LaTex (se peude instalar de manera similar utilizando los administradores de paquetes antes mencionados):
[MacTeX](https://www.tug.org/mactex/) para MacOS,
[MiKTeX](https://miktex.org/) o [Tex Live](https://www.tug.org/texlive/) para Windows y
[Tex Live](https://www.tug.org/texlive/) para linux.
  * Asegurarse de tener instalado XeTeX en caso de tener una instalación parcial de la distribución de LaTeX (particularmente en el caso de Tex Live en Linux).
* Fuentes:
  * [Linux Libertine](http://www.linuxlibertine.org/index.php?id=91&L=1)
  * [Inconsolata](http://levien.com/type/myfonts/inconsolata.html)

[^1]: https://www.gnu.org/software/make/


# Índice

El siguiente es un índice tentativo elaborado para proyectar la escritura de la tesis.
Algunos títulos tienen `etiquetas`.

* Introducción `pre-examen`
   * Temática y preguntas de investigación

1. Lenguajes de programación `marco teórico`
   1. eDSLs para programación multimedia
   1. Expresividad
       * Lenguaje de programación
       * Musical
   1. Abstracción
       * Computacional
       * Musical
   1. Programación funcional
       1. Programación reactiva funcional (representación del tiempo)
1. Programación funcional de música `estado de la cuestión`
   1. Lenguajes para música en Haskell
      1. La multidimensionalidad de la música: representaciones y modelos
      1. Código y pensamiento (en live coding y composición algorítmica)
      1. Casos de estudio
         1. Tidal Cycles
            1. Patrón algorítmico
         1. Euterpea
         1. Conductive
         1. RTG
1. RTG: Ritmo, tiempo y geometría `aporte tecnológico`
   1. Concepto
      * Patrones como transformaciones geométricas
      * Estudio matemático-geométrico `aporte teórico-artístico`
          * Programa de Erlangen
   1. Diseño
      1. Tipos de datos
      1. Funciones básicas
      1. Funciones auxiliares
   1. Implementación
   1. API
   1. Uso en _Live coding_ y composición algorítmica

* Conclusiones `pre-candidatura`
* Anexos
   * Grupos de transformaciones
   * Teoría de lenguajes de programación
* Índice analítico (pendiente)
* Glosario
* Bibliografía

# Metas

1. Crear una biblioteca de Haskell, basada en la noción de geometría que se desprende del [Programa de Erlangen](https://es.wikipedia.org/wiki/Programa_de_Erlangen), para la generación de patrones rítmicos.
   1. Definir familias de patrones rítmicos a partir de modelos y algoritmos geométricos (el ejemplo paradigmático son los ritmos euclidianos).
   1. Proveer una estructura de grupo para cada familia de patrones rítmicos, mediante una operación binaria que cumpla los axiomas de grupo abstracto.
   1. Desarrollar procedimientos y operaciones que produzcan efectos musicales significativos, es decir, que trasciendan la arbitrariedad en favor de una coherencia estructural entre los argumentos del código y los resultados sonoros.
2. Explorar las nociones de _expresividad_ del código relevantes para el ámbito de la programación al vuelo (live coding).
3. Desarrollar la biblioteca en un lenguaje de dominio específico embebido en que su acción se extienda a otras dimensiones musicales (altura, espectro, timbre y estructura).

# Críticas pertinentes y respuestas provisionales

> La investigación aborda aspectos relacionados con las matemáticas, las ciencias de la computación, la práctica artística y el desarrollo tecnológico
> ¿Cuál es su aspecto central o principal aporte? ¿Qué papel juegan estos ámbitos?

El papel de cada ámbito se puede aclarar con un artificio: presentaré sus roles como etapas secuenciales.

## Ciencias de la computación

La primera etapa de este proyecto se conformó por un clavado profundo en la teoría de lenguajes de programación a través del estudio de Haskell.
Durante este proceso he identificado la genealogía de mi investigación en la interrelación del trabajo de Alex McLean, Conal Elliot y Paul Hudak,
cuyo hilo conductor es el uso de la _programación funcional_ en la práctica creativa y creación multimedia.
De este tema se desprende el campo de estudio en que he situado esta investigación.[^1]
El fruto central de esta etapa ha sido la identificación de los conceptos de _abstracción_ y _expresividad_ como puntos de articulación de mi marco teórico.

[^1]: Cuyos esfuerzos de legitimación se han concentrado en la _Workshop on Functional art, music, modeling and design_ (FARM), celebrado anualmente en el marco de la _International Conference on Functional Programming_ (ICFP).

## Desarrollo tecnológico

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

## Práctica artística

La definición de instancias de grupo es el elemento característico de la biblioteca.
Corresponde a definir, para cada tipo de patrón rítmico, una operación que permite combinarlos.

Por otro lado la sintaxis de la biblioteca debe ser ergonómica y expresiva: permitir dinamismo en la generación, combinación y transformación de los patrones rítmicos en la composición y en la interpretación en vivo, de una forma previsible y coherente con la escucha.

La retroalimentación con la práctica artística, propia y de colegas, será fundamental para ajustar estas definiciones para lograr resultados interesantes en estos aspectos.

## Matemáticas

La inspiración de RTG está en el _programa de Erlangen_. Este programa de investigación matemática propone el estudio de la geometría
a través del estudio de los grupos de transformaciones (o _simetrías_).
La última etapa corresponde a la exploración de las posibilidades matemáticas de RTG.
De momento queda fuera del alcance de la tesis.
La idea es que RTG sirva para proyectar estructuras espaciales en patrones rítmicos desplegados en el tiempo.
Similarmente, grupos abstractos generales podrían ser usados para generar nuevas estructuras rítmicas.

## Otras preguntas

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
