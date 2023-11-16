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

* Introducción (_pre-examen_)
   * Temática y preguntas de investigación

1. Antecedentes y Marco teórico
   1. Genealogía (_antecedentes_)
	  1. Patrón algorítmico
	  1. Programación reactiva funcional
	  1. Haskell: eDSLs para programación multimedia
   1. Abstracción y Expresividad (_marco teórico_)
	  1. Musical vs computacional
		  * Expresividad de un lenguaje de programación
	  1. Ejemplo: Problema "FizzBuzz" (_¿se queda?_)
	  1. Ejemplo: RTG
1. Programación funcional de música (_estado de la cuestión_)
   1. Programación funcional
   1. Lenguajes para música en Haskell
	  1. La multidimensionalidad de la música: representaciones y modelos
	  1. Código y pensamiento (en live coding y composición algorítmica)
	  1. eDSLs
		  1. Tidal Cycles
		  1. Euterpea
		  1. Conductive
		  1. RTG
1. RTG: Ritmo, tiempo y geometría (_aporte tecnológico_)
   1. Concepto
	  * Patrones como transformaciones geométricas
   1. Diseño
	  1. Tipos de datos
	  1. Funciones básicas
	  1. Funciones auxiliares
   1. Implementación
   1. API
   1. _Live coding_ y composición algorítmica
1. Estudio matemático-geométrico (_aporte teórico-artístico_)
   1. Programa de Erlangen

* Conclusiones (_pre-candidatura_)
* Anexos
  * Grupos de transformaciones
  * Teoría de lenguajes de programación
* Índice analítico (pendiente)
* Glosario
* Bibliografía
