# Markdown como fuente

Esta tesis está escrita utilizando [Pandoc-Markdown](https://pandoc.org/MANUAL.html#pandocs-markdown),
la cual cuenta con una gran cantidad de extensiones de sintaxis que permiten traducir la misma fuente a formatos como
HTML, XML, DOCX, EPUB, ODT, LaTeX y PDF.


## Build

Las instrucciones en `Makefile` automatizan la llamada a Pandoc para
producir el texto en formato ODT (para revisión con control de cambios y comentarios) y PDF (para impresión)
a partir de los archivos de texto en _markdown_.

Para ello basta con ejecutar el comando `make` desde esta ubicación.[^1]
Es necesario [instalar Pandoc](https://pandoc.org/installing.html).

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
