# Marco teórico
Aún no se encuentra apropiadamente delimitado. Sin embargo presento algunos referentes que utilizaré para su construcción. Recordemos que los ritmos tienen maneras generativas de ser producidos [ver @Toussaint2005Euclidean, cap. 1; @Elliott1997Functional].

Esta investigación tiene las siguientes ramificaciones sobre el estudio de estos lenguajes de programación de patrones rítmicos:

1. Los aspectos tecnológicos en los que se fundamenta su diseño.
	1. Haskell, la programación funcional y la Functional Reactive Programming (FRP)
	2. Interfaz textual para la generación de patrones algorítmicos (concepto propuesto por Alex McLean).
2. Las derivaciones e implicaciones que tiene en la teoría musical occidental sobre el ritmo.
3. Su caracterización como implementación de modelos matemáticos y de estructura geométrica.

Para tratar los modelos matemáticos y geométricos del ritmo, así como su implementación, se construirá la parte matemático-musical-computacional del marco teórico a partir de libros recientes que abordan diversas aristas del tema. Estas fuentes me permitirán un contexto amplio para acotar conceptos como "ritmo", "metro", "modelo" y "patrón" en el contexto de implementaciones computacionales:

+ [Toussaint, G. T. (2020). The geometry of musical rhythm: What makes a “good” rhythm good?](https://www.taylorfrancis.com/books/9781351247771)
+ [Boenn, G. (2018). Computational Models of Rhythm and Meter. Springer International Publishing](https://doi.org/10.1007/978-3-319-76285-2)
+ [The Musical-Mathematical Mind. Patterns and Transformations (2017)](https://doi.org/10.1007/978-3-319-47337-6)

Sobre la relaciones entre la creación-generación de obra y el uso de lenguajes de programación (en particular programación funcional), el concepto de "patrón algorítmico" propuesto por Alex McLean se presenta como un primer elemento para esta parte del marco teórico. Resulta particularmente importante por su relación con el diseño de Tidal. Ademas hay un foro en internet en el cual se está conformado una comunidad de investigación alrededor del concepto:

+ [McLean, A. (2020). Algorithmic Pattern. Proceedings of the International Conference on New Interfaces for Musical Expression, 6](https://www.nime.org/proceedings/2020/nime2020_paper50.pdf)
+ [Tesis de Doctorado de McLean (2011)](https://slab.org/writing/thesis.pdf)
+ [Algorithmic Pattern Forum](https://forum.algorithmicpattern.org/)

Por ultimo, el programa de Erlangen es el componente principal de la parte geométrica de la investigación. [Este artículo](https://flm-journal.org/Articles/4C154BFE42449B32FFD61852283E7E.pdf) da una breve introducción a partir de su origen histórico.

## Matemáticas

Podemos tener ecuaciones matemáticas dentro del texto de la siguiente manera:

$$n \in \mathcal{N}$$

## Estado del arte
Platicas recientes con colegas del Seminario Permanente de Tecnología Musical me llevaron a identificar a un grupo de investigación dirigido por David Ogborn en la universidad MacMaster en el que participan los mexicanos Alejandro Franco, Luis Navarro y Jessica Rodriguez. Este grupo de investigación esta trabajando en el desarrollo de lo que llamo "lenguajes tipo Tidal", mismos que están siendo implementados en la plataforma de live coding en red [estuary](https://estuary.mcmaster.ca/), en la que se pueden utilizar desde el navegador de internet.

La siguientes ligas llevan a los repositorios de los correspondientes proyectos. En ellos basta leer el texto de la página principal para una descripción general. Se trata de proyectos de DSLs para la generación de patrones:

1. [Tidal Cycles](https://github.com/tidalcycles/Tidal)
2. [timeNot](https://github.com/AFrancoB/timeNot)
3. [Nanc-in-a-can Canon Generator](https://github.com/nanc-in-a-can/canon-generator)
4. [seis8s](https://github.com/luisnavarrodelangel/seis8s)
5. [TransMit](https://github.com/jac307/TransMit)
6. [CQenze](https://github.com/essteban/CQenze)

Siguiento esta [liga](#titulo) podemos ver el título tentativo.

Las notas al pie son una forma excelente de separar información del cuerpo del texto.[^1]
Y lo podemos hacer tantas veces como sea necesario.[^veces] También podemos aprovechar otras funciones para escribir notas al pie. ^[Como hacerlo de manera "inline" utilizando una sintaxis especial que no permite notas con saltos de línea. Aunque esta sintaxis es propia de Pandoc.]

[^1]: Por ejemplo, en este caso trasladamos esta información.

[^veces]: Y agregar más información relevante.
