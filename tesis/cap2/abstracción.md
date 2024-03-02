## Abstracción

La abstracción es un proceso tan fundamental para la cognición humana
que una definición precisa sería necesariamente insatisfactoria o paradógica.
En esta sección se abordará el concepto de abstracción y sus acepciones
en el contexto de la composición musical y la programación.

### Abstracción en la programación

La siguiente cita servirá de clave para abordar la abstracción en el contexto de la programación:

> Todos sabemos que la única herramienta mental mediante la cual un razonamiento muy limitado puede abarcar una miríada de casos se llama "abstracción";
> como resultado, la explotación efectiva de sus poderes de abstracción debe considerarse
> una de las actividades vitales de un programador competente. En relación a esto vale la pena
> señalar que el propósito de abstraer no es ser vago, sino crear un nuevo nivel semántico en el cual se puede ser absolutamente preciso.
^[We all know that the only mental tool by means of which a very finite piece of reasoning can cover a myriad of cases is called "abstraction"; as a result the effective exploitation of his powers of abstraction must be regarded as one of the most vital activities of a competent programmer. In this connection it might be worthwhile to point out that the purpose of abstracting is not to be vague, but to create a new semantic level in which one can be absolutely precise.]

En general, la palabra abstracción en programación denota el encapsulamiento de la complejidad
o la generalización de procesos.
Existen diversos factores que contribuyen a la complejidad del software:

* Requiere de una gran cantidad de líneas de código.
* El funcionamiento integral del mismo escapa a las posibilidades de una sola persona.
* La distancia entre su especificación y su implementación es grande.

Además con el paso del tiempo la cantidad de componentes, su mutua interdependencia y las correcciones no idiomáticas al código (_hacks_) tienden a crecer con el tiempo.^[Esto es conocido como la entropía del código.]
Es importante considerar que, en el contexto socio-económico actual, el software tiende a cambiar vertiginosamente en respuesta a las presiones del mercado por añadir nueva funcionalidad.
Por supuesto existen excepciones a esta regla:
algunos programas pueden considerarse "terminados" en el caso en que
su diseño y funcionalidad no haya cambiado en muchos años, ni cambiara en el futuro previsible, de manera significativa. Como ejemplo están los programas básicos del sistema operativo GNU/Linux,
como `cd` para cambiar de directorio o `ls` para ver su contenido [@Gilgado2023beauty].
La filosofía de la organización [suckless](https://suckless.org/philosophy/)
aboga por programas cuyo desarrollo se mida más por líneas borradas que añadidas,
identificando en ingenio con la simplicidad. Esta idea se basa a su vez en la [filosofía UNIX](https://es.wikipedia.org/wiki/Filosof%C3%ADa_de_Unix): que cada programa haga una sola cosa y la haga bien.

La complejidad del software constituye un problema de la ingeniería y
constantemente se buscan estrategias para mitigarla, o por lo menos encapsularla.
La idea básica del encapsulamiento es limitar el acceso a las partes del código
que no incumben a la funcionalidad de interés de una aplicación específica.
Este concepto es central para la abstracción computacional, y como veremos a parece de diversas maneras.
Por ejemplo, la estrategia precisa para el lavado de platos es algo que es abstraído
en las operaciones que realiza el contador de un restaurante.
Esta idea es implementada utilizando constructos como _objetos_ o _módulos_, y deriva en el concepto de API (_application programming interface_ o interfaz de programación de aplicaciones): el conjunto de funciones que determinan acciones sobre los objetos o tipos de datos que modelan
el dominio de aplicación del programa o biblioteca.

En esta tesis discutiremos la programación en relación al diseño, implementación y uso de bibliotecas de programación.
Los programas ejecutables, obtenidos a través del proceso de compilación, son archivos binarios que representan un conjunto de funcionalidades de los que se ha borrado todo rastro del código fuente.
Constituyen el salto de abstracción hacia el dominio del usuario final.

En seguida presentaré brevemente los dos tipos de abstracciones que describe @Abelson2002Structure.
Primero está la [abstracción de datos](#abstracción-de-datos) se relaciona directamente con el encapsulamiento que hemos discutido hasta ahora.
En segundo lugar, la [abstracción funcional](#abstracción-funcional) se corresponde con la generalización de procesos.

### Abstracción funcional

TODO

### Abstracción de datos

TODO


