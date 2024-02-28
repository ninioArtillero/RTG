## Abstracción

La abstracción es un proceso tan fundamental para la cognición humana
que una definición precisa sería necesariamente insatisfactoria o paradógica.
En esta sección se abordará el concepto de abstracción y sus acepciones
en el contexto de la composición musical y la programación.

### Abstracción computacional

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
Es importante considerar que en el contexto socio-económico actual el software no puede ser estático pues necesita, por lo menos, adaptarse a los cambios en sus dependencias.
Aunado a esto, su plasticidad como información invita a la constante adición de nueva funcionalidad.
La complejidad del software constituye un problema de la ingeniería y
constantemente se buscan estrategias para mitigarla, o por lo menos encapsularla.
Esto sucede al limitar el acceso a las partes del código
que no incumben a la funcionalidad de interés de una aplicación específica.
Por ejemplo, la estrategia precisa para el lavado de platos es algo que es abstraído
de la consideración del contador de un restaurante.
Esta idea es implementada utilizando constructos como _objetos_ o _módulos_, y deriva en el diseño de una API (_application programming interface_ o interfaz de programación de aplicaciones).

Por otro lado, la generalización de procesos es llamada también _abstracción funcional_ [@Abelson2002Structure]. 


