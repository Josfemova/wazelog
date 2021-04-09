---
title: Instituto Tecnológico de Costa Rica\endgraf\bigskip \endgraf\bigskip\bigskip\
 Tarea Corta 2 - wazelog \endgraf\bigskip\bigskip\bigskip\bigskip
author: 
- José Morales Vargas 
- Alejandro Soto Chacón
date: \bigskip\bigskip\bigskip\bigskip Area Académica de\endgraf Ingeniería en Computadores \endgraf\bigskip\bigskip\ Lenguajes, Compiladores \endgraf e intérpretes (CE3104) \endgraf\bigskip\bigskip Profesor Marco Rivera Meneses \endgraf\vfill  Semestre I
header-includes:
- \setlength\parindent{24pt}
lang: es-ES
papersize: letter
classoption: fleqn
geometry: margin=1in
mainfont: Arial
sansfont: Arial
monofont: DejaVuSansMono.ttf 
mathfont: texgyredejavu-math.otf 
fontsize: 12pt
linestretch: 1.5
...

\maketitle
\thispagestyle{empty}
\clearpage
\tableofcontents
\pagenumbering{roman}
\clearpage
\pagenumbering{arabic}
\setcounter{page}{1}

# wazelog

## 1.1. Descripción de los hechos y reglas implementadas

## 1.2. Descripción de las estructuras de datos desarrolladas

## 1.3. Descripción detallada de algoritmos desarrollado

Para comprender el diseño de la solución, es necesario una descripción general primero. El objetivo de la asignación era la creación de un sistema experto capaz de interactuar con un agente humano para proveerle de la mejor ruta posible para una ubicación actual, un destino, y una serie de paradas intermedias posibles.

La infraestructura básica de un sistema experto se puede separar en la base de datos de la cual obtiene su conocimiento, la interfaz con el usuario y el motor de inferencia, el cual a su vez se integra por las reglas dadas al sistema y el motor de deducción provisto por el lenguaje. El sistema también debe poseer 4 características esenciales:

1. Debe ser modular.
2. Incremental.
3. Modificable.
4. Transparente.

El solución final cumple con esta especificación. Se provee un programa cuya modularidad permite una fácil modificación y adaptación del sistema, y una separación lógica que hace de la toma de decisiones un proceso que sucede de modo fácil de seguir, explicar, y modificar según los requisitos del sistema.

Para la resolución general del problema se identificaron tres secciones esenciales para el funcionamiento del programa: 
 
 * Análisis de lenguaje natural mediante gramáticas libres de contexto.
 * Búsqueda de rutas óptimas en un grafo.
 * Interacción con el usuario (Interfaz en consola).

El centro del programa es la interfaz, puesto que administra el flujo de evaluación de las distintas reglas. El sistema de análisis de lenguaje sirve como instrumento para procesar la entrada de usuario en información útil para la resolución de problema en cuestión (la obtención de una ruta más corta). 

El flujo del programa es uno en que de manera incremental se va obteniendo información respecto al problema que se quiere solucionar, y la rutina de obtención de información termina hasta que el usuario haya dado de manera implícita una indicación de que ya no hay más datos para procesar. Una vez determinado que no es necesario procesar más datos de entrada, se obtiene la solución al problema modelado por la información dada por el usuario, en este caso, la búsqueda de una ruta óptima entre dos puntos en un grafo.

Antes de proceder con la explicación del algoritmo que describe el funcionamiento del programa, es esencial ver como interpreta el programa la entrada provista por el usuario. En el caso de la versión de wazelog desarrollada, se utiliza una gramática libre de contexto descrita por el siguiente Bach-Naur Form (BNF):

```BNF
<input> ::= <sentence-sep> <input>
          | <sentence> <extra-input>

<extra-input> ::= <sentence-sep> <extra-input>
                | EOF
                | <sentece> <extra-input>

<sentence> ::= <filler> <sentence>
             | <exclamation> <after-exclamation>
             | <svo>

<svo> ::= <verbal> <after-verbal>
        | <nominal> <after-nominal>

<after-verbal> ::= <filler> <after-verbal>
                 | <svo>

<after-nominal> ::= <filler> <after-nominal>
                  | EOF
                  | <sentence-sep>
                  | <svo>

<after-exclamation> ::= EOF
                      | <sentence-sep>
                      | <nominal> <after-exclamation>
                      | <filler> <after-exclamation>
                      | <verbal> <after-verbal>

<filler> ::= <unclassified>
           | <before-nominal>
           | <contraction>

<sentence-sep> ::= '.'
                 | ','
                 | ';'
                 | ':'

<exclamation> ::= 'si'
                | 'no'
                | 'hola'
                | 'adios'
                | 'gracias'

<nominal> ::= <(todo token que no sea <verbal> | <exclamation> | <filler>)>

<verbal> ::= 'esta'
           | 'estoy'
           | 'encuentro'
           | 'encuentra'
           | 'voy'
           | 'necesito'
           | 'ir'
           | 'es'
           | 'llegar'
           | 'pasar'
           | 'ubica'
           | 'gustaria'

<unclassified> ::= 'me'
                 | 'que'
                 | 'a'
                 | 'se'
                 | 'en'
                 | 'un'
                 | 'una'
                 | 'tengo'
                 | 'por'
                 | 'muchas'

<before-nominal> ::= 'el'
                   | 'los'
                   | 'la'
                   | 'las'
                   | 'de'

<contraction> ::= 'al'
                | 'del'

```

Reglas y hechos son definidos en el programa para parsear la entrada de usuario basados en la gramática descrita por el BNF. En los diagramas que se muestran posteriormente el resto de reglas que se ven involucradas en el programa, se presenta el como se procesa la gramática para evitar saturar los diagramas con esta información, pero se debe tener presente que es con las reglas derivadas del BNF anterior que se realiza el proceso de parsing con el que funcionan reglas como `parse_user_input` y `unbounded`


![](https://raw.githubusercontent.com/Josfemova/wazelog/main/doc/DiagramaSolGeneral.png)

Se puede utilizar el diagrama anterior para navegar el algoritmo general de resolución. Nótese que el diagrama no contiene absolutamente todo el programa, sin embargo, las partes omitidas se consideran de una relevancia menor (impresión y contenido de mensajes mayormente), o se desarrollarán más adelante en esta sección.

Primero, se puede observar una etapa inicial relativamente lineal. El punto de entrada por medio de la regla `main` permite especificar un lenguaje para la operación del programa, luego de esto, se configuran las opciones de operación por medio de la regla `set_lang` y otras configuraciones menores, y luego de esto se entra al programa en sí por medio de la regla `start`, la cual indica si el programa se encuentra en un estado activo. La condición en el que el programa es inactivo se da una vez que el usuario le indique al sistema mediante una exclamacion de despedida que no es necesaria mayor interacción. De lo contrario, el sistema se dispondrá a atender a un nuevo usuario o una nueva consulta en caso de ser necesario.

Luego de la rutina inicial, se entra al estado raíz, dado en la regla `run`. Esta útlima es el punto intermedio entre los 5 estados principales del sistema, los cuales se explican a continuación:

1. Solicitud de ubicación: El programa le pregunta al usuario cual es su ubicación actual; en el diagrama denotado como `src`. Una vez obtenida una respuesta válida o exclamación de despedida, procede con el siguiente estado. En el diagrama, se observa que primero se llaman a reglas auxiliares que manejan las preguntas al usuario, y posterior a la evaluación de estas reglas es que se lee y parsea la entrada del usuario. 

2. Solicitud de destino: Casi la misma rutina que la de solicitud de ubicación, pero en esta instancia la incógnita a resolverse es el destino del usuario. El programa seguirá preguntando un destino hasta obtener una respuesta válida. El flujo es prácticamente el mismo que en el primer estado, pero las preguntas al usuario cambian.

3. Solicitud de destinos intermedios: Se entra en un ciclo que repite una rutina similar a los estados anteriores, solo que esta vez se dan preguntas continuas hasta obtener una indicación de parte del usuario que no quedan más destinos intermedios por procesar. 

4. Procesamiento: Obtenidos los datos de ubicación, destino, y una lista de paradas intermedias, el programa calcula la mejor ruta a tomar mediante el uso de las reglas `shortest_path_through` y `shortest_path`. La primera regla se encargar de la inferencia de la ruta en sí, mientras que `shortest_path` evalúa otros datos relevantes para el usuario, tal como la distancia abarcada por la ruta, entre otras cosas.

5. Emisión de la respuesta: Obtenida la ruta y los datos correspondientes a la misma, se le comunica al usuario el resultado obtenido y el sistema retorna al estado 1 para responder futuras consultas.

Algo a tomar en cuenta para los estados 1, 2 y 3 es que el hay un procesamiento adicional posible, en casos de que el destino dado no sea lo suficientemente claro para el programa, pero igual sea identificable que el usuario se refiere a un lugar. Estos casos son manejados por la regla `pinpoint`, la cual trata de obtener la ciudad concreta en la cual se encuentra el destino de un usuario. E.g., el usuario puede querer dirigirse al mercado, pero al existir la posibilidad de que varias ciudades tengan mercados, pinpoint se encarga de resolver las preguntas de cuál mercado, y en qué ciudad queda dicho mercado. 

![](https://raw.githubusercontent.com/Josfemova/wazelog/main/doc/DiagramaPath.png)

## 1.4. Problemas sin solución

## 1.5. Actividades realizadas por estudiante

## 1.6. Problemas solucionados

1. Errores con caracteres especiales del idioma español
	
	* _Descripción_: Mientras se realizaban pruebas de comprobación de calidad se identificó un problema con la codificación en el ambiente de Windows 10. Ni la consola ni el software de SWI-Prolog parecen ser compatibles con UTF-8 en la plataforma de Windows, a pesar de no ser así en linux. Esta incompatibilidad significa que las interacciones usuario-sistema se veían afectadas ya que no había errores en la interpretación de la entrada de usuario y en la escritura de mensaje para el usuario. 
	* _Intentos de solución_: Se trató de utilizar la regla de encoding para forzar un reconocimiento de UTF-8, y aunque esto permitía ya que la interpretación correcta del archivo, igual se daban problema en la lectura de la entrada de usuario, porque la codificación del stream permanece en otro formato. 
	* _Solución encontrada_: Se consiguió una doble solución. Ambas se implementaron. Una versión del programa evita los caracteres especiales como la ñ y la tilde, mientras que otro se codificó en latin1 y se logró mantener la interacción en español con caracteres especiales incluídos.
	* _Recomendaciones_:
		- Si se quiere evitar problemas de codificación de consola con SWI-Prolog, es preferible utilizar un sistema basado en GNU/Linux, puesto que el problema parece ser único de la plataforma de Windows.
		- Si se quiere conservar la posibilidad de una interacción con tildes y eñes, una alternativa recomendable es codificar los archivos en latin1, pues parece que la compatibilidad de esta codificación con Windows es capaz de proveer esta posibilidad.
	* _Conclusiones_:
		- Se confirma como una
	* _Bibliografía_:
		
		- <https://www.swi-prolog.org/pldoc/doc_for?object=encoding/1>

2. 
	
	* _Descripción_:
	* _Intentos de solución_:
	* _Solución encontrada_:
	* _Recomendaciones_:
	* _Conclusiones_:
	* _Bibliografía_:

## 1.7. Conclusiones y Recomendaciones del Proyecto

- Dado su estatus relativamente estándar en el mercado, es recomendable utilizar BNF's para describir una gramática libre de contexto en caso de que se esté utilizando una, esto pues permite modelar el procesamiento de lenguaje de forma agnóstica respecto al lenguaje de implementación, y al mismo tiempo sirve de guía para la implementación en cualquier lenguaje. 



## 1.8. Bibliografía

<https://www.swi-prolog.org/pldoc/man?section=readutil>
<https://www.swi-prolog.org/pldoc/man?section=dicts>
<https://www.swi-prolog.org/pldoc/doc/_SWI_/library/heaps.pl>
<>
<>
<>
<>
<>
<>


## 1.9. Bitácoras
