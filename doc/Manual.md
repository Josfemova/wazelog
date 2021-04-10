---
title: Instituto Tecnológico de Costa Rica\endgraf\bigskip \endgraf\bigskip\bigskip\
 Tarea Corta 2 - Wazelog \endgraf\bigskip Manual de Usuario \endgraf\bigskip\bigskip
author: 
- José Morales Vargas, carné 2019024270
- Alejandro Soto Chacón, carné 2019008164
date: \bigskip\bigskip\bigskip\bigskip Area Académica de\endgraf Ingeniería en Computadores \endgraf\bigskip\bigskip\ Lenguajes, Compiladores \endgraf e intérpretes (CE3104) \endgraf\bigskip\bigskip Profesor Marco Rivera Meneses \endgraf\vfill  Semestre I
header-includes:
- \setlength\parindent{24pt}
lang: es-ES
papersize: letter
classoption: fleqn
geometry: margin=1in
#fontfamily: sans
fontsize: 12pt
monofont: "Noto Mono"
linestretch: 1.15
...

\maketitle
\thispagestyle{empty}
\clearpage
\tableofcontents
\pagenumbering{roman}
\clearpage
\pagenumbering{arabic}
\setcounter{page}{1}




# 2.1. Requisistos de sistema:

1. Instalación de SWI-Prolog en el sistema

# 2.2. Ambientes en los que se comprobó funcionamiento:

- Windows 10 64bit English
- Arch Linux - kernel 5.11.12
- Pop OS

# 2.3. Uso del programa

## 2.3.1 Inicio

Para iniciar el programa hay dos formas:

### Forma 1: Ejecución desde consola con comando swipl

Si se quiere iniciar el programa sin necesidad de abrir una instancia de la interfaz gráfica de swi-prolog, se puede ejecutar el siguiente comando:


Para windows:

```sh
export Directorio=[carpeta en la que se encuentra el archivo de main.pl]
swipl $Directorio\\main.pl --lang [en/es]
```

Para Linux:

```sh
export Directorio=[carpeta en la que se encuentra el archivo de main.pl]
swipl $Directorio/main.pl --lang [es/en]
```

### Forma 2: Ejecución mediante carga en la interfaz gráfica para swi-prolog

El punto de entrada del programa se encuentra en el archivo `main.pl`, por lo cual este debe ser el cargado. 

Una vez cargado, la consulta para poder lanzar el programa tiene la siguiente forma:

```prolog
?- main(['--lang', Lang]).
```

Donde la variable Lang tiene que ser cambiada por el lenguaje deseado por el usuario, ya sea `es` para español o `en` para inglés (experimental).

## 2.3.2. Interacción con el sistema

### Generalidades sobre preguntas relacionadas a lugares

Para poder obtener la mejor ruta posible, Wazelog necesita de datos de ciudades precisas. Estos datos se obtienen de una respuesta de parte del usuario, pero es posible que por diferentes razones, el usuario no responda con el nombre de una ciudad precisa, sino que puede darse una situación en que el usuario responda con un calificativo de lugar, por ejemplo "supermercado" o "parque". Ya que una interacción en la que simplemente se diga "parque no es un nombre de ciudad" resultaría poco amigable con el usuario, existe una subrutina que se asegura de manejar esta situación de forma que la interacción resulte más natural.

```sh
[Wazelog]:::| Bienvenido a WazeLog, la mejor lógica de llegar a su destino.
			  Por favor, indíqueme donde se encuentra. :::|
@Usuario: Hola Wazelog, me encuentro en Turrialba
[Wazelog]:::| Perfecto, ¿cuál es su destino? :::|
@Usuario: San José
[Wazelog]:::| Genial, ¿algún destino intermedio? :::|
@Usuario: Sí, me gustaría pasar al Walmart
[Wazelog]:::| ¿Dónde se encuentra el Walmart? :::|
@Usuario: en Cartago
[Wazelog]:::| ¿Algún otro destino intermedio? :::|
@Usuario: no
=============================================================================
[Wazelog]:::| Su ruta sería Turrialba, Pacayas, Cartago, Tres Ríos, San José. 
			  Longitud estimada de 47 km. Duración 47-94 min. :::|
[Wazelog]:::| ¡Muchas gracias por utilizar WazeLog! :::|
=============================================================================
```

No se debe descartar la posibilidad de entrada sin sentido por parte del usuario. Aún así, si tal situación llegara a darse, Wazelog maneja dichas interacciones comunicándole al usuario que fue incapaz de entender su mensaje. Si se recibe un mensaje de estos hay una fuerte posibilidad de que haya un error gramático en la entrada de usuario. El sistema es relativamente tolerante a errores en gramática, pero hay algunos que pueden ser más difíciles de controlar que otros. Si llega a recibir un mensaje de estos, puede intentar corregir la gramática del mensaje, o volver formular el mensaje en sí:

```sh
[Wazelog]:::| Perdón, no he podido entenderle. ¿Desea un destino intermedio? :::|
```

Otra posibilidad en la que Wazelog puede no entenderle es si el usuario decide utilizar voz pasiva para comunicarse. Existe cierta tolerancia del sistema, pero en caso de que haya un fallo en comprensión lo mejor será volver a escribir su respuesta en una voz activa. 

Considere además que por la escala del proyecto, hay varios registros del habla que no son soportados por el sistema, esto porque los mismos varían en estructura e incluso en símbolos de puntuación (por ejemplo, el registro "concho" costarricense que utiliza apóstrofes). Para lograr una comunicación efectiva con el sistema debe usarse lenguaje y gramática estándar.

En caso de querer profundizar un poco más respecto al diccionario que utiliza Wazelog, puede observar el archivo lang.pl.

### Pregunta de ubicación

Al iniciar el programa Wazelog inmediatamente solicitará la ubicación actual, puesto que la misma es esencial para poder obtener la mejor ruta de viaje.

```sh
[Wazelog]:::| Bienvenido a WazeLog, la mejor lógica de llegar a su destino.
			  Por favor, indíqueme donde se encuentra. :::|
@Usuario:
```

Una vez una entrada válida sea dada por el usuario, se pasa a la pregunta de destino.

### Pregunta de destino

Al igual que la pregunta de ubicación, el destino es información esencial sin la cual no se podrá calcular una ruta. 

```sh
[Wazelog]:::| Perfecto, ¿cuál es su destino? :::|
@Usuario: 
```

Al obtener una entrada válida, se pasará a la siguiente etapa.

### Resolución de destinos intermedios

Si durante el viaje el usuario ve necesario pasar por destinos intermedios, Wazelog provee de una funcionalidad que le permite definir la cantidad de destinos intermedios que considere necesarios.

```
[Wazelog]:::| Genial, ¿algún destino intermedio? :::|
@Usuario
```

Para detener las preguntas de destinos intermedios debe responder a la pregunta mostrada anteriormente con una exclamación negativa, tal como "no".

### Obtención de ruta

La información provista por el usuario será utilizada para calcular la mejor ruta posible entre la ubicación actual, el destino, y con paradas intermedias en los lugares especificados. una vez completada la evaluación de la mejor ruta, Wazelog le comunicará la mejor ruta en una forma similar a la siguiente:

```prolog
=============================================================================
[Wazelog]:::| Su ruta sería Turrialba, Pacayas, Cartago, Tres Ríos, San José. 
			  Longitud estimada de 47 km. Duración 47-94 min. :::|
[Wazelog]:::| ¡Muchas gracias por utilizar WazeLog! :::|
=============================================================================
```

### Terminando el programa

El usuario puede terminar ejecución del programa durante una pregunta de ubicación, destino, e incluso, en la pregunta de si desea un destino intermedio

```sh
[Wazelog]:::| Bienvenido a WazeLog, la mejor lógica de llegar a su destino.
			  Por favor, indíqueme donde se encuentra. :::|
@Usuario: san jose
[Wazelog]:::| Perfecto, ¿cuál es su destino? :::|
@Usuario: cartago
[Wazelog]:::| Genial, ¿algún destino intermedio? :::|
@Usuario: adios
[Wazelog]:::| ¡Muchas gracias por utilizar WazeLog! :::|
```

# 2.4. Notas sobre idioma y habilitación de caracteres especiales.

Se adjuntan dos versiones del programa. 

1. La carpeta "src" incluye una versión del programa libre de caracteres especiales que pueden resultar incompatibles con el sistema de windows por la codificación en UTF-8. Si se desea interactuar utilizando el idioma español en la plataforma de windows, deberá prescindir del uso de tildes, y en las palabras con ñ deberá usar el par de letras "ni" como sustitución de la eñe. 

Un ejemplo de como utilizar correctamente el sistema sería:

```prolog
[Wazelog]:::| Perfecto, cual es su destino? :::|
@Usuario: Juan Vinias
```
En este caso el usuario se quiere dirigir a Juan Viñas, sin embargo, para que el programa pueda parsear la entrada de usuario es necesario que reemplaze la ñ por una "ni".

2. La carpeta "src-latin1", contiene el mismo programa, pero la codificación se adaptó de manera que se pueda ejecutar en Windows y conservar la funcionalidad de interpretación de tildes y eñes. Puede utilizar el programa de esta carpeta en caso de que crea más conveniente interactuar con el sistema usando tildes y eñes.
