---
title: Instituto Tecnológico de Costa Rica\endgraf\bigskip \endgraf\bigskip\bigskip\
 Tarea Corta 2 - Wazelog \endgraf\bigskip\bigskip\bigskip\bigskip
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

## 2.3.2 Interacción con el sistema

### Generalidades sobre preguntas relacionadas a lugares

Para poder obtener la mejor ruta posible, Wazelog necesita de datos de ciudades precisas. Estos datos se obtienen de una respuesta de parte del usuario, pero es posible que por diferentes razones, el usuario no responda con el nombre de una ciudad precisa, sino que puede darse una situación en que el usuario responda con un calificativo de lugar, por ejemplo "supermercado" o "parque". Ya que una interacción en la que simplemente se diga "parque no es un nombre de ciudad" resultaría poco amigable con el usuario, existe una subrutina que se asegura de manejar esta situación de forma que la interacción resulte más natural.

```sh
caso en que entra a pinpoint
```

No se debe descartar la posibilidad de entrada sin sentido por parte del usuario. Aún así, si tal situación llegara a darse, Wazelog maneja dichas interacciones comunicándole al usuario que fue incapaz de entender su mensaje. Si se recibe un mensaje de estos hay una fuerte posibilidad de que haya un error gramático en la entrada de usuario. El sistema es relativamente tolerante a errores en gramática, pero hay algunos que pueden ser más difíciles de controlar que otros. Si llega a recibir un mensaje de estos, puede intentar corregir la gramática del mensaje, o reformular el mensaje en sí

```sh
[Wazelog]:::| Perdón, no he podido entenderle. ¿Desea un destino intermedio? :::|
```


### Pregunta de ubicación

```sh
[Wazelog]:::| Bienvenido a WazeLog, la mejor lógica de llegar a su destino.
			  Por favor, indíqueme donde se encuentra. :::|
@Usuario:
```

### Pregunta de destino

```sh
[Wazelog]:::| Perfecto, ¿cuál es su destino? :::|
@Usuario: 
```

### Resolución de destinos intermedios

```
[Wazelog]:::| Genial, ¿algún destino intermedio? :::|
@Usuario
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


