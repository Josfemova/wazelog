---
title: Instituto Tecnológico de Costa Rica\endgraf\bigskip \endgraf\bigskip\bigskip\
 Tarea Corta 2 - BlaCEJack \endgraf\bigskip\bigskip\bigskip\bigskip
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


![](link diagrama)


Para la resolución general del problema se identificaron tres secciones esenciales para el funcionamiento del programa: 
 
 * Análisis de lenguaje natural mediante gramáticas libres de contexto.
 * Búsqueda de rutas óptimas en un grafo.
 * Interacción con el usuario (Interfaz en consola).

El centro del programa es la interfaz, puesto que administra el flujo de evaluación de las distintas reglas. El sistema de análisis de lenguaje sirve como instrumento para procesar la entrada de usuario en información útil para la resolución de problema en cuestión (la obtención de una ruta más corta). 

El flujo del programa es uno en que de manera incremental se va obteniendo información respecto al problema que se quiere solucionar, y la rutina de obtención de información termina hasta que el usuario haya dado de manera implícita una indicación de que ya no hay más datos para procesar. Una vez determinado que no es necesario procesar más datos de entrada, se obtiene la solución al problema modelado por la información dada por el usuario, en este caso, la búsqueda de una ruta óptima entre dos puntos en un grafo.


## 1.4. Problemas sin solución

## 1.5. Actividades realizadas por estudiante

## 1.6. Problemas solucionados

## 1.7. Conclusiones y Recomendaciones del Proyecto

## 1.8. Bibliografía

<https://www.swi-prolog.org/pldoc/doc_for?object=read_line_to_string/2>

## 1.9. Bitácoras
