### 19 de Marzo

- Ambos miembros del equipo acordamos posponer el desarrollo hasta la siguiente semana debido a diferentes actividades en nuestros itinerarios.
- Se realizó un análisis base de los requisitos del proyecto para tenerlos listos previo a la reunión de  de coordinación que se acordó para el 27 de marzo.

### 27 de Marzo

- Se realizó una reunión de coordinación con el compañero.
- Se organizó la base del plan de actividades y se asignaron las distintas tareas
- Se creo el repositorio del proyecto y se crearon los archivos base de trabajo, incluidos los correspondientes a documentación
- Se investigó un poco sobre el procesamiento de las palabras para formular una idea de como podría estar llegando la información a la sección del sistema experto y el nivel de procesamiento de los datos del grafo

### 28 de Marzo

- Se agregaron algunas cláusulas de hechos, aunque las mismas probablemente tengan que cambiarse. Funcionan más como una guía para facilitar desarrollo posterior.
- Se agregaron algunas cláusulas base de manejo del grafo.
  
### 29 de Marzo

- Se agregaron mensajes de interfaz gráfica.
- Se trabajo un poco en la parte de búsqueda de ruta en paralelo con el compañero Alejandro. En la tarde se realizó una reunión para conversar sobre los diferentes acercamientos al problema y se decidió mantener la implementación de la búsqueda de ruta hecha por el compañero. En esta misma reunión se decidió que mi persona trabajaría en la programación del autómata de que interactúa con el usuario.

### 30 de Marzo

- Se realizó gran parte de la implementación del autómata, incluyendo las preguntas de origen, destino, y las paradas intermedias en un viaje. 
- Se realizaron pruebas de calidad relacionadas al desempeño del autómata y el acoplamiento con el procesador de lenguaje natural de manera que posteriormente se puedan hacer ajustes para mejorar la coherencia de los mensajes del autómata.

### 31 de Marzo

- Se integro la parte de pathfinding con el autómata. 
- Se resolvieron la mayoría de problemas relacionados al funcionamiento del autómata. Falta cubrir algunos corner cases pero en su mayoría parece estar listo.
- Se agregaron algunas clausulas para facilitar la traducción de átomos correspondientes a ciudades. Esto podría ser innecesario de modificarse un poco el código para procesar los elementos del pathfinding conservando los strings originales obtenidos del parser. 
- Se agregaron adornos a la comunicación entre el usuario y WazeLog para hacer la comunicación con el usuario más clara y agradable.
- De la parte de pathfinding noté que se deben resolver casos bidireccionales, le comuniqué al compañero al respecto para que pueda procesarlo a como lo vea mejor. 
- Seria agradable agregar los datos de tiempo de viaje en caso de ser necesarios. Debo discutir esto con el compañero.

### 1 de Abril

- Se tradujeron varias de las reglas para mejorar la cohesión aparente del código, esto puesto que parte del código tenía reglas con argumentos y nombres en español, mientras otra sección se manejaba en inglés.
- Se agregó documentación de algunas reglas principales.

### 3 de Abril

- Se agregó mayor documentación de reglas, específicamente las correspondientes a la parte del programa que interactúa directamente con el usuario.

### 4 de Abril

- Se agregaron más secciones de documentación interna al código y se cambió ligeramente el formato para ilustrar mejor los ejemplos y hacer más manejable para formatear posteriormente.

### 5 de Abril

- Se agregó documentación interna de código correspondiente a funciones varias de las secciones de código que manejan el procesamiento de lenguaje

### 8 de Abril

- Se confeccionó el diagrama de la solución general del algoritmo.
- Se agregó parte de la descripción detallada del algoritmo de resolución
- Se realizaron pruebas de calidad en diferentes sistemas operativos para asegurar la calidad del código y resolver posibles errores no encontrados hasta el momento.


### 9 de Abril

- Se agregaron las imágenes que corresponden al plan de actividades a la documentación.
- Se integró la documentación interna de reglas y hechos a la documentación externa.
- Se retocaron algunas secciones del manual de usuario y otras secciones de la documentación externa.