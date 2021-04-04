## 19 de marzo

- Se publica la especificación del trabajo.
- Se forman grupos de trabajo.
- El equipo de trabajo realiza una sesión de discusión inicial, en la cual se
  detallan algunas aclaraciones sobre aspectos de la especificación. En
  respuesta a lo mismo, se decide que será necesaria otra reunión, a definir en
  el futuro, antes de poder definir actividades asignadas.
- Se completa la teoría académica necesaria para la realización del proyecto.

## 27 de marzo

- El equipo de trabajo, disponiendo ahora de más información y aclaración sobre
  la intención general en lo que respecta al diseño del proyecto, realiza una
  llamada para acordar la estructura general planteada, el diseño propuesto, el
  plan de actividades y fechas estimadas de entrega.
- El otro compañero de equipo realiza sus actividades recién asignadas durante
  la misma llamada. Las actividades que me fueron asignadas serán realizadas en
  momentos posteriores.

## 29 de marzo

- Se definen los hechos de un grafo de ejemplo, el cual será utilizado para
  probar la futura implementación de búsqueda de mejor ruta. El grafo fue
  tomado de
  <https://upload.wikimedia.org/wikipedia/commons/5/57/Dijkstra_Animation.gif>.
- Se agrega un predicado para buscar la ruta más corta de un origen a un
  destino, pasando obligatoriamente por una lista de paradas intermedias.  De
  momento el predicado siempre falla, ya que está escrito en términos de un
  algoritmo de búsqueda de ruta más corta entre solamente dos nodos que no ha
  sido implementado. Específicamente, mientras que la lista de intermedios no
  sea vacía, busca la ruta más corta entre el origen y el primer intermedio, y
  la une con la ruta más corta dada por el mismo predicado cuando el origen es
  lo que antes era el primer intermedio, los intermedios son los mismos excepto
  que ya no contienen al que antes era el primero, y el destino se preserva.
  Para el caso donde la lista de intermedios es vacía, se define a la ruta más
  corta pasando por cero intermedios como la ruta «directa» más corta entre el
  origen y el destino.
- Se define el costo de la ruta con paradas como la suma de los costos mínimos
  de las subrutas, las cuales a su vez se definen como la suma de las
  distancias entre nodos en kilómetros. Lo anterior es en respuesta a
  indicaciones generales del profesor a los grupos de trabajo.
- Tras realizar pruebas de viabilidad, se escoge al algoritmo de Dijkstra para
  el cálculo de rutas directas más cortas entre nodos. Las alternativas,
  incluyendo algunos abusos de las técnicas de backtracking de Prolog, fueron
  descartadas por múltiples inconvenientes. El más importante de ellos es que
  no existen garantías de terminación correcta con tamaños de grafo no
  particularmente grandes, pero tampoco triviales. Además, el compañero de
  trabajo intentó prototipar una solución basada en filtrado de todas las
  posibles rutas, pero encontramos problemas análogos.
- Para la implementación del algoritmo de Dijkstra se decide utilizar las
  siguientes bibliotecas, ambas ofrecidas por SWI-Prolog:
  - `library(dicts)`: Se utilizarán diccionarios para llevar cuenta de las
	mejores distancias, los mejores padres y el estado de visitado.
  - `library(heaps)`: Se utilizará para la cola de prioridad de nodos sin
	visitar que requiere el algoritmo de Dijkstra.
- Se implementan las condiciones iniciales del algoritmo de Dijkstra, con lo
  cual se ponen a prueba inicial ambas bibliotecas, en intención de encontrar
  si la solución será eficaz.
- Se termina la implementación del algoritmo de Dijkstra. Se prueba la misma
  contra el grafo de prueba y se obtienen resultados satisfactorios,
  demostrando que la idea original es funcional.
- Para el procesamiento de lenguaje natural, se diseña de manera informal una
  gramática libre de contexto, que será declarada en forma EBNF posteriormente.
  La entrada de usuario se parseará como una lista de oraciones. Se definen
  hechos para los tokens que separarán oraciones.
- Se define un tipo especial de oración: las «exclamativas». Estas incluyen a
  palabras como «sí», «no» y «hola». Todas las demás oraciones esperan una
  estructura sujeto-verbo-objeto potencialmente recursiva. Las oraciones
  exclamativas pueden terminarse sin un separador explícito, con tal de
  acomodar errores gramaticales que posee la especificación. Se incluyen
  algunas formas verbales y palabras de relleno que se observan en la
  especificación.
- Se implementa análisis léxico de cadenas de entrada. Los caracteres
  alfabéticos consecutivos se vuelven tokens que contienen a tanto un átomo
  reducido (reduciendo, por ejemplo, mayúsculas) y una cadena con la forma
  original, ya que esta última puede ser necesaria a futuro.  Todos los demás
  caracteres no vacíos se vuelven tokens de puntuación.
- Se implementa la reducción de tildes al transformar segmentos de caracteres
  en átomos.
- Se implementa el parsing de una lista de oraciones a partir de una lista de
  tokens. Las oraciones pueden partirse por separadores explícitos, y la última
  oración de la entrada no necesita separador final. No se implementa todavía
  el parsing de oraciones propiamente, por lo cual esto no sirve de momento.
- Se agrega la implementación de parseo de oraciones, siguiendo las reglas
  gramaticales establecidas con anterioridad. Todavía hace falta agrupar grupos
  de árboles sintácticos para estructurar propiamente las oraciones, así como
  la unión de formas nominales en compuestos.
- Se agrega la lógica faltante para construir árboles sintácticos SVO conforme
  se parsean las oraciones no exclamativas. Esta lógica toma en cuenta
  situaciones como predicados anidados, así como mantener una sola estructura
  SVO sin anidación al agregar un nominal inmediatamente después de un SVO con
  objeto tácito.
- Se agrega un prototipo de predicado para determinar si un AST construido es
  válido o no. Se implementará luego.

  ## 30 de marzo

- La clasificación de formas nominales ahora resulta en tres elementos en vez
  de los dos anteriores: un átomo, una cadena «original» y una cadena con
  artículos eliminados. Esto resuelve un problema con conversaciones donde
  lugares como «La Sabana» tienen al artículo como parte de su nombre completo,
  pero otros como «el TEC» no lo tienen.
- Se ajustan aspectos de la interfaz de usuario necesarios para acomodar el
  cambio anterior.
- El cambio anterior rompió el manejo de oraciones exclamativas debido a que no
  se cambió un `nominal(_, _)` por un `nominal(_, _, _)`. Se arregla esto.

## 2 de abril

- Se termina de implementar la verificación de validez de oraciones que fue
  propuesta el 29 de marzo. En particular, las estructuras SVO deben de cumplir
  las siguientes condiciones. Si hay un componente verbal presente, deben tener
  un objeto/predicado válido según aplicación recursiva de estas reglas. Si no
  hay un verbo presente, debe haber un sujeto válido según estas reglas. Un
  nominal se considera válido si su átomo no es el átomo vacío. Una forma
  verbal es válida si no está constituida por una lista vacía. Una forma
  nominal de primer nivel es una oración válida, pero una forma verbal de
  primer nivel, sin estructura SVO, es un error.

## 3 de abril

- Se elimina el grafo de prueba original, ya que ahora puede ser reemplazado
  con uno más apropiado al contexto del proyecto.
- Se separan los hechos contextuales (lenguaje y rutas) de la lógica.
- Se implementan aspectos de conversación para casos donde no existe una ruta
  con los puntos solicitados.
- Se limpian y reescriben partes de `wazelog.pl`. No se altera la lógica
  observable.

## 4 de abril

- Se reestructura el predicado `parse_user_input/3`, ahora
  `parse_user_input/2`, para contener su salida en una sola unidad estructural,
  en vez de tener dos parámetros de salida para ello. Se modifica código
  relacionado para considerar este cambio.
- Se agrega el verbo «está» a la lista de formas verbales.
- Se implementa repetición de la rutina principal del programa. De momento no
  existe suficiente transfondo para que esto funcione correctamente.
- Se implementa el bucle recién mencionado, así como condiciones de terminación
  con exclamaciones como «adiós» y variantes.
