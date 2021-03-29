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
