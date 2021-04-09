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

# Wazelog

## 1.1. Descripción de los hechos y reglas implementadas

### Hechos: `arco(origen, destino, distancia, tiempo, con_presa).`

**Ejemplo:**

```prolog
?- arco(cartago, paraiso, D, Tmin, Tmax).
D = 10,
Tmin = 10,
Tmax = 20.
```

**Descripción:** Declara arcos unidireccionales (el grafo es mixto).

### Hechos: `arco_bi(lugar1, lugar2, distancia, tiempo, con_presa).`

**Ejemplo:**

```prolog
?- arco_bi(sanjose, cartago, D, Tmin, Tmax).
D = 20,
Tmin = 20,
Tmax = 40.
```

**Descripción:** Declara arcos bidireccionales (el grafo es mixto).

### Hechos: `city(lugar, nombre).`

**Ejemplo:**

```prolog
?- city(sanjose, N).
N = "San José".
```

**Descripción:** Declara las ciudades conocidas y sus nombres.

### Hechos: `supported_lang(lenguaje).`

**Ejemplo:**

```prolog
?- supported_lang(es).
true.
```

**Descripción:** Enumera los idiomas soportados por la aplicación.

### Hecho dinámico: `lang(lenguaje).`

**Ejemplo:**

```prolog
?- lang(L), farewell(L, F).
F = "¡Muchas gracias por utilizar WazeLog!".
?- set_lang(en), lang(L), farewell(L, F).
F = "Thank you for using WazeLog!".
```

**Descripción:** `lang/1` es un predicado dinámico, es decir, cuyos hechos pueden ser alterados en tiempo de ejecución.  Esto permite, de manera conveniente, escoger un idioma sin tener que acarrear una variable de lenguaje por toda la aplicación. El idioma se altera con `set_lang/1`.

### Hecho: `sentence_sep(token).`

**Ejemplo:**

```prolog
?- sentence_sep('.').
true.
```

**Descripción:** Enuncia los tokens que separan oraciones.

### Regla: `append_space(sin_espacio, ConEspacio).`

**Ejemplo:**

```prolog
?- append_space("a", "a ").
true.
```

**Descripción:** Agrega un espacio al final de una cadena solamente si la entrada no es la cadena vacía.

### Regla: `arco(origen, destino, distancia, tiempo, con_presa).`

**Ejemplo:**

```prolog
?- arco(cartago, sanjose, D, Tmin, Tmax).
D = 20,
Tmin = 20,
Tmax = 40.
```

**Descripción:** Descompone arcos bidireccionales en dos arcos unidireccionales, simplificando la lógica de grafo.

### Regla: `ask_city(prompter, repeat(estado, Siguiente)).`

**Ejemplo:**

```prolog
?- ask_city(q_src, repeat(first, Then))
...
Then = done(ok(cartago)).
```

**Descripción:** Hace una iteración del proceso de preguntar una ciudad (origen o destino). A ser utilizada con `ask_in_loop/2`. El "prompter" es un predicado que acepta el estado de iteración actual en un primer parámetro y una variable sin unificar en el segundo, unificando esta variable con la cadena de pregunta respectiva. Esto permite unificar preguntas de origen y destino en un único predicado.

### Regla: `ask_in_loop(predicado, Salida).`

**Ejemplo:**

```prolog
?- ask_in_loop(ask_stops, Paradas).
...
Paradas = stops([cartago, tresrios]).
```

**Descripción:** Ejecuta `predicado` con un parámetro de la forma `repeat(It, Then)`. `It` es la iteración actual, inicialmente `first`. `Then` debe ser unificado por el predicado invocado, y puede ser `done(Salida)`, en cuyo caso `ask_in_loop` termina, o cualquier otro. Si es cualquier otro, se repite el bucle con ese `Then` como entrada del predicado, lo cual permite formar máquinas de estados finitos. Si el predicado falla, se vuelve a ejecutar con la misma entrada si es que esta entrada ya tenía la forma `done(_)`. De lo contrario, se vuelve a ejecutar con entrada `done(EntradaAnterior)`.

### Regla: `ask_stops(repeat(estado, Siguiente)).`

**Ejemplo:**

```prolog
?- ask_stops(repeat(first, Then))
Then = stops([primera]).
?- ask_stops(repeat(stops([primera]), Then))
Then = done(stops([primera, segunda])).
```

**Descripción:** Realiza una iteración de la rutina conversacional que pregunta la lista de paradas intermedias que desea el usuario. A ser utilizada con `ask_in_loop/2`. La salida final es de la forma `stops(ListaDeParadas)`.

### Regla: `ast_join(existente, agregado, Salida).`

**Ejemplo:**

```prolog
?- ast_join(nomatch, filler(la, "la"), R1), ast_join(R1, nominal(mesa, "mesa"), R2).
R1 = nominal('', "la", ""),
R2 = nominal(mesa, "la mesa", "mesa").
```

**Descripción:** Construye un árbol de sintaxis, con algunas interpretaciones semánticas incluidas, a partir de un estado previo del mismo árbol y un componente siguiente a agregar. El átomo `nomatch` se utiliza como árbol previo para indicar que no existía uno anteriormente.

### Regla: `atoms_to_words(Atoms, Words).`

**Ejemplo:**

```prolog
?- atoms_to_words([sanjose, manzana, cartago],Words).
Words = [word(sanjose, "sanjose"), word(manzana, "manzana"), word(cartago, "cartago")].
```

**Descripción:** "Traduce" una lista de átomos `Atoms` a una lista de palabras `word(átomo, string)`.

### Regla: `before_nominal(token).`

**Ejemplo:**

```prolog
?- before_nominal(el).
true.
```

**Descripción:** Enumera palabras de relleno que se espera serán seguidas inmediatamente por una forma nominal. Esto incluye en el caso del español a los artículos y algunas preposiciones. Esto permite incluir estas palabras en la forma articulada original del término nominal sin afectar tener que afectar su átomo identificador con respecto a no haber escrito la palabra antecesora.

### Regla: `classify(word(Atom, Orig), Type).`

**Ejemplo:**

```prolog
?- classify(word(encuentro, "encuentro"), Type).
Type = verbal(encuentro).
```

**Descripción:** Clasifica palabras según su función en una oración, ya sea en tipo verbal, nominal, exclamación o relleno.

### Regla: `clause(oración).`

**Ejemplo:**

```prolog
?- clause(nominal(yo, "yo", "yo")).
true.
?- clause(exclamation(affirmative)).
true.
```

**Descripción:** Tiene éxito solo si la oración en cuestión es una oración válida. Esto ocurre para todas las sub-expresiones válidas, excepto formas verbales solitarias sin asociación jerárquica.

### Regla: `contraction(token, Expansión).`

**Ejemplo:**

```prolog
?- contraction(al, C).
C = [a, el].
```

**Descripción:** Expande contracciones en formas equivalentes.

### Regla: `display_no_route(desde, hacia, Texto).`

**Ejemplo:**

```prolog
?- display_no_route("A", "B", T).
T = "No hay una ruta conocida de A a B.".
```

**Descripción:** Define el texto que se muestra cuando no existe una ruta entre un origen y un destino.

### Regla: `display_path(ruta, costo, Salida).`

**Ejemplo:**

```prolog
?- display_path("San José, Cartago", cost(1, 2, 3), T).
Salida = "Su ruta sería San José, Cartago. Longitud estimada de 1 Km. Duración 2-3 min.".
```

**Descripción:** Define el mensaje de ruta y costos a partir de esta misma información.

### Regla: `exclamation(token, tipo).`

**Ejemplo:**

```prolog
?- exclamation(hola, T).
T = greeting.
```

**Descripción:** Relaciona palabras clave de oraciones consideradas como exclamativas con el tipo interno de exclamación. Al mismo tiempo, esta regla identifica el hecho de que tales palabres clave conformen este tipo de oraciones.

### Regla: `expand(Tokens, Expanded).`

**Ejemplo:**

```prolog
?- expand([word(al, "al"), word(del, "del"), word(alto, "Alto")], Expanded).
Expanded = [word(a, "a"), word(el, "el"), word(de, "de"), word(el, "el"), word(alto, "Alto")].
```

**Descripción:** La regla toma una lista de palabras representadas en la lista `Tokens` como elementos `word(átomo, string)` y separa las palabras que sean identificadas como contracciones en tokens distintos, la lista conformada por las palabras procesadas por `expand` es el argumento de salida Expanded.

### Regla: `farewell(Salida).`

**Ejemplo:**

```prolog
?- farewell(O).
O = "¡Muchas gracias por utilizar WazeLog!".
```

**Descripción:** Define la despedida.

### Regla: `filler(Word).`

**Ejemplo:**

```prolog
?- filler(por).
true.
```

**Descripción:** Toma un átomo que representa una palabra e indica si la palabra es o no relevante para el análisis de lenguaje.

### Regla: `key_nominal(SVO, nominal(A, Orig, Bare)).`

**Ejemplo:**

```prolog
?- key_nominal([svo(nominal('', "", ""), verbal([voy]), nominal(alto, "el Alto", "Alto"))], nominal(A, Orig, Bare).
A = alto.
Orig = "el Alto".
Bare = "Alto".
```

**Descripción:** La regla toma una oración representada en SVO como una estructura sujeto-verbo-objeto y busca su sustantivo complemento. Mayoritariamente utilizada para obtener el nombre de una ciudad, el cual siempre se encuentra en la posición de complemento en la voz activa.

### Regla: `last_stops(iteración, Paradas).`

**Ejemplo:**

```prolog
?- last_stops(first, []).
true.
?- last_stops(stops(X), Y).
X = Y.
```

**Descripción:** Extrae una lista de paradas a partir de una iteración dada en un bucle de `ask_in_loop/2`. Utilizado por `ask_stops`.

### Regla: `lex(Input, Tokens).`

**Ejemplo:**

```prolog
?- lex("voy a San José", Tokens).
Tokens = [word(voy, "voy"), word(a, "a"), word(san, "San"), word(jose, "José")].
```

**Descripción:** Toma un string `Input` que representa una oración y obtiene una lista de tokens `word(átomo, string)`, con cada token correspondiente a una de las palabras de la oración.

### Regla: `loop.`

**Ejemplo:**

```prolog
?- loop.
...
```

**Descripción:** Bucle principal. Ejecuta en repetición la rutina conversacional de WazeLog hasta que el usuario indique que desea salir de la aplicación.

### Regla: `main(Argv).`

**Ejemplo:**

```prolog
?- main(["--lang", es]).
...
```

**Descripción:** Entrypoint de la aplicación. Se debe proporcionar un lenguaje por la línea de comandos (parámetro `Argv`).

### Regla: `nominal(N).`

**Ejemplo:**

```prolog
?- nominal(sanjose).
true.
```

**Descripción:** Toma un átomo que representa una palabra e indica si dicha palabra es o no un sustantivo.

### Regla: `nominal_join(izquierdo, derecho, Salida).`

**Ejemplo:**

```prolog
?- nominal_join(nominal('', "la", ""), nominal("sabana", sabana), R).
R = nominal(sabana, "la sabana", "sabana").
```

**Descripción:** Concatena dos formas nominales en un nominal compuesto.

### Regla: `override_distance(actual, distancia, vecino, nodo, pila, nodos, NuevaPila, NuevosNodos).`

**Ejemplo:**

Se omite, el predicado es de uso interno.

**Descripción:** Si la distancia a un nodo a través de otro resulta ser menor, entonces cambia su mejor padre conocido. De lo contrario no realiza ninguna acción. Parte del algoritmo de Dijkstra.

### Regla: `parse_user_input(Input, Result).`

**Ejemplo:**

```prolog
?- parse_user_input("Voy a Cartago", Result).
Result = ok([svo(nominal('', "", ""), verbal([voy]), nominal(cartago, "cartago", "Cartago"))]).
```

**Descripción:** Toma un string input del usuario y lo separa en sus diferentes elementos.

### Regla: `pinpoint(forma_nominal, Parada).`

**Ejemplo:**

```prolog
?- pinpoint(nominal(banco, "banco", "banco"), Stop).
...
Stop = tresrios.
```

**Descripción:** Pregunta en recursión al usuario por la ubicación de un lugar desconocido. La pregunta varía según si WazeLog es capaz de reconocer que la entrada se trata de un tipo de lugar.

### Regla: `place_type(lugar).`

**Ejemplo:**

```prolog
?- place_type(hospital).
true.
?- place_type(automercado).
false.
```

**Descripción:** Enuncia las formas nominales que se conoce que se refieren siempre a tipos de lugares en vez de lugares específicos. Se cumple si existe un hecho correspondiente para el idioma en uso.

### Regla: `q_dest(iteración, Prompt).`

**Ejemplo:**

```prolog
?- q_dest(first, Prompt).
Prompt = "Perfecto, ¿cuál es su destino?".
```

**Descripción:** Define la pregunta de destino. Esta pregunta puede cambiar si se ingresa una respuesta incorrecta la primera vez, lo cual se expresa en el parámetro de iteración.

### Regla: `q_direction(lugar, Prompt).`

**Ejemplo:**

```prolog
?- q_direction("AutoMercado").
Prompt = "¿Dónde se encuentra AutoMercado?".
```

**Descripción:** Define la pregunta de ubicación de un lugar.

### Regla: `q_src(iteración, Prompt).`

**Ejemplo:**

```prolog
?- q_src(first, Prompt).
Prompt = "Bienvenido a WazeLog, la mejor lógica de llegar a su destino. Por favor, indíqueme dónde se encuentra.".
```

**Descripción:** Define la pregunta de origen. Esta pregunta puede cambiar si se ingresa una respuesta incorrecta la primera vez, lo cual se expresa en el parámetro de iteración.

### Regla: `q_stops(iteración, Prompt).`

**Ejemplo:**

```prolog
?- q_stops(first, Prompt).
Prompt = "Genial, ¿algún destino intermedio?".
```

**Descripción:** Define las preguntas que solicitan el primer y los demás destinos intermedios.

### Regla: `q_which(lugar, Prompt).`

**Ejemplo:**

```prolog
?- q_which("supermercado").
Prompt = "¿Cuál supermercado?".
```

**Descripción:** Define la pregunta para especificar una ubicación concreta a partir de un tipo de ubicación.

### Regla: `read_user_input(Result).`

**Ejemplo:**

```prolog
?- read_user_input(R).
@usuario: yo voy a la sabana.
R = ok([svo(nominal(yo, "yo", "yo"), verbal([voy]), nominal(sabana, "la sabana", "sabana"))]).
```

**Descripción:** Una entrada de un usuario se puede descomponer en una estructura definida por la gramática libre de contexto, la cual se compone por sujeto, verbo y objeto-complemento. Si esta descomposición es exitosa, `R` será `ok(Descomp)` donde `Descomp` es esta descomposición. `R` es `bye` si el usuario detiene la entrada, y `fail(T)` si hay un fallo sintáctico, donde `T` es un token.

### Regla: `run(estado, paso, Salida).`

**Ejemplo:**

```prolog
?- run(start, start, R).
...
R = stop.
```

**Descripción:** Máquina de estados finitos que constituye las distintas transiciones en la rutina conversacional de WazeLog. El estado inicial es `start`. El predicado se invoca recursivamente con las continuaciones necesarias.

### Regla: `sentence(tokens, Resto, Oración).`

**Ejemplo:**

```prolog
?- sentence([word(yo, "Yo"), word(estoy, "estoy"), word(en, "en"), word(cartago, "Cartago"), punct('.')], R, S).
R = [],
S = svo(nominal(yo, "Yo", "Yo"), verbal([estoy]), nominal(cartago, "Cartago", "Cartago")) ;
```

**Descripción:** Parsea una oración a partir de un flujo de entrada. Su salida es tanto la oración como la lista de tokens que la suceden y que deben luego parsearse como más oraciones. Una oración puede ser una forma exclamativa, una forma nominal independiente o una estructura subjeto-verbo-objeto (SVO).

### Regla: `set_lang(lenguaje).`

**Ejemplo:**

```prolog
?- set_lang(es).
true.
```

**Descripción:** Cambia el idioma en uso. El idioma ingresado debe ser soportado según reporte `supported_lang/1`.

### Regla: `shortest_path(Source, Target, Path, Cost).`

**Ejemplo:**

```prolog
?- shortest_path(sanpedro, sanjose, Path, Cost).
Path = [sanpedro, sanjose].
Cost = 3
```

**Descripción:** Si existen una o varias rutas del nodo Source al nodo Target, esta regla obtiene la ruta más corta entre dos nodos mediante una implementaión del algoritmo del camino más corto de Dijkstra.

### Regla: `shortest_path_through(Source, Stops, Target, Result).`

**Ejemplo:**

```prolog
?- shortest_path_through(sanjose, [tresrios], cartago, Result).
Result = [sanjose, sanpedro, tresrios, taras, cartago].
```

**Descripción:** Si existe una ruta de un punto origen Source a un punto destino Target, con las paradas intermedias Stops, la regla evalua las posibles rutas óptimas mediante una implementación del algoritmo del camino más corto de dijkstra adptado para tomar en cuenta la existencia de los destinos intermedios.

### Regla: `spacing.`

**Ejemplo:**

```prolog
?- spacing.
=========================================================================================
```

**Descripción:** Un espaciado es un agregado estético a la salida en en stream de salida por defecto conformado por una cadena de símbolos '='

### Regla: `start(Estado).`

**Ejemplo:**

```prolog
?- start(R).
...
R = stop.
```

**Descripción:** Realiza una iteración de la rutina de la aplicación. La salida es `stop` si el usuario decide salir, de lo contrario `continue`.

### Regla: `stop(entrada).`

**Ejemplo:**

```prolog
?- stop([exclamation(bye)]).
true.
?- stop([]).
false.
```

**Descripción:** Tiene éxito solamente si existe una exclamación de terminación en la entrada, indicando por tanto que el programa debe terminar.

### Regla: `stop_asking_intermed(Input).`

**Ejemplo:**

```prolog
?- stop_asking_intermed([exclamation(negative)]).
true.
```

**Descripción:** Describe si se debe dejar de preguntar al usuario por destinos intermedios. Esta condición se da solo si una exclamación negativa forma parte de la respuesta del usuario (la cual se encuentra descompuesta en Input).

### Regla: `string_repeat(Base, Times, Repeated).`

**Ejemplo:**

```prolog
?- string_repeat("=", 5, B).
"=====".
```

**Descripción:** `Repeated` es un parámetro de salida el cuál toma el valor de la serie de caracteres especificados por `Base` repetidos `Times` veces.

### Regla: `test_neighbors(vecinos, actual, distancia, pila, nodos, NuevaPila, NuevosNodos).`

**Ejemplo:**

Se omite, el predicado es de uso interno.

**Descripción:** Considera cada uno de los vecinos de un nodo que está siendo expandido y de ser suficientes las condiciones los agrega a nodos abiertos. Parte del algoritmo de Dijkstra.

### Regla: `traceback(origen, destino, nodos, Ruta, Costo).`

**Ejemplo:**

```prolog
?- traceback(a, a, nodes{}, R, C).
R = [a],
C = cost(0, 0, 0).
```

**Descripción:** Reconstruye una ruta y su costo a partir de una solución dada por el algoritmo de Dijkstra.

### Regla: `translate(lista_nombres, salida, estado).`

**Ejemplo:**

```prolog
?- translate([cartago, paraiso], R).
R = "Cartago, Paraíso".
```

**Descripción:** Une una lista de ciudades por coma, convirtiendo a su forma mostrable en el proceso.

### Regla: `unbounded(tokens, Salida).`

**Ejemplo:**

Ver `sentence/3`.

**Descripción:** Parsea una entrada completa ("no delimitada", por tanto el nombre del predicada). El resultado es o una lista de oraciones o una indicación de fallo.

### Regla: `unclassified(token).`

**Ejemplo:**

```prolog
?- unclassified(que).
true.
```

**Descripción:** Declara que una palabra es explícitamente no clasificada y por tanto debe considerarse como de relleno (ver `filler/1`) en vez de como nominal, ya que lo último se asume por defecto.

### Regla: `undecorate(Cs, Us).`

**Ejemplo:**

```prolog
?- undecorate(['á','é','í','ó','ú','ü'], Us).
Us = ['a','e','i','o','u','u'].
```

**Descripción:** Toma una lista de caracteres y obtiene su version sin decoraciones(acentos y diéresis) para evitar conflictos a la hora de procesar datos.

### Regla: `user_title(Titulo).`

**Ejemplo:**

```prolog
?- user_title(T).
T = "Usuario".
```

**Descripción:** Define el título del usuario.

### Regla: `verbal(token).`

**Ejemplo:**

```prolog
?- verbal(estoy).
true.
```

**Descripción:** Tiene éxito si el token en cuestión es un componente de forma verbal. Es decir, esta regla identifica a los verbos.

### Regla: `visit(origen, destino, actual, distancia, nodo, pila, nodos, SiguientesNodos).`

**Ejemplo:**

Se omite, el predicado es de uso interno.

**Descripción:** Visita un nodo (parte del algoritmo de Dijkstra).

### Regla: `wazelog_writeln(mensaje).`

**Ejemplo:**

```prolog
?- wazelog_writeln("Hola, gracias por usar wazelog").
[wazelog]:::| Hola, gracias por usar wazelog :::|
```

**Descripción:** Un mensaje de wazelog se compone por un string, el cual se imprime en pantalla utilizando el stream de salida por defecto.

### Regla: `well_formed(Expresion).`

**Ejemplo:**

```prolog
?- well_formed(nominal(yo,"yo","yo")).
true.
```

**Descripción:** Evalúa si una expresión está formada de manera correcta. La expresión puede ser de tipo verbal, nominal, exclamación, u otro tipo.

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

Antes de proceder con la explicación del algoritmo que describe el funcionamiento del programa, es esencial ver como interpreta el programa la entrada provista por el usuario. En el caso de la versión de Wazelog desarrollada, se utiliza una gramática libre de contexto descrita por el siguiente Bach-Naur Form (BNF):

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

- Especificación.
- Presentaciones, grabaciones y material de clase.
- <https://www.swi-prolog.org/pldoc/man?section=readutil>.
- <https://www.swi-prolog.org/pldoc/man?section=dicts>.
- <https://www.swi-prolog.org/pldoc/doc/_SWI_/library/heaps.pl>.
- <https://www.swi-prolog.org/pldoc/man?section=encoding>.
- <https://www.swi-prolog.org/pldoc/man?section=strings>.
- <https://www.swi-prolog.org/pldoc/man?section=strings>.
- <https://www.swi-prolog.org/pldoc/man?section=dynpreds>.
- <https://www.swi-prolog.org/pldoc/man?predicate=format/3>.


## 1.9. Bitácoras
