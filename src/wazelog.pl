:- module(wazelog, [start/1]).
:- use_module(library(readutil)).
:- use_module(nlp).
:- use_module(lang).
:- use_module(path).
:- use_module(routes).

%Instituto Tecnologico de Costa Rica
%Area Academica de Ingenieria en Computadores
%Lenguajes, Compialdores e Interpretes
%Estudiantes
%		Jose Morales Vargas		2019024270
%		Alejandro Soto Chacon	2019008164
%Semestre I 2021

%reglas y hechos de programa------------------------

%Regla: wazelog_writeln(mensaje).
%Ejemplo: 
%?- wazelog_writeln("Hola, gracias por usar wazelog").
%[wazelog]:::| Hola, gracias por usar wazelog :::|
%Descripcion: Un mensaje de wazelog se compone por un string, el cual se imprime en pantalla utilizando el stream de salida por defecto.
wazelog_writeln(Msg) :-
	format("[Wazelog]:::| ~s :::|\n", [Msg]).

%Regla: spacing.
%Ejemplo: 
%?- spacing.
%=========================================================================================
%Descripcion: Un espaciado es un agregado estetico a la salida en en stream de salida por defecto conformado por una cadena de simbolos '='
spacing :-
	tty_size(_, Width),
	string_repeat("=", Width, Repeated),
	writeln(Repeated).

%Regla: string_repeat(Base, Times, Repeated).
%Ejemplo:
%?- string_repeat("=", 5, B).
%"=====".
%Descripcion: `Repeated` es un parametro de salida el cual toma el valor de la serie de caracteres especificados por `Base` repetidos `Times` veces. 
string_repeat(_, 0, "") :-
	!.
string_repeat(Base, Times, Repeated) :-
	Pred is Times - 1,
	string_repeat(Base, Pred, Next),
	string_concat(Base, Next, Repeated).

%Regla: read_user_input(Result).
%Ejemplo: 
%?- read_user_input(R).
%@usuario: yo voy a la sabana.
%R = ok([svo(nominal(yo, "yo", "yo"), verbal([voy]), nominal(sabana, "la sabana", "sabana"))]).
%Descripcion: Una entrada de un usuario se puede descomponer en una estructura definida por la gramatica libre de contexto, la cual se compone por sujeto, verbo y objeto-complemento. Si esta descomposicion es exitosa, `R` sera `ok(Descomp)` donde `Descomp` es esta descomposicion. `R` es `bye` si el usuario detiene la entrada, y `fail(T)` si hay un fallo sintactico, donde `T` es un token.
read_user_input(Result) :-
	user_title(Title),
	format("@~s: ", [Title]),
	current_input(Stdin),
	read_string(Stdin, "\n", "\r\t ", _, Text), 
	parse_user_input(Text, ParseResult),
	(
		ParseResult = ok(Input),
		not(stop(Input)),
		!,
		ParseResult = Result;

		ParseResult \= ok(_),
		!,
		ParseResult = Result;

		Result = bye
	).

%Regla: translate(lista_nombres, salida, estado)
%Ejemplo:
%  ?- translate([cartago, paraiso], R).
%  R = "Cartago, Paraiso".
%Descripcion: Une una lista de ciudades por coma,
%convirtiendo a su forma mostrable en el proceso.
translate(Path, SRes) :-
	translate(Path, [], SRes).
translate([], S, SRes) :-
	reverse(S, Inv),
	atomics_to_string(Inv, ", ", SRes).
translate([City | Path], S, SRes) :-
	city(City, Trad),
	translate(Path, [Trad | S], SRes).

%Regla: start(Estado).
%Ejemplo:
%  ?- start(R).
%  ...
%  R = stop.
%Descripcion: Realiza una iteracion de
%la rutina de la aplicacion. La salida es
%`stop` si el usuario decide salir, de lo
%contrario `continue`.
start(Then) :-
	run(start, _, Then),
	!.

%Regla: run(estado, paso, Salida).
%Ejemplo:
%  ?- run(start, start, R).
%  ...
%  R = stop.
%Descripcion: Maquina de estados finitos que
%constituye las distintas transiciones en la
%rutina conversacional de WazeLog. El estado
%inicial es `start`. El predicado se invoca
%recursivamente con las continuaciones necesarias.
run(bye, _, stop) :-
	farewell(Text),
	wazelog_writeln(Text).
run(start, _, Then) :-
	ask_in_loop(ask_city(q_src), Src),
	run(Src, start, Then).
run(city(Src), start, Then) :-
	ask_in_loop(ask_city(q_dest), Dest),
	run(Dest, src(Src), Then).
run(city(Dest), src(Src), Then) :-
	ask_in_loop(ask_stops, Paradas),
	run(Paradas, src_dest(Src, Dest), Then).
run(stops(Paradas), src_dest(Src, Dest), continue) :-
	shortest_path_through(Src, Paradas, Dest, Result),
	spacing,
	(
		Result = shortest_path(Ruta, Cost),
		translate(Ruta, StrPath),
		display_path(StrPath, Cost, DisplayPath),
		wazelog_writeln(DisplayPath);

		Result = no_route(From, To),
		city(From, StrFrom),
		city(To, StrTo),
		display_no_route(StrFrom, StrTo, DisplayNoRoute),
		wazelog_writeln(DisplayNoRoute)
	),
	run(bye, _, _),
	spacing.

%Regla: ask_in_loop(predicado, Salida).
%Ejemplo:
%  ?- ask_in_loop(ask_stops, Paradas).
%  ...
%  Paradas = stops([cartago, tresrios]).
%Descripcion: Ejecuta `predicado` con un parametro
%de la forma `repeat(It, Then)`. `It` es la iteracion
%actual, inicialmente `first`. `Then` debe ser unificado
%por el predicado invocado, y puede ser `done(Salida)`,
%en cuyo caso `ask_in_loop` termina, o cualquier otro.
%Si es cualquier otro, se repite el bucle con ese `Then`
%como entrada del predicado, lo cual permite formar maquinas
%de estados finitos. Si el predicado falla, se vuelve a
%ejecutar con la misma entrada si es que esta entrada ya
%tenia la forma `done(_)`. De lo contrario, se vuelve a
%ejecutar con entrada `done(EntradaAnterior)`.
ask_in_loop(Predicate, Input) :-
	ask_in_loop(first, Predicate, Input).
ask_in_loop(Iteration, Predicate, Input) :-
	call(Predicate, repeat(Iteration, Then)),
	!,
	(
		Then = done(Input),
		!;

		ask_in_loop(Then, Predicate, Input)
	).
ask_in_loop(again(Iteration), Predicate, Input) :-
	!,
	ask_in_loop(again(Iteration), Predicate, Input).
ask_in_loop(Iteration, Predicate, Input) :-
	ask_in_loop(again(Iteration), Predicate, Input).

%Regla: ask_city(prompter, repeat(estado, Siguiente))
%Ejemplo:
%  ?- ask_city(q_src, repeat(first, Then))
%  ...
%  Then = done(ok(cartago)).
%Descripcion: Hace una iteracion del proceso de
%preguntar una ciudad (origen o destino). A ser
%utilizada con `ask_in_loop/2`. El "prompter" es
%un predicado que acepta el estado de iteracion actual
%en un primer parametro y una variable sin unificar en
%el segundo, unificando esta variable con la cadena de
%pregunta respectiva. Esto permite unificar preguntas de
%origen y destino en un unico predicado.
ask_city(Prompter, repeat(Iteration, done(Out))) :-
	call(Prompter, Iteration, Prompt),
	wazelog_writeln(Prompt),
	read_user_input(Input),
	(
		Input = bye,
		Out = bye;

		Input = ok(CityRaw),
		key_nominal(CityRaw, Nominal),
		pinpoint(Nominal, City),
		Out = city(City)
	).

%Regla: ask_stops(repeat(estado, Siguiente)).
%Ejemplo:
%  ?- ask_stops(repeat(first, Then))
%  Then = stops([primera]).
%  ?- ask_stops(repeat(stops([primera]), Then))
%  Then = done(stops([primera, segunda])).
%Descripcion: Realiza una iteracion de la rutina
%conversacional que pregunta la lista de paradas
%intermedias que desea el usuario. A ser utilizada
%con `ask_in_loop/2`. La salida final es de la
%forma `stops(ListaDeParadas)`.
ask_stops(repeat(Iteration, Then)) :-
	q_stops(Iteration, Prompt),
	wazelog_writeln(Prompt),
	last_stops(Iteration, Stops),
	read_user_input(Result),
	(
		Result = bye,
		Then = done(bye);

		Result = ok(Input),
		(
			key_nominal(Input, Nominal),
			!,
			not(stop_asking_intermed(Input)),
			pinpoint(Nominal, Stop),
			append(Stops, [Stop], NextStops),
			Then = stops(NextStops);

			stop_asking_intermed(Input),
			Then = done(stops(Stops))
		)
	).

%Regla: last_stops(iteracion, Paradas).
%Ejemplo:
%  ?- last_stops(first, []).
%  true.
%  ?- last_stops(stops(X), Y).
%  X = Y.
%Descripcion: Extrae una lista de paradas a
%partir de una iteracion dada en un bucle de
%`ask_in_loop/2`. Utilizado por `ask_stops`.
last_stops(first, []).
last_stops(stops(Stops), Stops).
last_stops(again(Iteration), Stops) :-
	last_stops(Iteration, Stops).

%Regla: pinpoint(forma_nominal, Parada).
%Ejemplo:
%  ?- pinpoint(nominal(banco, "banco", "banco"), Stop).
%  ...
%  Stop = tresrios.
%Descripcion: Pregunta en recursion al usuario
%por la ubicacion de un lugar desconocido. La
%pregunta varia segun si WazeLog es capaz de
%reconocer que la entrada se trata de un tipo
%de lugar.
pinpoint(nominal(Stop, _, _), Stop) :-
	city(Stop, _),
	!.
pinpoint(nominal(Place, Orig, Bare), Stop) :-
	(
		place_type(Place),
		!,
		q_which(Bare, Prompt);

		q_direction(Orig, Prompt)
	),
	wazelog_writeln(Prompt),
	read_user_input(ok(Input)),
	key_nominal(Input, Nominal),
	pinpoint(Nominal, Stop).

%Regla: stop_asking_intermed(Input).
%Ejemplo: 
%?- stop_asking_intermed([exclamation(negative)]).
%true.
%Descripcion: Describe si se debe dejar de preguntar al usuario por destinos intermedios. Esta condicion se da solo si una exclamacion negativa forma parte de la respuesta del usuario (la cual se encuentra descompuesta en Input).
stop_asking_intermed(Input) :- 
	contains_term(exclamation(negative), Input).

%Regla: stop(entrada).
%Ejemplo:
%  ?- stop([exclamation(bye)]).
%  true.
%  ?- stop([]).
%  false.
%Descripcion: Tiene exito solamente si existe
%una exclamacion de terminacion en la entrada,
%indicando por tanto que el programa debe terminar.
stop(Input) :- 
	contains_term(exclamation(bye), Input).
