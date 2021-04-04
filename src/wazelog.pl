:- module(wazelog, [start/0]).
:- use_module(library(readutil)).
:- use_module(nlp).
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
%Ejemplo: wazelog_writeln("Hola, gracias por usar wazelog") ->>(mensaje en StdOut)->>
%			[wazelog]:::| Hola, gracias por usar wazelog :::|
%Descripción: Un mensaje de wazelog se compone por un string, el cual se imprime en pantalla utilizando el stream de salida por defecto.
wazelog_writeln(Msg):-write("[Wazelog]:::| "), write(Msg), write(" :::| \n").

%Regla: spacing.
%Ejemplo: spacing ->>(salida en StdOut)->>
%			=========================================================================================
%Descripción: Un espaciado es un agregado estético a la salida en en stream de salida por defecto conformado por una cadena de símbolos '='
spacing :-
	tty_size(Width, _),
	string_repeat("=", Width, Repeated),
	writeln(Repeated).

string_repeat(_, 0, "") :-
	!.
string_repeat(Base, Times, Repeated) :-
	Pred is Times - 1,
	string_repeat(Base, Pred, Next),
	string_concat(Base, Next, Repeated).

%Regla: q_direction(Place).
%Ejemplo: q_direction("AutoMercado"). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Dónde se encuentra AutoMercado?  :::|
%Descripción: Utiliza la regla de wazelog_writeln para comunicarle al usuario una pregunta sobre la localización de un lugar destino intermedio. 
q_direction(Lugar) :-
	format(atom(Msg), "Donde se encuentra ~w?", [Lugar]),
	wazelog_writeln(Msg).

%Ejemplo: q_which("supermercado"). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Cuál supermercado?  :::|
%Descripción: Utiliza la regla de wazelog_writeln para comunicar al usuario una pregunta para que se especifique precisamente cual lugar de tipo Place es el que quiere tomar como destino intermedio. 
q_which(Place) :-
	format(atom(Msg), "Cual ~w?", [Place]),
	wazelog_writeln(Msg).

%Regla: read_user_input(Descomp, Test)
%Ejemplo: read_user_input(A, B).
%			@usuario: yo voy a cartago.
%		A = [svo(nominal(yo, "yo", "yo"), verbal([voy]), nominal(cartago, "cartago", "cartago"))],
%		B = ok.
%Descripción: Una entrada de un usuario se puede descomponer en una estructura definida por la gramática libre de contexto, la cual se compone por sujeto, verbo y objeto-complemento. Esta descomposición se denomina Descomp. La prueba de coherencia de dicha descomposición está dada por el argumento Test, el cual toma el valor "ok" si la oración es coherente, un valor de los términos conflictivos si no hay coherencia. 
read_user_input(Descomp, Test) :-
	write("@Usuario: "),
	current_input(Stdin),
	read_string(Stdin, "\n", "\r\t", _, Text), 
	parse_user_input(Text, Descomp, Test).

translate([], S, SRes) :-
	atomics_to_string(S, ", ", SRes),
	!.
translate([City | Path], S, SRes) :-
	city(City, Trad),
	translate(Path, [Trad | S], SRes).

start :-
	ask_in_loop(ask_src, Src),
	ask_in_loop(ask_dest, Dest),
	intermed([], Paradas),
	shortest_path_through(Src, Paradas, Dest, Result),
	spacing,
	(
		Result = shortest_path(Ruta, Peso),
		reverse(Ruta, RutaInv),
		translate(RutaInv, [], StrPath),
		format("Su ruta seria ~w. Longitud estimada de ~d Km.\n", [StrPath, Peso]);

		Result = no_route(From, To),
		city(From, StrFrom),
		city(To, StrTo),
		format("No hay una ruta conocida entre ~w y ~w.\n", [StrFrom, StrTo])
	),
	!,
	writeln("Muchas Gracias por utilizar WazeLog!"),
	spacing.

ask_in_loop(Predicate, Input) :-
	ask_in_loop(first, Predicate, Input).
ask_in_loop(Repeat, Predicate, Input) :-
	call(Predicate, Tentative, repeat(Repeat, Then)),
	!,
	(
		Then = done,
		!,
		Input = Tentative;

		ask_in_loop(Then, Predicate, Input)
	).
ask_in_loop(_, Predicate, Input) :-
	ask_in_loop(again, Predicate, Input).

ask_src(Src, repeat(Repeat, done)) :-
	(
		Repeat = first,
		wazelog_writeln("Bienvenido a WazeLog, la mejor logica de llegar a su destino, por favor indiqueme donde se encuentra.");

		Repeat = again,
		wazelog_writeln("Creo que hay un malentendido, por favor, me puede repetir, cual es su ubicacion actual?")
	),
	read_user_input(SrcRaw, ok),
	key_nominal(SrcRaw, nominal(Src, _, _)),
	city(Src, _).

ask_dest(Dest, repeat(Repeat, done)) :-
	(
		Repeat = first,
		wazelog_writeln("Perfecto, cual es su destino?");

		Repeat = again,
		wazelog_writeln("Mis disculpas, no le he entendido, puede reformular su respuesta? A donde se dirige?")
	),
	read_user_input(DestRaw, ok),
	key_nominal(DestRaw, nominal(Dest, _, _)),
	city(Dest, _).

%lista debe comenzar []
%Regla:
%Ejemplo:
%Descripción:
intermed(Lista, Stops) :-
	(
		Lista = [],
		wazelog_writeln("Genial, Algun destino intermedio?");

		Lista = [_ | _],
		wazelog_writeln("Algun otro destino intermedio?")
	),
	read_user_input(Input, Test),
	answer(Input, Lista, Test, Stops).

intermed_extra(Lista, PlaceType, Stops) :-
	q_which(PlaceType), 
	read_user_input(Input, _),
	key_nominal(Input, nominal(_, Place, _)),
	q_direction(Place),
	read_user_input(Input2, Test2),
	answer(Input2, Lista, Test2, Stops).

%Regla: stop_asking_intermed(Input).
%Ejemplo: stop_asking_intermed([exclamation(no)]). true.
%Descripción: Describe si se debe dejar de preguntar al usuario por destinos intermedios. Esta condición se da solo si una exclamación negativa forma parte de la respuesta del usuario (la cual se encuentra descompuesta en Input).
stop_asking_intermed(Input) :- 
	contains_term(exclamation(no), Input).

%Regla: 
%Ejemplo:
%Descripción:
answer(Input, Lista, ok, Stops) :-
	key_nominal(Input, nominal(Lugar, _, _)),
	city(Lugar, _),
	!,
	intermed([Lugar | Lista], Stops).
answer(Input, Lista, ok, Stops) :-
	key_nominal(Input, nominal(Lugar, _, Lugar_orig)),
	not(city(Lugar, _)),
	!,
	intermed_extra(Lista, Lugar_orig, Stops),
	!.
answer(Input, Lista, ok, Lista) :-
	not(key_nominal(Input, _)),
	stop_asking_intermed(Input),
	!.
answer(_, Lista, _, Stops) :-
	wazelog_writeln("Perdon, no he podido entenderle, repito mi pregunta."),
	intermed(Lista, Stops).
