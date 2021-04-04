:- module(wazelog, [start/1]).
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

%Regla: read_user_input(Result)
%Ejemplo: read_user_input(R).
%			@usuario: yo voy a la sabana.
%		R = ok([svo(nominal(yo, "yo", "yo"), verbal([voy]), nominal(sabana, "la sabana", "sabana"))]).
%Descripción: Una entrada de un usuario se puede descomponer en una estructura definida por la gramática libre de contexto, la cual se compone por sujeto, verbo y objeto-complemento. Si esta descomposición es exitosa, `R` será `ok(Descomp)` donde `Descomp` es esta descomposición. `R` es `bye` si el usuario detiene la entrada, y `fail(T)` si hay un fallo sintáctico, donde `T` es un token.
read_user_input(Result) :-
	write("@Usuario: "),
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

translate([], S, SRes) :-
	atomics_to_string(S, ", ", SRes),
	!.
translate([City | Path], S, SRes) :-
	city(City, Trad),
	translate(Path, [Trad | S], SRes).

start(Then) :-
	run(start, _, Then).

run(bye, _, stop) :-
	writeln("Muchas Gracias por utilizar WazeLog!").
run(start, _, Then) :-
	ask_in_loop(ask_city(ask_src), Src),
	run(Src, start, Then).
run(city(Src), _, Then) :-
	ask_in_loop(ask_city(ask_dest), Dest),
	run(Dest, src(Src), Then).
run(city(Dest), src(Src), Then) :-
	intermed([], Paradas),
	run(Paradas, src_dest(Src, Dest), Then).
run(stops(Paradas), src_dest(Src, Dest), continue) :-
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
	run(bye, _, _),
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

ask_src(first, "Bienvenido a WazeLog, la mejor logica de llegar a su destino, por favor indiqueme donde se encuentra.").
ask_src(again, "Creo que hay un malentendido, por favor, me puede repetir, cual es su ubicacion actual?").

ask_dest(first, "Perfecto, cual es su destino?").
ask_dest(again, "Mis disculpas, no le he entendido, puede reformular su respuesta? A donde se dirige?").

ask_city(Prompter, Out, repeat(Repeat, done)) :-
	call(Prompter, Repeat, Prompt),
	wazelog_writeln(Prompt),
	read_user_input(Input),
	(
		Input = bye,
		Out = bye;

		Input = ok(CityRaw),
		key_nominal(CityRaw, nominal(City, _, _)),
		city(City, _),
		Out = city(City)
	).

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
	read_user_input(Input),
	answer(Input, Lista, Stops).

intermed_extra(Lista, PlaceType, Stops) :-
	q_which(PlaceType), 
	read_user_input(ok(Input)),
	key_nominal(Input, nominal(_, Place, _)),
	q_direction(Place),
	read_user_input(Input2),
	answer(Input2, Lista, Stops).

%Regla: stop_asking_intermed(Input).
%Ejemplo: stop_asking_intermed([exclamation(no)]). true.
%Descripción: Describe si se debe dejar de preguntar al usuario por destinos intermedios. Esta condición se da solo si una exclamación negativa forma parte de la respuesta del usuario (la cual se encuentra descompuesta en Input).
stop_asking_intermed(Input) :- 
	contains_term(exclamation(no), Input).

stop(Input) :- 
	contains_term(exclamation(adios), Input).

%Regla: 
%Ejemplo:
%Descripción:
answer(ok(Input), Lista, Stops) :-
	key_nominal(Input, nominal(Lugar, _, _)),
	city(Lugar, _),
	!,
	intermed([Lugar | Lista], Stops).
answer(ok(Input), Lista, Stops) :-
	key_nominal(Input, nominal(Lugar, _, Lugar_orig)),
	not(city(Lugar, _)),
	!,
	intermed_extra(Lista, Lugar_orig, Stops),
	!.
answer(ok(Input), Lista, stops(Lista)) :-
	not(key_nominal(Input, _)),
	stop_asking_intermed(Input),
	!.
answer(_, Lista, Stops) :-
	wazelog_writeln("Perdon, no he podido entenderle, repito mi pregunta."),
	intermed(Lista, Stops).
