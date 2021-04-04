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

%reglas de utilidad --------------

%Regla:
%Ejemplo:
%Descripción:
wazelog_writeln(Msg):-write("[Wazelog]:::| "), write(Msg), write(" :::| \n").

%Regla:
%Ejemplo:
%Descripción:
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

%Regla:
%Ejemplo:
%Descripción:
farewell :-
	wazelog_writeln("Muchas gracias por utilizar Wazelog!").

%Regla:
%Ejemplo:
%Descripción:
q_direction(Lugar) :-
	format(atom(Msg), "Donde se encuentra ~w?", [Lugar]),
	wazelog_writeln(Msg).

q_which(Place) :-
	format(atom(Msg), "Cual ~w?", [Place]),
	wazelog_writeln(Msg).

%Regla:
%Ejemplo:
%Descripción:
read_user_input(Descomp, Test) :-
	write("@Usuario: "),
	current_input(Stdin),
	read_string(Stdin, "\n", "\r\t", _, Text), 
	parse_user_input(Text, Descomp, Test).

%Regla:
%Ejemplo:
%Descripción:
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

%Regla:
%Ejemplo:
%Descripción:
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

%Regla:
%Ejemplo:
%Descripción:
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

%Regla:
%Ejemplo:
%Descripción:
intermed_extra(Lista, PlaceType, Stops) :-
	q_which(PlaceType), 
	read_user_input(Input, _),
	key_nominal(Input, nominal(_, Place, _)),
	q_direction(Place),
	read_user_input(Input2, Test2),
	answer(Input2, Lista, Test2, Stops).

%Regla:
%Ejemplo:
%Descripción:
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
