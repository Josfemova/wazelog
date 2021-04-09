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
%Descripci�n: Un mensaje de wazelog se compone por un string, el cual se imprime en pantalla utilizando el stream de salida por defecto.
wazelog_writeln(Msg) :-
	format("[Wazelog]:::| ~s :::|\n", [Msg]).

%Regla: spacing.
%Ejemplo: 
%?- spacing.
%=========================================================================================
%Descripci�n: Un espaciado es un agregado est�tico a la salida en en stream de salida por defecto conformado por una cadena de s�mbolos '='
spacing :-
	tty_size(_, Width),
	string_repeat("=", Width, Repeated),
	writeln(Repeated).

%Regla: string_repeat(Base, Times, Repeated).
%Ejemplo:
%?- string_repeat("=", 5, B).
%"=====".
%Descripci�n: `Repeated` es un par�metro de salida el cu�l toma el valor de la serie de caracteres especificados por `Base` repetidos `Times` veces. 
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
%Descripci�n: Una entrada de un usuario se puede descomponer en una estructura definida por la gram�tica libre de contexto, la cual se compone por sujeto, verbo y objeto-complemento. Si esta descomposici�n es exitosa, `R` ser� `ok(Descomp)` donde `Descomp` es esta descomposici�n. `R` es `bye` si el usuario detiene la entrada, y `fail(T)` si hay un fallo sint�ctico, donde `T` es un token.
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

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
translate([], S, SRes) :-
	atomics_to_string(S, ", ", SRes),
	!.
translate([City | Path], S, SRes) :-
	city(City, Trad),
	translate(Path, [Trad | S], SRes).

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
start(Then) :-
	run(start, _, Then),
	!.

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
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
		reverse(Ruta, RutaInv),
		translate(RutaInv, [], StrPath),
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

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
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

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
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

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
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

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
last_stops(first, []).
last_stops(stops(Stops), Stops).
last_stops(again(Iteration), Stops) :-
	last_stops(Iteration, Stops).

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
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
%Descripci�n: Describe si se debe dejar de preguntar al usuario por destinos intermedios. Esta condici�n se da solo si una exclamaci�n negativa forma parte de la respuesta del usuario (la cual se encuentra descompuesta en Input).
stop_asking_intermed(Input) :- 
	contains_term(exclamation(negative), Input).

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
stop(Input) :- 
	contains_term(exclamation(bye), Input).