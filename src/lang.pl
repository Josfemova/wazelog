:- module(lang, [sentence_sep/1, exclamation/2, verbal/1, before_nominal/1,
                 contraction/2, unclassified/1, place_type/1, find_language/0,
				 q_src/2, q_dest/2, q_direction/2, q_which/2, q_stops/2,
				 farewell/1, user_title/1, display_path/3, display_no_route/3]).

lang(es).
:- dynamic lang/1.

find_language :-
	retractall(lang(_)),
	asserta(lang(es)).

%Regla: 
%Ejemplo:
%Descripción:
sentence_sep('.').
sentence_sep(',').
sentence_sep(';').
sentence_sep(':').

%Regla: 
%Ejemplo:
%Descripción:
place_type(PlaceType) :-
	lang(Lang),
	place_type(Lang, PlaceType).
place_type(es, supermercado).
place_type(es, mercado).
place_type(es, tienda).
place_type(es, banco).
place_type(es, hospital).
place_type(es, escuela).
place_type(es, restaurante).
place_type(es, parque).
place_type(es, instituto).

%Regla: 
%Ejemplo:
%Descripción:
exclamation(Exclamation, Type) :-
	lang(Lang),
	exclamation(Lang, Exclamation, Type).
exclamation(es, si, affirmative).
exclamation(es, no, negative).
exclamation(es, hola, greeting).
exclamation(es, adios, bye).
exclamation(es, gracias, misc).

%Regla: 
%Ejemplo:
%Descripción:
verbal(Verb) :-
	lang(Lang),
	verbal(Lang, Verb).
verbal(es, esta).
verbal(es, estoy).
verbal(es, encuentro).
verbal(es, encuentra).
verbal(es, voy).
verbal(es, necesito).
verbal(es, ir).
verbal(es, es).
verbal(es, llegar).
verbal(es, pasar).
verbal(es, ubica).
verbal(es, gustaria).

%Regla: 
%Ejemplo:
%Descripción:
before_nominal(Word) :-
	lang(Lang),
	before_nominal(Lang, Word).
before_nominal(es, el).
before_nominal(es, los).
before_nominal(es, la).
before_nominal(es, las).
before_nominal(es, de).

%Regla: 
%Ejemplo:
%Descripción:
contraction(Word, Expanded) :-
	lang(Lang),
	contraction(Lang, Word, Expanded).
contraction(es, al, [a, el]).
contraction(es, del, [de, el]).

%Regla: 
%Ejemplo:
%Descripción:
unclassified(Word) :-
	lang(Lang),
	unclassified(Lang, Word).
unclassified(es, me).
unclassified(es, que).
unclassified(es, a).
unclassified(es, se).
unclassified(es, en).
unclassified(es, de).
unclassified(es, un).
unclassified(es, una).
unclassified(es, tengo).
unclassified(es, por).
unclassified(es, muchas).

q_src(Iteration, Prompt) :-
	lang(Lang),
	q_src(Lang, Iteration, Prompt).
q_src(es, first, "Bienvenido a WazeLog, la mejor logica de llegar a su destino, por favor indiqueme donde se encuentra.").
q_src(es, again(_), "Creo que hay un malentendido, por favor, me puede repetir, cual es su ubicacion actual?").

q_dest(Iteration, Prompt) :-
	lang(Lang),
	q_dest(Lang, Iteration, Prompt).
q_dest(es, first, "Perfecto, cual es su destino?").
q_dest(es, again(_), "Mis disculpas, no le he entendido, puede reformular su respuesta? A donde se dirige?").

%Regla: q_direction(Place).
%Ejemplo: q_direction("AutoMercado"). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Dónde se encuentra AutoMercado?  :::|
%Descripción: Utiliza la regla de wazelog_writeln para comunicarle al usuario una pregunta sobre la localización de un lugar destino intermedio. 
q_direction(Place, Prompt) :-
	lang(Lang),
	q_direction(Lang, Place, Prompt).
q_direction(es, Place, Prompt) :-
	format(string(Prompt), "Donde se encuentra ~w?", [Place]).

%Ejemplo: q_which("supermercado"). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Cuál supermercado?  :::|
%Descripción: Utiliza la regla de wazelog_writeln para comunicar al usuario una pregunta para que se especifique precisamente cual lugar de tipo Place es el que quiere tomar como destino intermedio. 
q_which(Place, Prompt) :-
	lang(Lang),
	q_which(Lang, Place, Prompt).
q_which(es, Place, Prompt) :-
	format(string(Prompt), "Cual ~w?", [Place]).

q_stops(Iteration, Prompt) :-
	lang(Lang),
	q_stops(Lang, Iteration, Prompt).
q_stops(es, first, "Genial, Algun destino intermedio?").
q_stops(es, stops(_), "Algun otro destino intermedio?").
q_stops(es, again(first), "Perdon, no he podido entenderle, desea un destino intermedio?").
q_stops(es, again(stops(_)), "Perdon, no he podido entenderle, desea otro destino intermedio?").

farewell(Farewell) :-
	lang(Lang),
	farewell(Lang, Farewell).
farewell(es, "Muchas Gracias por utilizar WazeLog!").

user_title(Title) :-
	lang(Lang),
	user_title(Lang, Title).
user_title(es, "Usuario").

display_path(Path, Cost, Text) :-
	lang(Lang),
	display_path(Lang, Path, Cost, Text).
display_path(es, Path, cost(Cost, Min, Max), Text) :-
	format(string(Text), "Su ruta seria ~s. Longitud estimada de ~d Km. Duración ~d-~d min.", [Path, Cost, Min, Max]).

display_no_route(From, To, Text) :-
	lang(Lang),
	display_no_route(Lang, From, To, Text).
display_no_route(es, From, To, Text) :-
	format(string(Text), "No hay una ruta conocida entre ~s y ~s.", [From, To]).
