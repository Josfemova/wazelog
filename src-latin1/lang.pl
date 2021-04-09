:- module(lang, [sentence_sep/1, exclamation/2, verbal/1, before_nominal/1,
                 contraction/2, unclassified/1, place_type/1, set_lang/1,
				 q_src/2, q_dest/2, q_direction/2, q_which/2, q_stops/2,
				 farewell/1, user_title/1, display_path/3, display_no_route/3]).

%Hecho: supported_lang(lenguaje).
%Ejemplo:
%?- supported_lang(es).
%true.
%Descripci�n:
supported_lang(es).
supported_lang(en).

%Regla: 
%Ejemplo:
%?- 
%
%Descripci�n:
lang(es).
:- dynamic lang/1.

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
set_lang(Lang) :-
	supported_lang(Lang),
	retractall(lang(_)),
	asserta(lang(Lang)).

%Regla: 
%Ejemplo:
%?- 
%
%Descripci�n:
sentence_sep('.').
sentence_sep(',').
sentence_sep(';').
sentence_sep(':').
sentence_sep('�').
sentence_sep('!').

%Regla: 
%Ejemplo:
%Descripci�n:
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
place_type(en, supermercado).
place_type(en, market).
place_type(en, store).
place_type(en, bank).
place_type(en, hospital).
place_type(en, school).
place_type(en, restaurant).
place_type(en, park).
place_type(en, institute).

%Regla: 
%Ejemplo:
%?- 
%
%Descripci�n:
exclamation(Exclamation, Type) :-
	lang(Lang),
	exclamation(Lang, Exclamation, Type).
exclamation(es, si, affirmative).
exclamation(es, no, negative).
exclamation(es, hola, greeting).
exclamation(es, adios, bye).
exclamation(es, gracias, misc).
exclamation(en, yes, affirmative).
exclamation(en, no, negative).
exclamation(en, hi, greeting).
exclamation(en, hello, greeting).
exclamation(en, bye, bye).
exclamation(en, goodbye, bye).
exclamation(en, thank, misc).
exclamation(en, thanks, misc).

%Regla: 
%Ejemplo:
%?- 
%
%Descripci�n:
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
verbal(es, tengo).
verbal(es, llegar).
verbal(es, pasar).
verbal(es, ubica).
verbal(es, gustaria).
verbal(en, is).
verbal(en, am).
verbal(en, have).
verbal(en, go).
verbal(en, going).
verbal(en, need).
verbal(en, needs).
verbal(en, arrive).
verbal(en, located).
verbal(en, like).

%Regla: 
%Ejemplo:
%?- 
%
%Descripci�n:
before_nominal(Word) :-
	lang(Lang),
	before_nominal(Lang, Word).
before_nominal(es, el).
before_nominal(es, los).
before_nominal(es, la).
before_nominal(es, las).
before_nominal(es, de).
before_nominal(en, the).
before_nominal(en, of).

%Regla: 
%Ejemplo:
%?- 
%
%Descripci�n:
contraction(Word, Expanded) :-
	lang(Lang),
	contraction(Lang, Word, Expanded).
contraction(es, al, [a, el]).
contraction(es, del, [de, el]).
contraction(en, 'i\'m', [i, am]).

%Regla: 
%Ejemplo:
%?- 
%
%Descripci�n:
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
unclassified(es, por).
unclassified(es, muchas).
unclassified(en, a).
unclassified(en, an).
unclassified(en, at).
unclassified(en, in).
unclassified(en, to).
unclassified(en, by).
unclassified(en, many).
unclassified(en, lot).
unclassified(en, lots).

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
q_src(Iteration, Prompt) :-
	lang(Lang),
	q_src(Lang, Iteration, Prompt).
q_src(es, first, "Bienvenido a WazeLog, la mejor l�gica de llegar a su destino. Por favor, ind�queme donde se encuentra.").
q_src(es, again(_), "Creo que hay un malentendido. Por favor, me puede repetir, �cu�l es su ubicaci�n actual?").
q_src(en, first, "Welcome to WazeLog, the best logic to arrive at your destination. Where are you?").
q_src(en, again(_), "Sorry, I couldn't understand you. Where are you right now?").

q_dest(Iteration, Prompt) :-
	lang(Lang),
	q_dest(Lang, Iteration, Prompt).
q_dest(es, first, "Perfecto, �cu�l es su destino?").
q_dest(es, again(_), "Mis disculpas, no le he entendido. �Puede reformular su respuesta? �A d�nde se dirige?").
q_dest(en, first, "Got it, where are you going to?").
q_dest(en, again(_), "I'm sorry, but I failed to understand you. What's your destination?").

%Regla: q_direction(Place).
%Ejemplo:
%q_direction("AutoMercado").
%[wazelog]:::| D�nde se encuentra AutoMercado?  :::|.
%Descripci�n: Utiliza la regla de wazelog_writeln para comunicarle al usuario una pregunta sobre la localizaci�n de un lugar destino intermedio. 
q_direction(Place, Prompt) :-
	lang(Lang),
	q_direction(Lang, Place, Prompt).
q_direction(es, Place, Prompt) :-
	format(string(Prompt), "�D�nde se encuentra ~w?", [Place]).
q_direction(en, Place, Prompt) :-
	format(string(Prompt), "Where is ~w located?", [Place]).

%Ejemplo: 
%q_which("supermercado"). 
%[wazelog]:::| Cu�l supermercado?  :::|
%Descripci�n: Utiliza la regla de wazelog_writeln para comunicar al usuario una pregunta para que se especifique precisamente cual lugar de tipo Place es el que quiere tomar como destino intermedio. 
q_which(Place, Prompt) :-
	lang(Lang),
	q_which(Lang, Place, Prompt).
q_which(es, Place, Prompt) :-
	format(string(Prompt), "�Cu�l ~w?", [Place]).
q_which(en, Place, Prompt) :-
	format(string(Prompt), "Which ~w?", [Place]).

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
q_stops(Iteration, Prompt) :-
	lang(Lang),
	q_stops(Lang, Iteration, Prompt).
q_stops(es, first, "Genial, �alg�n destino intermedio?").
q_stops(es, stops(_), "�Alg�n otro destino intermedio?").
q_stops(es, again(first), "Perd�n, no he podido entenderle. �Desea un destino intermedio?").
q_stops(es, again(stops(_)), "Perd�n, no he podido entenderle. �Desea otro destino intermedio?").
q_stops(en, first, "Great, is there some stop in between?").
q_stops(en, stops(_), "Any other stop?").
q_stops(en, again(first), "Again, would you like to stop midway?").
q_stops(en, again(stops(_)), "Again, would you like to stop another time?").

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
farewell(Farewell) :-
	lang(Lang),
	farewell(Lang, Farewell).
farewell(es, "�Muchas gracias por utilizar WazeLog!").
farewell(en, "Thanks for using WazeLog!").

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
user_title(Title) :-
	lang(Lang),
	user_title(Lang, Title).
user_title(es, "Usuario").
user_title(en, "User").

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
display_path(Path, Cost, Text) :-
	lang(Lang),
	display_path(Lang, Path, Cost, Text).
display_path(es, Path, cost(Cost, Min, Max), Text) :-
	format(string(Text), "Su ruta seria ~s. Longitud estimada de ~d Km. Duraci�n ~d-~d min.", [Path, Cost, Min, Max]).
display_path(en, Path, cost(Cost, Min, Max), Text) :-
	format(string(Text), "This is your ~d-km path: ~s. It will take ~d to ~d minutes.", [Cost, Path, Min, Max]).

%Regla:
%Ejemplo:
%?- 
%
%Descripci�n:
display_no_route(From, To, Text) :-
	lang(Lang),
	display_no_route(Lang, From, To, Text).
display_no_route(es, From, To, Text) :-
	format(string(Text), "No hay una ruta conocida de ~s a ~s.", [From, To]).
display_no_route(en, From, To, Text) :-
	format(string(Text), "There's no path from ~s to ~s.", [From, To]).
