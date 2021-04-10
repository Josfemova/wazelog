:- module(lang, [sentence_sep/1, exclamation/2, verbal/1, before_nominal/1,
                 contraction/2, unclassified/1, place_type/1, set_lang/1,
				 q_src/2, q_dest/2, q_direction/2, q_which/2, q_stops/2,
				 farewell/1, user_title/1, display_path/3, display_no_route/3]).

%Hechos: supported_lang(lenguaje).
%Ejemplo:
%  ?- supported_lang(es).
%  true.
%Descripcion: Enumera los idiomas soportados por la aplicacion.
supported_lang(es).
supported_lang(en).

%Hecho dinamico: lang(lenguaje).
% Ejemplo:
%   ?- lang(L), farewell(L, F).
%   F = "Muchas gracias por utilizar WazeLog!".
%   ?- set_lang(en), lang(L), farewell(L, F).
%   F = "Thank you for using WazeLog!".
%Descripcion: `lang/1` es un predicado dinamico, es decir, cuyos hechos pueden
%ser alterados en tiempo de ejecucion.  Esto permite, de manera conveniente,
%escoger un idioma sin tener que acarrear una variable de lenguaje por toda la
%aplicacion. El idioma se altera con `set_lang/1`.
plang(es).
:- dynamic lang/1.

%Regla: set_lang(lenguaje)
%Ejemplo:
%  ?- set_lang(es).
%  true.
%Descripcion: Cambia el idioma en uso. El idioma ingresado
%debe ser soportado segun reporte `supported_lang/1`.
set_lang(Lang) :-
	supported_lang(Lang),
	retractall(lang(_)),
	asserta(lang(Lang)).

%Hecho: sentence_sep(token).
%Ejemplo:
%  ?- sentence_sep('.').
%  true.
%Descripcion: Enuncia los tokens que separan oraciones.
sentence_sep('.').
sentence_sep(',').
sentence_sep(';').
sentence_sep(':').
sentence_sep('!').

%Regla: place_type(lugar).
%Ejemplo:
%  ?- place_type(hospital).
%  true.
%  ?- place_type(automercado).
%  false.
%Descripcion: Enuncia las formas nominales que se conoce
%             que se refieren siempre a tipos de lugares
%             en vez de lugares especificos. Se cumple si
%             existe un hecho correspondiente para el
%             idioma en uso.
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

%Regla: exclamation(token, tipo).
%Ejemplo:
%  ?- exclamation(hola, T).
%  T = greeting.
%Descripcion: Relaciona palabras clave de oraciones
% consideradas como exclamativas con el tipo interno
% de exclamacion. Al mismo tiempo, esta regla identifica
% el hecho de que tales palabres clave conformen este
% tipo de oraciones.
exclamation(Exclamation, Type) :-
	lang(Lang),
	exclamation(Lang, Exclamation, Type).
exclamation(es, si, affirmative).
exclamation(es, no, negative).
exclamation(es, hola, greeting).
exclamation(es, adios, bye).
exclamation(es, chao, bye).
exclamation(es, gracias, misc).
exclamation(en, yes, affirmative).
exclamation(en, no, negative).
exclamation(en, hi, greeting).
exclamation(en, hello, greeting).
exclamation(en, bye, bye).
exclamation(en, goodbye, bye).
exclamation(en, thank, misc).
exclamation(en, thanks, misc).

%Regla: verbal(token).
%Ejemplo:
%  ?- verbal(estoy).
%  true.
%Descripcion: Tiene exito si el token en cuestion
%es un componente de forma verbal. Es decir, esta
%regla identifica a los verbos.
verbal(Verb) :-
	lang(Lang),
	verbal(Lang, Verb).
verbal(es, esta).
verbal(es, estoy).
verbal(es, encuentro).
verbal(es, encuentra).
verbal(es, voy).
verbal(es, necesito).
verbal(es, requiero).
verbal(es, ir).
verbal(es, es).
verbal(es, tengo).
verbal(es, llegar).
verbal(es, pasar).
verbal(es, ubica).
verbal(es, gustaria).
verbal(es, quisiera).
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

%Regla: before_nominal(token).
%Ejemplo:
%  ?- before_nominal(el).
%  true.
%Descripcion: Enumera palabras de relleno
%que se espera seran seguidas inmediatamente
%por una forma nominal. Esto incluye en el
%caso del espaniol a los articulos y algunas
%preposiciones. Esto permite incluir estas
%palabras en la forma articulada original del
%termino nominal sin afectar tener que afectar
%su atomo identificante con respecto a no haber
%escrito la palabra antecesora.
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

%Regla: contraction(token, Expansion).
%Ejemplo:
%  ?- contraction(al, C).
%  C = [a, el].
%Descripcion: Expande contracciones en formas equivalentes.
contraction(Word, Expanded) :-
	lang(Lang),
	contraction(Lang, Word, Expanded).
contraction(es, al, [a, el]).
contraction(es, del, [de, el]).
contraction(en, 'i\'m', [i, am]).

%Regla: unclassified(token).
%Ejemplo:
%  ?- unclassified(que).
%  true.
%Descripcion: Declara que una palabra es explicitamente no clasificada
%y por tanto debe considerarse como de relleno (ver `filler/1`) en vez
%de como nominal, ya que lo ultimo se asume por defecto.
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

%Regla: q_src(iteracion, Prompt).
%Ejemplo:
%   ?- q_src(first, Prompt).
%   Prompt = "Bienvenido a WazeLog, la mejor logica de llegar a su destino. Por favor, in    diqueme donde se encuentra.".
%Descripcion: Define la pregunta de origen. Esta pregunta
%puede cambiar si se ingresa una respuesta incorrecta la
%primera vez, lo cual se expresa en el parametro de iteracion.
q_src(Iteration, Prompt) :-
	lang(Lang),
	q_src(Lang, Iteration, Prompt).
q_src(es, first, "Bienvenido a WazeLog, la mejor logica de llegar a su destino. Por favor, indiqueme donde se encuentra.").
q_src(es, again(_), "Creo que hay un malentendido. Por favor, me puede repetir, cual es su ubicacion actual?").
q_src(en, first, "Welcome to WazeLog, the best logic to arrive at your destination. Where are you?").
q_src(en, again(_), "Sorry, I could not understand you. Where are you right now?").

%Regla: q_src(iteracion, Prompt).
%Ejemplo:
%  ?- q_dest(first, Prompt).
%  Prompt = "Perfecto, cual es su destino?".
%Descripcion: Define la pregunta de destino. Esta pregunta
%puede cambiar si se ingresa una respuesta incorrecta la
%primera vez, lo cual se expresa en el parametro de iteracion.
q_dest(Iteration, Prompt) :-
	lang(Lang),
	q_dest(Lang, Iteration, Prompt).
q_dest(es, first, "Perfecto, cual es su destino?").
q_dest(es, again(_), "Mis disculpas, no le he entendido. Puede reformular su respuesta? A donde se dirige?").
q_dest(en, first, "Got it, where are you going to?").
q_dest(en, again(_), "I am sorry, but I failed to understand you. What is your destination?").

%Regla: q_direction(lugar, Prompt).
%Ejemplo:
%  ?- q_direction("AutoMercado").
%  Prompt = "Donde se encuentra AutoMercado?".
%Descripcion: Define la pregunta de ubicacion de un lugar.
q_direction(Place, Prompt) :-
	lang(Lang),
	q_direction(Lang, Place, Prompt).
q_direction(es, Place, Prompt) :-
	format(string(Prompt), "Donde se encuentra ~w?", [Place]).
q_direction(en, Place, Prompt) :-
	format(string(Prompt), "Where is ~w located?", [Place]).

%Regla: q_which(lugar, Prompt).
%Ejemplo:
%  ?- q_which("supermercado"). 
%  Prompt = "Cual supermercado?".
%Descripcion: Define la pregunta para especificar una
%ubicacion concreta a partir de un tipo de ubicacion.
q_which(Place, Prompt) :-
	lang(Lang),
	q_which(Lang, Place, Prompt).
q_which(es, Place, Prompt) :-
	format(string(Prompt), "Cual ~w?", [Place]).
q_which(en, Place, Prompt) :-
	format(string(Prompt), "Which ~w?", [Place]).

%Regla: q_stops(iteracion, Prompt).
%Ejemplo:
%  ?- q_stops(first, Prompt).
%  Prompt = "Genial, algun destino intermedio?".
%Descripcion: Define las preguntas que solicitan el
%primer y los demas destinos intermedios.
q_stops(Iteration, Prompt) :-
	lang(Lang),
	q_stops(Lang, Iteration, Prompt).
q_stops(es, first, "Genial, algun destino intermedio?").
q_stops(es, stops(_), "Algun otro destino intermedio?").
q_stops(es, again(first), "Perdon, no he podido entenderle. Desea un destino intermedio?").
q_stops(es, again(stops(_)), "Perdon, no he podido entenderle. Desea otro destino intermedio?").
q_stops(en, first, "Great, is there some stop in between?").
q_stops(en, stops(_), "Any other stop?").
q_stops(en, again(first), "Again, would you like to stop midway?").
q_stops(en, again(stops(_)), "Again, would you like to stop another time?").

%Regla: farewell(Salida).
%Ejemplo:
%  ?- farewell(O).
%  O = "Muchas gracias por utilizar WazeLog!".
%Descripcion: Define la despedida.
farewell(Farewell) :-
	lang(Lang),
	farewell(Lang, Farewell).
farewell(es, "Muchas gracias por utilizar WazeLog!").
farewell(en, "Thank you for using WazeLog!").

%Regla: user_title(Titulo).
%Ejemplo:
%  ?- user_title(T).
%  T = "Usuario".
%Descripcion: Define el titulo del usuario.
user_title(Title) :-
	lang(Lang),
	user_title(Lang, Title).
user_title(es, "Usuario").
user_title(en, "User").

%Regla: display_path(ruta, costo, Salida).
%Ejemplo:
%  ?- display_path("San Jose, Cartago", cost(1, 2, 3), T).
%  Salida = "Su ruta seria San Jose, Cartago. Longitud estimada de 1 Km. Duracion 2-3 min.".
%Descripcion: Define el mensaje de ruta y costos a partir de
%esta misma informacion.
display_path(Path, Cost, Text) :-
	lang(Lang),
	display_path(Lang, Path, Cost, Text).
display_path(es, Path, cost(Cost, Min, Max), Text) :-
	format(string(Text), "Su ruta seria ~s. Longitud estimada de ~d km. Duracion ~d-~d min.", [Path, Cost, Min, Max]).
display_path(en, Path, cost(Cost, Min, Max), Text) :-
	format(string(Text), "This is your ~d-km path: ~s. It will take ~d to ~d minutes.", [Cost, Path, Min, Max]).

%Regla: display_no_route(desde, hacia, Texto).
%Ejemplo:
%  ?- display_no_route("A", "B", T).
%  T = "No hay una ruta conocida de A a B.".
%Descripcion: Define el texto que se muestra cuando
%no existe una ruta entre un origen y un destino.
display_no_route(From, To, Text) :-
	lang(Lang),
	display_no_route(Lang, From, To, Text).
display_no_route(es, From, To, Text) :-
	format(string(Text), "No hay una ruta conocida de ~s a ~s.", [From, To]).
display_no_route(en, From, To, Text) :-
	format(string(Text), "There's no path from ~s to ~s.", [From, To]).
