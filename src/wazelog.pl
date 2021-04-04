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

%Regla: list_member(Elemento, Lista).
%Ejemplo: list_member(3,[1,4,2,3]).
%Descripción: Un elemento X es list_member de una lista T dado que dicho elemento sea el primer elemento de T, o de alguna de las sublistas de T.
list_member(X,[X|_]).
list_member(X,[_|T]):-list_member(X,T).

%Regla: inverse_list(Lista,ListaInversa).
%Ejemplo: inverse_list([1,2,3,4],Y). Y = [4,3,2,1].
%Descripción: Una lista X es inversa de una lista Y si se pueden tomar sus elementos de primero a ultimo, integrarlos a una nueva lista, y la misma es igual a la lista Y.
inverse_list(X,Y):-inverse_list(X,Y,[]).
inverse_list([],Z,Z).
inverse_list([H|T],Z,Acumulador) :- inverse_list(T,Z,[H|Acumulador]).

%Regla: longitud(Lista, Longitud).
%Ejemplo: longitud([2,3,4],L). L = 3. 
%Descripción: La longitud de una lista es la cantidad de elementos que se tengan que remover hasta llegar a una lista vacia.
longitud([],0). %la longitud de una lista vacia es 0.
longitud([_|T],X):-longitud(T, Y), X is Y+1.

%Regla:
%Ejemplo:
%Descripción:
list_push(X,L,[X|L]).

conectados(C1,C2,Dist,Time):-arco(C2,C1,Dist,Time).
conectados(C2,C1,Dist,Time):-arco(C2,C1,Dist,Time).

%Regla:
%Ejemplo:
%Descripción:
wazelog_writeln(Msg):-write("[Wazelog]:::| "), write(Msg), write(" :::| \n").
%Regla:
%Ejemplo:
%Descripción:
spacing:-writeln("====================================================================================================").
%Regla:
%Ejemplo:
%Descripción:
greeting(1):-wazelog_writeln("Bienvenido a WazeLog, la mejor logica de llegar a su destino, por favor indiqueme donde se encuentra.").
greeting(_):-wazelog_writeln("Creo que hay un malentendido, por favor, me puede repetir, cual es su ubicacion actual?").

%Regla:
%Ejemplo:
%Descripción:
q_dest(1):-wazelog_writeln("Perfecto, cual es su destino?").
q_dest(_):-wazelog_writeln("Mis disculpas, no le he entendido, puede reformular su respuesta? A donde se dirige?").

%Regla:
%Ejemplo:
%Descripción:
farewell:-wazelog_writeln("Muchas gracias por utilizar Wazelog!").

%Regla:
%Ejemplo:
%Descripción:
q_intermed(1):-wazelog_writeln("Genial, Algun destino intermedio?").
q_intermed(_):-wazelog_writeln("Algun otro destino intermedio?").

%Regla:
%Ejemplo:
%Descripción:
q_direction(Lugar):-format(atom(Msg),"Donde se encuentra ~w?", [Lugar]),wazelog_writeln(Msg).
q_which(Place):-format(atom(Msg),"Cual ~w?", [Place]),wazelog_writeln(Msg).

%Regla:
%Ejemplo:
%Descripción:
read_user_input(Descomp,Test):-write("@Usuario: "),current_input(Stdin),
	read_string(Stdin, "\n","\r\t",_,Text), 
	parse_user_input(Text, Descomp, Test).

%Regla:
%Ejemplo:
%Descripción:
translate([],S,SRes):-atomics_to_string(S, ", ", SRes).
translate([City|Path], S, SRes):- city(City,Trad), translate(Path, [Trad|S], SRes).

%start(Src, Dest, Paradas):-
start:-
	ask_src(Src,1),ask_dest(Dest,1),intermed([],1,Paradas),
	shortest_path_through(Src, Paradas,Dest, Ruta,Peso),inverse_list(Ruta, RutaInv),translate(RutaInv, [], StrPath), %falta hacer el el pathfinder sea no dirigido
	spacing,format("Su ruta seria ~w. Longitud estimada de ~d Km.\nMuchas Gracias por utilizar WazeLog!\n", [StrPath, Peso]),
	spacing.

%Regla:
%Ejemplo:
%Descripción:
ask_src(Src, Cnt):-greeting(Cnt),!, read_user_input(SrcRaw, Test), valid_src(SrcRaw, Src, Test, Cnt).
valid_src(SrcRaw, Src, Test, _):- Test = ok, key_nominal(SrcRaw, nominal(Src,_,_)),city(Src, _),!.
%Regla:
%Ejemplo:
%Descripción:
valid_src(SrcRaw, Src, Test, Cnt):- Test = ok, key_nominal(SrcRaw, nominal(BadSrc,_,_)),not(city(BadSrc, _)),!,
	Cntx is Cnt+1,ask_src(Src, Cntx).
valid_src(SrcRaw, Src, Test, Cnt):- Test = ok,not(key_nominal(SrcRaw, _)),!, Cntx is Cnt+1, ask_src(Src, Cntx).
valid_src(_, Src, Test, Cnt):- Test \= ok,!,Cntx is Cnt+1, ask_src(Src, Cntx).

%Regla:
%Ejemplo:
%Descripción:
ask_dest(Dest, Cnt):-q_dest(Cnt), read_user_input(DestRaw, Test),!, valid_src(DestRaw, Dest, Test, Cnt).
valid_dest(DestRaw, Dest, Test, _):- Test = ok, key_nominal(DestRaw, nominal(Dest,_,_)), city(Dest, _),!.
%Regla:
%Ejemplo:
%Descripción:
valid_dest(DestRaw, Dest, Test, Cnt):- Test = ok, key_nominal(DestRaw, nominal(BadDest,_,_)), not(city(BadDest, _)),!,
	Cntx is Cnt+1,ask_dest(Dest, Cntx).
valid_dest(DestRaw, Dest, Test, Cnt):- Test = ok, not(key_nomical(DestRaw,_)),!, Cntx is Cnt+1, ask_dest(Dest, Cntx).
valid_dest(_, Dest, Test, Cnt):- Test \= ok,!,Cntx is Cnt+1, ask_dest(Dest, Cntx).

%lista debe comenzar []
%Regla:
%Ejemplo:
%Descripción:
intermed(Lista, Cnt, Stops):-
	q_intermed(Cnt),read_user_input(Input, Test),!,
	answer(Input, Lista, Cnt, Test, Stops).

%Regla:
%Ejemplo:
%Descripción:
intermed_extra(Lista,Cnt,PlaceType, Stops):-
	q_which(PlaceType), 
	read_user_input(Input, _), key_nominal(Input, nominal(_, Place, _)),
	q_direction(Place),
	read_user_input(Input2, Test2),
	answer(Input2, Lista, Cnt, Test2, Stops).

%Regla:
%Ejemplo:
%Descripción:
stop_asking_intermed(Input):-list_member(exclamation(no), Input).
stop_asking_intermed(Input):-list_member(exclamation(no,_), Input).

%Regla: 
%Ejemplo:
%Descripción:
answer(Input, Lista, Cnt, Test, Stops):-Test = ok, key_nominal(Input, nominal(Lugar, _, _)),
	city(Lugar, _),!,list_push(Lugar, Lista, NewList),Cntx is Cnt+1, intermed(NewList, Cntx, Stops).

answer(Input, Lista, Cnt, Test, Stops):-Test = ok, key_nominal(Input, nominal(Lugar, _, Lugar_orig)),
	not(city(Lugar, _)),!, intermed_extra(Lista, Cnt, Lugar_orig, Stops),!.

answer(Input, Lista, _, Test, Stops):-Test = ok,not(key_nominal(Input,_)),stop_asking_intermed(Input),!,Stops=Lista.

answer(Input, Lista, Cnt, Test, Stops):-Test = ok,not(key_nominal(Input,_)),not(stop_asking_intermed(Input)),!,
	wazelog_writeln("Perdon, no he podido entenderle, repito mi pregunta."),
	intermed(Lista, Cnt, Stops).

answer(_, Lista, Cnt, Test, Stops):-Test \=ok,!, 
	wazelog_writeln("Perdon, no he podido entenderle, repito mi pregunta."),
	intermed(Lista, Cnt, Stops).


