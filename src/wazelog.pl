:-use_module(library(readutil)).
:-use_module(nlp).
:-use_module(routes).
%Instituto Tecnologico de Costa Rica
%Area Academica de Ingenieria en Computadores
%Lenguajes, Compialdores e Interpretes
%Estudiantes
%		Jose Morales Vargas		2019024270
%		Alejandro Soto Chacon	2019008164
%Semestre I 2021

%reglas de utilidad --------------

%miembro
miembro(X,[X|_]).
miembro(X,[_|T]):-miembro(X,T).

%Hechos y deducciones para función inversa 
inversa(X,Y):-inversa(X,Y,[]).
%condición de paro, el acumulador es igual a la lista y se recorrió la lista entera
inversa([],Z,Z).
%Si el acumulador no es igual a Z y la lista no es vacía, toma Head, la concatena al acumulador y sigue buscando
inversa([H|T],Z,Acumulador) :- inversa(T,Z,[H|Acumulador]).

%Cláusulas para longitud 
longitud([],0). %la longitud de una lista vacia es 0
longitud([_|T],X):-longitud(T, Y), X is Y+1.
%se corta la lista hasta llegar a una lista vacia

%rutas en forma [inicio, destino, intermedios ]

%contains_all chequea que todos los elementos de una lista estén en otra
contains_all(_,[]).
contains_all(L1,[H1|T1]):-miembro(H1, L1), contains_all(L1, T1).

list_push(X,L,[X|L]).


%ciudad('lugar').
ciudad(sanjose).
ciudad(cartago).
ciudad(sanpedro).
ciudad(tresrios).
ciudad(zapote).
ciudad(taras).
ciudad(paraiso).
ciudad(quircot).
ciudad(paseocolon).
ciudad(desamparados).
ciudad(guadalupe).
ciudad(curridabat).
ciudad(sabana).
%arco(ciudad1, ciudad2, distancia, tiempo)
%El grafo es no dirigido
arco(cartago,taras,3,5,25).
arco(cartago,paraiso,5,10,25).
arco(taras, tresrios,3,5,25).
arco(sanpedro,tresrios,10,25,25).
arco(sanpedro,zapote,2,5,25).
arco(sanpedro, curridabat,2,10,25).
arco(sanpedro,sanjose,3,5,25).
arco(sanjose,zapote,3,5,25).
arco(sanjose,curridabat,3,5,25).

conectados(C1,C2,Dist,Time):-arco(C2,C1,Dist,Time).
conectados(C2,C1,Dist,Time):-arco(C2,C1,Dist,Time).


%suponiendo que se construye la ruta como [peso|ruta]
saludo(1):-writeln("Bienvenido a WazeLog, la mejor logica de llegar a su destino"),
	writeln("Por favor indiqueme donde se encuentra").
saludo(_):-writeln("Creo que hay un malentendido, por favor, me puede repetir, cual es su ubicacion actual?").
preg_destino(1):-writeln("Perfecto, cual es su destino?").
preg_destino(_):-writeln("Mis disculpas, no le he entendido, puede reformular su respuesta? A donde se dirige?").
despedida:-writeln("Muchas gracias por utilizar Wazelog!").
preg_intermedio(1):-writeln("Genial, Algun destino intermedio?").
preg_intermedio(_):-writeln("Algun otro destino intermedio?").
preg_direccion(Lugar):-format("Donde se encuentra ~w?\n", [Lugar]).
preg_cual(Place):-format("Cual ~w?\n", [Place]).
read_user_input(Descomp,Test):-current_input(Stdin),
	read_string(Stdin, "\n","\r\t",_,Text), 
	parse_user_input(Text, Descomp, Test).

start(Src, Dest, Paradas):-
	ask_src(Src,1),ask_dest(Dest,1),intermed([],1,Paradas).%falta calculo de rutas ac

ask_src(Src, Cnt):-saludo(Cnt), read_user_input(SrcRaw, Test),!, valid_src(SrcRaw, Src, Test, Cnt).
valid_src(SrcRaw, Src, Test, _):- Test = ok, key_nominal(SrcRaw, nominal(Src,_,_)),ciudad(Src),!.
valid_src(SrcRaw, Src, Test, Cnt):- Test = ok, key_nominal(SrcRaw, nominal(BadSrc,_,_)),not(ciudad(BadSrc)),!,
	Cntx is Cnt+1,ask_src(Src, Cntx).
valid_src(SrcRaw, Src, Test, Cnt):- Test = ok,not(key_nominal(SrcRaw, _)),!, Cntx is Cnt+1, ask_src(Src, Cntx).
valid_src(_, Src, Test, Cnt):- Test \= ok,!,Cntx is Cnt+1, ask_src(Src, Cntx).

ask_dest(Dest, Cnt):-preg_destino(Cnt), read_user_input(DestRaw, Test),!, valid_src(DestRaw, Dest, Test, Cnt).
valid_dest(DestRaw, Dest, Test, _):- Test = ok, key_nominal(DestRaw, nominal(Dest,_,_)), ciudad(Dest),!.
valid_dest(DestRaw, Dest, Test, Cnt):- Test = ok, key_nominal(DestRaw, nominal(BadDest,_,_)), not(ciudad(BadDest)),!,
	Cntx is Cnt+1,ask_dest(Dest, Cntx).
valid_dest(DestRaw, Dest, Test, Cnt):- Test = ok, not(key_nomical(DestRaw,_)),!, Cntx is Cnt+1, ask_dest(Dest, Cntx).
valid_dest(_, Dest, Test, Cnt):- Test \= ok,!,Cntx is Cnt+1, ask_src(Dest, Cntx).

%lista debe comenzar []
intermed(Lista, Cnt, Stops):-
	preg_intermedio(Cnt),read_user_input(Input, Test),!,
	resultadoRespuesta(Input, Lista, Cnt, Test, Stops).

intermed_extra(Lista,Cnt,PlaceType, Stops):-
	preg_cual(PlaceType), 
	read_user_input(Input, _), key_nominal(Input, nominal(_, Place, _)),
	preg_direccion(Place),
	read_user_input(Input2, Test2),
	resultadoRespuesta(Input2, Lista, Cnt, Test2, Stops).

stop_asking_intermed(Input):-miembro(exclamation(no), Input).
stop_asking_intermed(Input):-miembro(exclamation(no,_), Input).

resultadoRespuesta(Input, Lista, Cnt, Test, Stops):-Test = ok, key_nominal(Input, nominal(Lugar, _, _)),
	ciudad(Lugar),!,list_push(Lugar, Lista, NewList),Cntx is Cnt+1, intermed(NewList, Cntx, Stops).

resultadoRespuesta(Input, Lista, Cnt, Test, Stops):-Test = ok, key_nominal(Input, nominal(Lugar, _, Lugar_orig)),
	not(ciudad(Lugar)),!, intermed_extra(Lista, Cnt, Lugar_orig, Stops),!.

resultadoRespuesta(Input, Lista, _, Test, Stops):-Test = ok,not(key_nominal(Input,_)),stop_asking_intermed(Input),!,Stops=Lista.

resultadoRespuesta(Input, Lista, Cnt, Test, Stops):-Test = ok,not(key_nominal(Input,_)),not(stop_asking_intermed(Input)),!,
	writeln("Perdon, no he podido entenderle, repito mi pregunta."),
	intermed(Lista, Cnt, Stops).

resultadoRespuesta(_, Lista, Cnt, Test, Stops):-Test \=ok,!, 
	writeln("Perdon, no he podido entenderle, repito mi pregunta."),
	intermed(Lista, Cnt, Stops).


%. mi problema es que fallo al concatenar la puta lista