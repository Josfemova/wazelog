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
arco(cartago,taras,3,5).
arco(cartago,paraiso,5,10).
arco(taras, tresrios,3,5).
arco(sanpedro,tresrios,10,25).
arco(sanpedro,zapote,2,5).
arco(sanpedro, curridabat,2,10).
arco(zapote,sanjose,3,5).
arco(sanpedro,sanjose,3,5).
arco(curridabat,sanjose,3,5).

conectados(C1,C2,Dist,Time):-arco(C2,C1,Dist,Time).
conectados(C2,C1,Dist,Time):-arco(C2,C1,Dist,Time).


%suponiendo que se construye la ruta como [peso|ruta]
saludo:-writeln("Bienvenido a WazeLog, la mejor logica de llegar a su destino"),
	writeln("Por favor indiqueme donde se encuentra"),!.
preg_destino:-writeln("Perfecto, cual es su destino?").
despedida:-writeln("Muchas gracias por utilizar Wazelog!").
preg_intermedio(1):-writeln("Genial, Algun destino intermedio?"),!.
preg_intermedio(_):-writeln("Algun otro destino intermedio?").
preg_direccion(Lugar):-format("Donde se encuentra ~w?\n", [Lugar]).
preg_cual(Place):-format("Cual ~w?\n", [Place]).
read_user_input(Descomp,Test):-current_input(Stdin),
	read_string(Stdin, "\n","\r\t",_,Text), 
	parse_user_input(Text, Descomp, Test).

start(Src, Dest, Paradas):-!,
	saludo,read_user_input(SrcRaw,Test),n(SrcRaw, Src,_),
	preg_destino, read_user_input(DestRaw,Test),n(DestRaw,Dest,_),
	intermed(Active,Paradas,1).%falta calculo de rutas aca


intermed(Src, Lista, It):-
	preg_intermedio(It),read_user_input(Input, Test),
	resultadoRespuesta(Input, Lista, It, Test).

intermed_extra(Src, Lista,It,PlaceType):-
	preg_cual(PlaceType), 
	read_user_input(Input, Test), n(Input, _,Place),
	preg_direccion(Place),
	read_user_input(Input2, Test2),
	resultadoRespuesta(Input2, Lista, It, Test2).

resultadoRespuesta(Src, Lista, It, Test):-Test = ok, n(Src, Lugar, _),
	ciudad(Lugar),list_push(Lugar, Lista, L2),Itx is It+1, intermed(Src, L2, Itx).
resultadoRespuesta(Src, Lista, It, Test):-Test = ok, n(Src, Lugar, Lugar_orig),
	not(ciudad(Lugar)), intermed_extra(Src, Lista, It, Lugar_orig).
resultadoRespuesta(Src, Lista, It, Test):-Test = ok,!.
resultadoRespuesta(Src, Lista, It, Test):-Test \=ok, 
	writeln("Perdon, no he podido entenderle, repito mi pregunta."),
	intermed(Src, Lista, It).
