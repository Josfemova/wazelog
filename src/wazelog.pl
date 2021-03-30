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


%ciudad('lugar').
ciudad('san jose').
ciudad('cartago').
ciudad('san pedro').
ciudad('tres rios').
ciudad('zapote').
ciudad('taras').
ciudad('paraíso').
ciudad('quircot').
ciudad('paseo colon').
ciudad('desamparados').
ciudad('guadalupe').

%arco(ciudad1, ciudad2, distancia, tiempo)
%El grafo es no dirigido
arco('cartago','taras',3,5).
arco('cartago','paraiso',5,10).
arco('taras', 'tres rios',3,5).
arco('san pedro','tres rios',10,25).
arco('san pedro','zapote',2,5).
arco('san pedro', 'curridabat',2,10).
arco('zapote','san jose',3,5).
arco('san pedro','san jose',3,5).
arco('curridabat','san jose',3,5).

conectados(C1,C2,Dist,Time):-arco(C2,C1,Dist,Time).
conectados(C2,C1,Dist,Time):-arco(C2,C1,Dist,Time).

camino(RutaDada, )



%suponiendo que se construye la ruta como [peso|ruta]
saludo:-writeln("Bienvenido a WazeLog, la mejor lógica de llegar a su destino"),
	writeln("Por favor indíqueme donde se encuentra").
preg_destino:-write("Genial, cuál es su destino?").
despedida:-writeln("Muchas gracias por utilizar Wazelog!").
preg_intermedio(1):-write("Algún destino intermedio?"),!.
preg_intermedio(_):-write("Algún otro destino intermedio?").
preg_direccion(Lugar):-format("Dónde se encuentra ~w?", [Lugar]).


