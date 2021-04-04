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

%Regla: list_push(elemento, lista, [elemento|lista]
%Ejemplo: list_push(1, [1,2,3,4,5], A). A = [1,1,2,3,4,5].
%Descripción: Si se realiza una operación push de un elemento X sobre una lista L, el resultado de dicha operación debe ser una tecera lista de la cual es primer elemento X, y el resto de miembros son los anteriores miembros de la lista L
list_push(X,L,[X|L]).

%reglas y hechos de programa------------------------

%Hecho: ciudad(lugar).
%Ejemplo: ciudad(paseocolon) 
%Descripción: Una ciudad está definida por el nombre de un lugar, sin símbolos de acentos o espacios de por medio.
city(sanjose).
city(cartago).
city(sanpedro).
city(tresrios).
city(zapote).
city(taras).
city(paraiso).
city(quircot).
city(paseocolon).
city(desamparados).
city(guadalupe).
city(curridabat).
city(sabana).

%Regla:
%Ejemplo:
%Descripción:
city_name(sanjose, "San Jose").
city_name(cartago, "Cartago").
city_name(sanpedro, "San Pedro").
city_name(tresrios, "Tres Rios").
city_name(zapote, "Zapote").
city_name(taras, "Taras").
city_name(paraiso, "Paraiso").
city_name(quircot, "El Quircot").
city_name(paseocolon, "Paseo Colon").
city_name(desamparados, "Desamparados").
city_name(guadalupe, "Guadalupe").
city_name(curridabat, "Curridabat").
city_name(sabana, "La Sabana").
%arco(ciudad1, ciudad2, distancia, tiempo)
%El grafo es no dirigido
arco(cartago,taras,3,5,25).
arco(cartago,paraiso,5,10,25).
arco(taras, tresrios,3,5,25).
arco(tresrios, sanpedro, 10,25,25).
arco(sanpedro,tresrios,10,25,25).
arco(sanpedro,zapote,2,5,25).
arco(sanpedro, curridabat,2,10,25).
arco(sanpedro,sanjose,3,5,25).
arco(sanjose,zapote,3,5,25).
arco(sanjose,curridabat,3,5,25).

conectados(C1,C2,Dist,Time):-arco(C2,C1,Dist,Time).
conectados(C2,C1,Dist,Time):-arco(C2,C1,Dist,Time).

%Regla: wazelog_writeln(mensaje).
%Ejemplo: wazelog_writeln("Hola, gracias por usar wazelog") ->>(mensaje en StdOut)->>
%			[wazelog]:::| Hola, gracias por usar wazelog :::|
%Descripción: Un mensaje de wazelog se compone por un string, el cual se imprime en pantalla utilizando el stream de salida por defecto.
wazelog_writeln(Msg):-write("[Wazelog]:::| "), write(Msg), write(" :::| \n").

%Regla: spacing.
%Ejemplo: spacing ->>(salida en StdOut)->>
%			=========================================================================================
%Descripción: Un espaciado es un agregado estético a la salida en en stream de salida por defecto conformado por una cadena de símbolos '='
spacing:-writeln("====================================================================================================").

%Regla: greeting(#repeticiones).
%Ejemplo: greeting(2). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Creo que hay un malentendido, por favor, me puede repertir, cual es su ubicacion actual? :::|
%		  greeting(1). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Bienvenido a Wazelog, la mejor logica de llegar a su destino, por favor, indiqueme donde se encuentra :::|
%Descripción: Un saludo "greeting" está compuesto por una bienvenida al programa, y una pregunta sobre ubicación, pero si ya se ha dado antes esta información, el saludo está compuesto por un mensaje que informa malentendido, y repite la pregunta de ubicación
greeting(1):-wazelog_writeln("Bienvenido a WazeLog, la mejor logica de llegar a su destino, por favor indiqueme donde se encuentra.").
greeting(_):-wazelog_writeln("Creo que hay un malentendido, por favor, me puede repetir, cual es su ubicacion actual?").

%Regla: q_dest(#repeticiones)
%Ejemplo: q_dest(2). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Mis disculpas, no le he entendido, puede reformular su respuesta? A dónde se dirige?  :::|
%		  q_dest(1). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Perfecto, cuál es su destino? :::|
%Descripción: Una pregunta de destino inicialmente solo es la pregunta en sí, de lo contrario, 
q_dest(1):-wazelog_writeln("Perfecto, cual es su destino?").
q_dest(_):-wazelog_writeln("Mis disculpas, no le he entendido, puede reformular su respuesta? A donde se dirige?").

%Regla: farewell.
%Ejemplo: farewell. ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Muchas gracias por utilizar wazelog :::|
%Descripción: Una despedida se da al comunicar un agradecimiento por utilizar el programa
farewell:-wazelog_writeln("Muchas gracias por utilizar Wazelog!").

%Regla: q_intermed(#repeticiones)
%Ejemplo: q_intermed(2). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Algún otro destino intermedio?  :::|
%		  q_intermed(1). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Genial, Algún destino intermedio? :::|
%Descripción: Una pregunta de destino intermedio se conforma en primera vez que se hace por una exclamación, y la pregunta en sí sobre si hay und estino intermedio, sin embargo, en instancias posteriores, se agrega la palabra "otro" para indicar que ya hay un destino intermedio sinedo tomado en cuenta.
q_intermed(1):-wazelog_writeln("Genial, Algun destino intermedio?").
q_intermed(_):-wazelog_writeln("Algun otro destino intermedio?").

%Regla: q_direction(Place).
%Ejemplo: q_direction("AutoMercado"). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Dónde se encuentra AutoMercado?  :::|
%Descripción: Utiliza la regla de wazelog_writeln para comunicarle al usuario una pregunta sobre la localización de un lugar destino intermedio. 
q_direction(Place):-format(atom(Msg),"Donde se encuentra ~w?", [Place]),wazelog_writeln(Msg).

%Regla: q_which(Place).
%Ejemplo: q_which("supermercado"). ->>(mensaje por medio de wazelog_writeln)->>
%			[wazelog]:::| Cuál supermercado?  :::|
%Descripción: Utiliza la regla de wazelog_writeln para comunicar al usuario una pregunta para que se especifique precisamente cual lugar de tipo Place es el que quiere tomar como destino intermedio. 
q_which(Place):-format(atom(Msg),"Cual ~w?", [Place]),wazelog_writeln(Msg).

%Regla: read_user_input(Descomp, Test)
%Ejemplo: read_user_input(A, B).
%			@usuario: yo voy a cartago.
%		A = [svo(nominal(yo, "yo", "yo"), verbal([voy]), nominal(cartago, "cartago", "cartago"))],
%		B = ok.
%Descripción: Una entrada de un usuario se puede descomponer en una estructura definida por la gramática libre de contexto, la cual se compone por sujeto, verbo y objeto-complemento. Esta descomposición se denomina Descomp. La prueba de coherencia de dicha descomposición está dada por el argumento Test, el cual toma el valor "ok" si la oración es coherente, un valor de los términos conflictivos si no hay coherencia. 
read_user_input(Descomp,Test):-write("@Usuario: "),current_input(Stdin),
	read_string(Stdin, "\n","\r\t",_,Text), 
	parse_user_input(Text, Descomp, Test).

%Regla:
%Ejemplo:
%Descripción:
translate([],S,SRes):-atomics_to_string(S, ", ", SRes).
translate([City|Path], S, SRes):- city_name(City,Trad), translate(Path, [Trad|S], SRes).

%regla:start(Src, Dest, Paradas):
start:-
	ask_src(Src,1),ask_dest(Dest,1),intermed([],1,Paradas),
	shortest_path_through(Src, Paradas,Dest, Ruta,Peso),inverse_list(Ruta, RutaInv),translate(RutaInv, [], StrPath), %falta hacer el el pathfinder sea no dirigido
	spacing,format("Su ruta seria ~w. Longitud estimada de ~d Km.\nMuchas Gracias por utilizar WazeLog!\n", [StrPath, Peso]),
	spacing.

%Regla: ask_src(Src, Cnt).
%Ejemplo: ask_src(Src, 1). Dest = sanjose.
%Descripción: Pregunta por la ubicación del usuario, y termina evaluación hasta que una respuesta válida sea dada. Si no se da una ubicación válida, se vuelve a preguntar al usuario por una ubicación hasta terminar en una evaluación verdadera de la regla.
ask_src(Src, Cnt):-greeting(Cnt),!, read_user_input(SrcRaw, Test), valid_src(SrcRaw, Src, Test, Cnt).

%Regla: valid_src(SrcRaw, Src, Test, Cnt).
%Ejemplo: valid_src([nominal(sanjose, "sanjose", "San José")], Src, ok, 1). Src=sanjose. 
%Descripción: Una ubicación válida está dada por un átomo que es una ciudad registrada.
valid_src(SrcRaw, Src, Test, _):- Test = ok, key_nominal(SrcRaw, nominal(Src,_,_)),city(Src),!.
valid_src(SrcRaw, Src, Test, Cnt):- Test = ok, key_nominal(SrcRaw, nominal(BadSrc,_,_)),not(city(BadSrc)),!,
	Cntx is Cnt+1,ask_src(Src, Cntx).
valid_src(SrcRaw, Src, Test, Cnt):- Test = ok,not(key_nominal(SrcRaw, _)),!, Cntx is Cnt+1, ask_src(Src, Cntx).
valid_src(_, Src, Test, Cnt):- Test \= ok,!,Cntx is Cnt+1, ask_src(Src, Cntx).

%Regla: ask_dest(Dest, Cnt). 
%Ejemplo: ask_dest(Dest, 1). Dest = sanjose.
%Descripción: Pregunta por el destino del usuario, y termina evaluación hasta que una respuesta válida sea dada.
ask_dest(Dest, Cnt):-q_dest(Cnt), read_user_input(DestRaw, Test),!, valid_src(DestRaw, Dest, Test, Cnt).

%Regla: valid_dest(DestRaw, Dest, Test, Cnt).
%Ejemplo: valid_src([nominal(sanjose, "sanjose", "San José")], Dest, ok, 1). Dest=sanjose. 
%Descripción: Un destino válido está dado por un átomo que es una ciudad registrada. Si no se da un destino válido, se vuelve a preguntar al usuario por un destino hasta terminar en una evaluación verdadera de la regla.
valid_dest(DestRaw, Dest, Test, _):- Test = ok, key_nominal(DestRaw, nominal(Dest,_,_)), city(Dest),!.
valid_dest(DestRaw, Dest, Test, Cnt):- Test = ok, key_nominal(DestRaw, nominal(BadDest,_,_)), not(city(BadDest)),!,
	Cntx is Cnt+1,ask_dest(Dest, Cntx).
valid_dest(DestRaw, Dest, Test, Cnt):- Test = ok, not(key_nomical(DestRaw,_)),!, Cntx is Cnt+1, ask_dest(Dest, Cntx).
valid_dest(_, Dest, Test, Cnt):- Test \= ok,!,Cntx is Cnt+1, ask_dest(Dest, Cntx).

%lista debe comenzar []
%Regla: intermed(Lista, Cnt, Stops).
%Ejemplo: intermed([], 1, Stops). Stops = [cartago, sanjose]
%Descripción: Regla que funciona como punto de entrada para preguntarle al usuario por los destinos intermedios deseados. La regla termina su evaluación hasta que las reglas subordinadas terminen su evaluación. Las reglas subordinadas generarán una lista final de paradas intermedias, la cual terminará siendo el valor del argumento de salida Stops. 
intermed(Lista, Cnt, Stops):-
	q_intermed(Cnt),read_user_input(Input, Test),!,
	answer(Input, Lista, Cnt, Test, Stops).

%Regla: intermed_extra(Lista, Cnt, PlaceType,Stops).
%Ejemplo: intermed_extra([], 0, "Supermercado", []). 
%			->>(preguntará "cuál supermercado" y tambien preguntará por su dirección).
%		Stops=[cartago, tres rios]
%Descripción: Maneja el paso intermedio de cuando un usuario responde a una pregunta de destino intermedio con información general del lugar, lo que hace necesario obtener una respuesta específica de lugar. La regla termina evaluación hasta que el usuario deje de solicitar más destinos, y Stops toma entonces el valor de la lista definitiva de paradas intermedias a usarse para el cálculo de la respuesta final.
intermed_extra(Lista,Cnt,PlaceType, Stops):-
	q_which(PlaceType), 
	read_user_input(Input, _), key_nominal(Input, nominal(_, Place, _)),
	q_direction(Place),
	read_user_input(Input2, Test2),
	answer(Input2, Lista, Cnt, Test2, Stops).

%Regla: stop_asking_intermed(Input).
%Ejemplo: stop_asking_intermed([exclamation(no)]). true.
%Descripción: Describe si se debe dejar de preguntar al usuario por destinos intermedios. Esta condición se da solo si una exclamación negativa forma parte de la respuesta del usuario (la cual se encuentra descompuesta en Input).
stop_asking_intermed(Input):-list_member(exclamation(no), Input).
stop_asking_intermed(Input):-list_member(exclamation(no,_), Input).

%Regla: answer(Input, List, Cnt, Test, Stops).
%Ejemplo: answer(A, [tresrios], 2, B, Stops).
%		Dado previamente que: A = [svo(nominal(me, "me", "me"), verbal([gustaria]), nominal(cartago, "cartago", "cartago"))], B = ok.
%		Stops=[cartago, tresrios] *luego de algunos pasos, si se dan destinos intermedios y no solicita más paradas 
%Descripción: Una respuesta a las preguntas del programa son dadas por una entrada de usuario descompuesta por read_user_input(Input), una lista de destinos intermedios previos anotados(List) previos a la evaluación actual, un contador de destinos intermedios(Cnt), un indicador de coherencia de respuesta(Test) y una lista de paradas definitiva(Stops). Una respuesta distinta de una exclamación negativa resulta en una extensión de la respuesta mediante una nueva evaluación. Una exclamación negativa resulta en la evaluación de la lista List, de la cual Stops toma su valor final.  
answer(Input, Lista, Cnt, Test, Stops):-Test = ok, key_nominal(Input, nominal(Lugar, _, _)),
	city(Lugar),!,list_push(Lugar, Lista, NewList),Cntx is Cnt+1, intermed(NewList, Cntx, Stops).

answer(Input, Lista, Cnt, Test, Stops):-Test = ok, key_nominal(Input, nominal(Lugar, _, Lugar_orig)),
	not(city(Lugar)),!, intermed_extra(Lista, Cnt, Lugar_orig, Stops),!.

answer(Input, Lista, _, Test, Stops):-Test = ok,not(key_nominal(Input,_)),stop_asking_intermed(Input),!,Stops=Lista.

answer(Input, Lista, Cnt, Test, Stops):-Test = ok,not(key_nominal(Input,_)),not(stop_asking_intermed(Input)),!,
	wazelog_writeln("Perdon, no he podido entenderle, repito mi pregunta."),
	intermed(Lista, Cnt, Stops).

answer(_, Lista, Cnt, Test, Stops):-Test \=ok,!, 
	wazelog_writeln("Perdon, no he podido entenderle, repito mi pregunta."),
	intermed(Lista, Cnt, Stops).


