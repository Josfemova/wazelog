%Instituto Tecnologico de Costa Rica
%Area Academica de Ingenieria en Computadores
%Lenguajes, Compialdores e Interpretes
%Estudiantes
%		Jose Morales Vargas		2019024270
%		Alejandro Soto Chacon	2019008164
%Semestre I 2021

%tipo_sitio(clasificacion).
tipo_sitio(supermercado).
tipo_sitio(parque).
tipo_sitio(mall).
tipo_sitio(tienda).
tipo_sitio(ferreteria).
%ciudad('lugar').
ciudad('san jose').
ciudad('cartago').
ciudad('san pedro').
ciudad('tres rios').


saludo:-writeln("Bienvenido a WazeLog, la mejor logica de llegar a su destino"),
	writeln("Por favor indiqueme donde se encuentra").
despedida:-writeln("Muchas gracias por utilizar Wazelog!").
preg_intermedio(1):-write("Algun destino intermedio?"),!.
preg_intermedio(_):-write("Algun otro destino intermedio?").
preg_direccion(Lugar):-format("Donde se encuentra ~w?", [Lugar]).
