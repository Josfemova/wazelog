#!/usr/bin/env swipl

:- use_module(wazelog).
:- use_module(lang).
:- initialization(main, main).

%Regla: main(Argv).
%Ejemplo:
%  ?- main(["--lang", es]).
%  ...
%Descripcion: Entrypoint de la aplicacion.
%Se debe proporcionar un lenguaje por la
%linea de comandos (parametro `Argv`).
main(['--lang', Lang]) :-
	set_lang(Lang),
	!,
	loop.
main(_) :-
	writeln("Usage: main.pl --lang <lang>").

%Regla: loop.
%Ejemplo:
%  ?- loop. 
%  ...
%Descripcion: Bucle principal. Ejecuta en repeticion
%la rutina conversacional de WazeLog hasta que el usuario
%indique que desea salir de la aplicacion.
loop :-
	start(Out),
	(
		Out = stop;

		Out = continue,
		loop
	).
