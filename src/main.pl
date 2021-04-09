#!/usr/bin/env swipl

:- use_module(wazelog).
:- use_module(lang).
:- initialization(main, main).

%Regla: main(Argv).
%Ejemplo:
%  ?- main(["--lang", es]).
%  ...
%Descripción: Entrypoint de la aplicación.
%Se debe proporcionar un lenguaje por la
%línea de comandos (parámetro `Argv`).
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
%Descripción: Bucle principal. Ejecuta en repetición
%la rutina conversacional de WazeLog hasta que el usuario
%indique que desea salir de la aplicación.
loop :-
	start(Out),
	(
		Out = stop;

		Out = continue,
		loop
	).
