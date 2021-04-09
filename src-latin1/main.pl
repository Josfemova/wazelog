#!/usr/bin/env swipl

:- use_module(wazelog).
:- use_module(lang).
:- initialization(main, main).

%Regla: main(Argv).
%Ejemplo:
%  ?- main(["--lang", es]).
%  ...
%Descripci�n: Entrypoint de la aplicaci�n.
%Se debe proporcionar un lenguaje por la
%l�nea de comandos (par�metro `Argv`).
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
%Descripci�n: Bucle principal. Ejecuta en repetici�n
%la rutina conversacional de WazeLog hasta que el usuario
%indique que desea salir de la aplicaci�n.
loop :-
	start(Out),
	(
		Out = stop;

		Out = continue,
		loop
	).
