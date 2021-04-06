#!/usr/bin/env swipl

:- use_module(wazelog).
:- use_module(lang).
:- initialization(main, main).

%Regla: 
%Ejemplo:
%?- 
%
%Descripción:
main(['--lang', Lang]) :-
	set_lang(Lang),
	!,
	loop.
main(_) :-
	writeln("Usage: main.pl --lang <lang>").

%Regla: 
%Ejemplo:
%?- 
%
%Descripción:
loop :-
	start(Out),
	(
		Out = stop;

		Out = continue,
		loop
	).
