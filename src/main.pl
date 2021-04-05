#!/usr/bin/env swipl

:- use_module(wazelog).
:- use_module(lang).
:- initialization(main, main).

main(['--lang', Lang]) :-
	set_lang(Lang),
	!,
	loop.
main(_) :-
	writeln("Usage: main.pl --lang <lang>").

loop :-
	start(Out),
	(
		Out = stop;

		Out = continue,
		loop
	).
