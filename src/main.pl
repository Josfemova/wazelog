#!/usr/bin/env swipl

:- use_module(wazelog).
:- use_module(lang).
:- initialization(main, main).

main(_) :-
	find_language,
	loop.

loop :-
	start(Out),
	(
		Out = stop;

		Out = continue,
		loop
	).
