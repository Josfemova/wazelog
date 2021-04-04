#!/usr/bin/env swipl

:- use_module(wazelog).
:- initialization(main, main).

main(_) :-
	loop.

loop :-
	start(Out),
	(
		Out = stop;

		Out = continue,
		loop
	).
