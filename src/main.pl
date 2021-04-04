#!/usr/bin/env swipl

:- use_module(wazelog).
:- initialization(main, main).

main(_) :-
	loop.

loop :-
	start,
	(
		true, % Cambiar por Out = bye
		!;

		loop
	).
