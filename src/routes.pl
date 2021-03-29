:- module(routes, [shortest_path_through/5]).

arco(1, 2, 7, 10, 25).
arco(2, 1, 7, 10, 25).
arco(1, 6, 14, 10, 25).
arco(6, 1, 14, 10, 25).
arco(1, 3, 9, 10, 25).
arco(3, 1, 9, 10, 25).
arco(6, 3, 2, 10, 25).
arco(3, 6, 2, 10, 25).
arco(3, 4, 11, 10, 25).
arco(4, 3, 11, 10, 25).
arco(3, 2, 10, 10, 25).
arco(2, 3, 10, 10, 25).
arco(2, 4, 15, 10, 25).
arco(4, 2, 15, 10, 25).
arco(6, 5, 9, 10, 25).
arco(5, 6, 9, 10, 25).
arco(5, 4, 6, 10, 25).
arco(4, 5, 6, 10, 25).

shortest_path_through(Source, [], Target, Path, Cost) :-
	!,
	shortest_path(Source, Target, Path, Cost).
shortest_path_through(Source, [Stop | Stops], Target, Path, Cost) :-
	shortest_path(Source, Stop, FirstPath, FirstCost),
	shortest_path_through(Stop, Stops, Target, [Stop | NextPath], NextCost),
	Cost is FirstCost + NextCost,
	append(FirstPath, NextPath, Path).

shortest_path(_, _, _, _) :- fail.
