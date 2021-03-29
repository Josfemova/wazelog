:- module(routes, [shortest_path_through/5]).

use_module(library(heaps)).
use_module(library(dicts)).

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

shortest_path(Source, Target, Path, Cost) :-
	empty_heap(Empty),
	add_to_heap(Empty, 0, Source, Heap),
	dict_create(Initial, nodes, [Source: node('', 0, unvisited)]),
	shortest_path(Source, Target, Heap, Initial, Nodes),
	traceback(Source, Target, Nodes, ReversePath, Cost),
	reverse(ReversePath, Path).

traceback(Source, Source, _, [Source], 0) :-
	!.
traceback(Source, Target, Nodes, [Target | ReversePath], Cost) :-
	get_dict(Target, Nodes, node(Parent, Cost, _)),
	traceback(Source, Parent, Nodes, ReversePath, _).

shortest_path(_, _, _, _, _) :- fail.
