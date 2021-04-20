:- module(path, [shortest_path_through/4]).
:- use_module(library(heaps)).
:- use_module(library(dicts)).
:- use_module(routes).

%Regla:
%Ejemplo:
%Descripción:
shortest_path_through(Source, [], Target, shortest_path(Path, Cost)) :-
	shortest_path(Source, Target, Path, Cost),
	!.
shortest_path_through(Source, [], Target, no_route(Source, Target)) :-
	!.
shortest_path_through(Source, [Stop | Stops], Target, Result) :-
	shortest_path(Source, Stop, FirstPath, FirstCost),
	!,
	shortest_path_through(Stop, Stops, Target, NextResult),
	(
		NextResult = shortest_path([Stop | NextPath], NextCost),
		Cost is FirstCost + NextCost,
		append(FirstPath, NextPath, Path),
		Result = shortest_path(Path, Cost);

		NextResult = Result
	),
	!.
shortest_path_through(Source, [Stop | _], _, no_route(Source, Stop)).

%Regla:
%Ejemplo:
%Descripción:
shortest_path(Source, Target, Path, Cost) :-
	empty_heap(Empty),
	add_to_heap(Empty, 0, Source, Heap),
	dict_create(Initial, nodes, [Source: node('', 0, unvisited)]),
	shortest_path(Source, Target, Heap, Initial, Nodes),
	traceback(Source, Target, Nodes, ReversePath, Cost),
	reverse(ReversePath, Path).


%Regla:
%Ejemplo:
%Descripción:
traceback(Source, Source, _, [Source], 0) :-
	!.
traceback(Source, Target, Nodes, [Target | ReversePath], Cost) :-
	get_dict(Target, Nodes, node(Parent, Cost, _)),
	traceback(Source, Parent, Nodes, ReversePath, _).

shortest_path(Source, Target, Heap, Known, Nodes) :-
	get_from_heap(Heap, Distance, Key, NextHeap),
	!,
	get_dict(Key, Known, State),
	visit(Source, Target, Key, Distance, State, NextHeap, Known, Nodes).

%Regla:
%Ejemplo:
%Descripción:
visit(_, Target, Target, Distance, node(_, Distance, unvisited), _, Nodes, Nodes) :-
	!.
visit(Source, Target, Key, Distance, node(Parent, Distance, unvisited), Heap, Known, Nodes) :-
	!,
	put_dict(Key, Known, node(Parent, Distance, visited), KnownWithVisited),
	findall(Neighbor, arco(Key, Neighbor, _, _, _), Neighbors),
	test_neighbors(Neighbors, Key, Distance, Heap, KnownWithVisited, NextHeap, NextKnown),
	visit(Source, Target, Key, Distance, node(Parent, Distance, visited), NextHeap, NextKnown, Nodes).
visit(Source, Target, _, _, _, Heap, Known, Nodes) :-
	shortest_path(Source, Target, Heap, Known, Nodes).

%Regla:
%Ejemplo:
%Descripción:
test_neighbors([], _, _, Heap, Known, Heap, Known).
test_neighbors([Neighbor | Neighbors], Key, Distance, Heap, Known, NextHeap, NextKnown) :-
	get_dict(Neighbor, Known, State),
	!,
	override_distance(Key, Distance, Neighbor, State, Heap, Known, HeapWithNeighbor, KnownWithNeighbor),
	test_neighbors(Neighbors, Key, Distance, HeapWithNeighbor, KnownWithNeighbor, NextHeap, NextKnown).
test_neighbors([Neighbor | Neighbors], Key, Distance, Heap, Known, NextHeap, NextKnown) :-
	FakeNode = node('', inf, unvisited),
	override_distance(Key, Distance, Neighbor, FakeNode, Heap, Known, HeapWithNeighbor, KnownWithNeighbor),
	test_neighbors(Neighbors, Key, Distance, HeapWithNeighbor, KnownWithNeighbor, NextHeap, NextKnown).

%Regla:
%Ejemplo:
%Descripción:
override_distance(Key, Distance, Neighbor, node(_, OldDistance, unvisited), Heap, Known, NextHeap, NextKnown) :-
	arco(Key, Neighbor, Cost, _, _),
	NewDistance is Distance + Cost,
	NewDistance < OldDistance,
	!,
	add_to_heap(Heap, NewDistance, Neighbor, NextHeap),
	put_dict(Neighbor, Known, node(Key, NewDistance, unvisited), NextKnown).
override_distance(_, _, _, _, Heap, Known, Heap, Known).
