:- module(routes, [city/2, arco/5]).

%Hechos: city(lugar, nombre).
%Ejemplo:
%  ?- city(sanjose, N).
%  N = "San Jose".
%Descripcion: Declara las ciudades conocidas y sus nombres.
city(sanjose, "San Jose").
city(corralillo, "Corralillo").
city(musgoverde, "Musgo Verde").
city(tresrios, "Tres Rios").
city(cartago, "Cartago").
city(pacayas, "Pacayas").
city(paraiso, "Paraiso").
city(orosi, "Orosi").
city(cervantes, "Cervantes").
city(cachi, "Cachi").
city(juanvinias, "Juan Vinias").
city(turrialba, "Turrialba").

%Hechos: arco_bi(lugar1, lugar2, distancia, tiempo, con_presa).
%Ejemplo:
%  ?- arco_bi(sanjose, cartago, D, Tmin, Tmax).
%  D = 20,
%  Tmin = 20,
%  Tmax = 40.
%Descripcion: Declara arcos bidireccionales (el grafo es mixto).
arco_bi(sanjose, corralillo, 22, 22, 44).
arco_bi(sanjose, cartago, 20, 20, 40).
arco_bi(corralillo, musgoverde, 6, 6, 12).
arco_bi(musgoverde, cartago, 10, 10, 20).
arco_bi(tresrios, pacayas, 15, 15, 30).
arco_bi(cartago, pacayas, 13, 13, 26).
arco_bi(pacayas, cervantes, 8, 8, 16).
arco_bi(paraiso, orosi, 8, 8, 16).
arco_bi(paraiso, cachi, 10, 10, 20).
arco_bi(orosi, cachi, 12, 12, 24).
arco_bi(cervantes, cachi, 7, 7, 14).
arco_bi(cachi, turrialba, 40, 40, 80).

%Hechos: arco(origen, destino, distancia, tiempo, con_presa).
%Ejemplo:
%  ?- arco(cartago, paraiso, D, Tmin, Tmax).
%  D = 10,
%  Tmin = 10,
%  Tmax = 20.
%Descripcion: Declara arcos unidireccionales (el grafo es mixto).
arco(tresrios, sanjose, 8, 8, 16).
arco(cartago, tresrios, 8, 8, 16).
arco(cartago, paraiso, 10, 10, 20).
arco(paraiso, cervantes, 4, 4, 8).
arco(turrialba, pacayas, 18, 18, 36).
arco(cervantes, juanvinias, 5, 5, 10).
arco(juanvinias, turrialba, 4, 4, 8).

%Regla: arco(origen, destino, distancia, tiempo, con_presa).
%Ejemplo:
%  ?- arco(cartago, sanjose, D, Tmin, Tmax).
%  D = 20,
%  Tmin = 20,
%  Tmax = 40.
%Descripcion: Descompone arcos bidireccionales en dos
%arcos unidireccionales, simplificando la logica de grafo.
arco(Source, Target, Cost, BestTime, WorstTime) :-
	arco_bi(Source, Target, Cost, BestTime, WorstTime);
	arco_bi(Target, Source, Cost, BestTime, WorstTime).
