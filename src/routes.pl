:- module(routes, [city/2, arco/5]).

%Regla: 
%Ejemplo:
%?- 
%
%Descripción:
city(sanjose, "San José").
city(corralillo, "Corralillo").
city(musgoverde, "Musgo Verde").
city(tresrios, "Tres Ríos").
city(cartago, "Cartago").
city(pacayas, "Pacayas").
city(paraiso, "Paraíso").
city(orosi, "Orosi").
city(cervantes, "Cervantes").
city(cachi, "Cachí").
city(juanviñas, "Juan Viñas").
city(turrialba, "Turrialba").

% El grafo es mixto
%Regla: 
%Ejemplo:
%?- 
%
%Descripción:
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
arco(tresrios, sanjose, 8, 8, 16).
arco(cartago, tresrios, 8, 8, 16).
arco(cartago, paraiso, 10, 10, 20).
arco(paraiso, cervantes, 4, 4, 8).
arco(turrialba, pacayas, 18, 18, 36).
arco(cervantes, juanviñas, 5, 5, 10).
arco(juanviñas, turrialba, 4, 4, 8).
arco(Source, Target, Cost, BestTime, WorstTime) :-
	arco_bi(Source, Target, Cost, BestTime, WorstTime);
	arco_bi(Target, Source, Cost, BestTime, WorstTime).
