:- module(routes, [city/2, arco/5]).

city(sanjose, "San Jose").
city(cartago, "Cartago").
city(sanpedro, "San Pedro").
city(tresrios, "Tres Rios").
city(zapote, "Zapote").
city(taras, "Taras").
city(paraiso, "Paraiso").
city(quircot, "El Quircot").
city(paseocolon, "Paseo Colon").
city(desamparados, "Desamparados").
city(guadalupe, "Guadalupe").
city(curridabat, "Curridabat").
city(sabana, "La Sabana").

% El grafo es mixto
arco_bi(cartago,taras,3,5,25).
arco_bi(cartago,paraiso,5,10,25).
arco_bi(taras, tresrios,3,5,25).
arco_bi(tresrios, sanpedro, 10,25,25).
arco_bi(sanpedro,tresrios,10,25,25).
arco_bi(sanpedro,zapote,2,5,25).
arco_bi(sanpedro, curridabat,2,10,25).
arco_bi(sanpedro,sanjose,3,5,25).
arco_bi(sanjose,zapote,3,5,25).
arco_bi(sanjose,curridabat,3,5,25).
arco(Source, Target, Cost, BestTime, WorstTime) :-
	arco_bi(Source, Target, Cost, BestTime, WorstTime);
	arco_bi(Target, Source, Cost, BestTime, WorstTime).
