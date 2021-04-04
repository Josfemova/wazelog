:- module(lang, [sentence_sep/1, exclamation/1, verbal/1, before_nominal/1,
                 contraction/2, unclassified/1, place_type/1]).

%Regla: 
%Ejemplo:
%Descripción:
place_type(supermercado).
place_type(mercado).
place_type(tienda).
place_type(banco).
place_type(escuela).
place_type(restaurante).
place_type(parque).
place_type(instituto).

%Regla: 
%Ejemplo:
%Descripción:
sentence_sep('.').
sentence_sep(',').
sentence_sep(';').
sentence_sep(':').

%Regla: 
%Ejemplo:
%Descripción:
exclamation(si).
exclamation(no).
exclamation(hola).
exclamation(adios).
exclamation(gracias).

%Regla: 
%Ejemplo:
%Descripción:
verbal(esta).
verbal(estoy).
verbal(encuentro).
verbal(encuentra).
verbal(voy).
verbal(necesito).
verbal(ir).
verbal(es).
verbal(llegar).
verbal(pasar).
verbal(ubica).
verbal(gustaria).

%Regla: 
%Ejemplo:
%Descripción:
before_nominal(el).
before_nominal(los).
before_nominal(la).
before_nominal(las).
before_nominal(de).

%Regla: 
%Ejemplo:
%Descripción:
contraction(al, [a, el]).
contraction(del, [de, el]).

%Regla: 
%Ejemplo:
%Descripción:
unclassified(me).
unclassified(que).
unclassified(a).
unclassified(se).
unclassified(en).
unclassified(de).
unclassified(un).
unclassified(una).
unclassified(tengo).
unclassified(por).
unclassified(muchas).
