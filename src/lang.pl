:- module(lang, [sentence_sep/1, exclamation/1, verbal/1,
                 before_nominal/1, contraction/2, unclassified/1]).

sentence_sep('.').
sentence_sep(',').
sentence_sep(';').
sentence_sep(':').

exclamation(si).
exclamation(no).
exclamation(hola).
exclamation(gracias).

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

before_nominal(el).
before_nominal(los).
before_nominal(la).
before_nominal(las).
before_nominal(de).

contraction(al, [a, el]).
contraction(del, [de, el]).

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
