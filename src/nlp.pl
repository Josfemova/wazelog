:- module(nlp, [parse_user_input/2, key_nominal/2]).
:- use_module(lang).

%Regla: parse_user_input(Input, Result).
%Ejemplo: 
%?- parse_user_input("Voy a Cartago", Result).
%Result = ok([svo(nominal('', "", ""), verbal([voy]), nominal(cartago, "cartago", "Cartago"))]).
%Descripcion: Toma un string input del usuario y lo separa en sus diferentes elementos 
parse_user_input(Input, Result) :-
	lex(Input, Tokens),
	expand(Tokens, Expanded),
	unbounded(Expanded, Result).

%Regla: filler(Word).
%Ejemplo:
%?- filler(por).
%true.
%Descripcion: Toma un atomo que representa una palabra e indica si la palabra es o no relevante para el analisis de lenguaje. 
filler(Word) :-
	unclassified(Word);
	before_nominal(Word);
	contraction(Word, _).

%Regla: nominal(N).
%Ejemplo: 
%?- nominal(sanjose).
%true.
%Descripcion: Toma un atomo que representa una palabra e indica si dicha palabra es o no un sustantivo.
nominal(N) :-
	not(exclamation(N, _)), not(verbal(N)), not(filler(N)).

%Regla: expand(Tokens, Expanded). 
%Ejemplo:
%?- expand([word(al, "al"), word(del, "del"), word(alto, "Alto")], Expanded).
%Expanded = [word(a, "a"), word(el, "el"), word(de, "de"), word(el, "el"), word(alto, "Alto")].
%Descripcion: La regla toma una lista de palabras respresentadas en la lista `Tokens` como elementos `word(atomo, string)` y separa las palabras que sean identificadas como contracciones en tokens distintos, la lista conformada por las palabras procesadas por `expand` es el argumento de salida Expanded.
expand([], []).
expand([word(Contraction, _) | Tokens], NextTokens) :-
	contraction(Contraction, Expanded),
	!,
	atoms_to_words(Expanded, ExpandedWords),
	expand(Tokens, NextExpanded),
	append(ExpandedWords, NextExpanded, NextTokens).
expand([T | Tokens], [T | NextTokens]) :-
	expand(Tokens, NextTokens).

%Regla: atoms_to_words(Atoms, Words).
%Ejemplo: 
%?- atoms_to_words([sanjose, manzana, cartago],Words).
%Words = [word(sanjose, "sanjose"), word(manzana, "manzana"), word(cartago, "cartago")].
%Descripcion: "Traduce" una lista de atomos `Atoms` a una lista de palabras `word(atomo, string)`.
atoms_to_words([], []) :-
	!.
atoms_to_words([Atom | Atoms], [word(Atom, Orig) | NextWords]) :-
	atom_string(Atom, Orig),
	atoms_to_words(Atoms, NextWords).

%Regla: lex(Input, Tokens).
%Ejemplo:
%?- lex("voy a San Jose", Tokens).
%Tokens = [word(voy, "voy"), word(a, "a"), word(san, "San"), word(jose, "Jose")].
%Descripcion: Toma un string `Input` que representa una oracion y obtiene una lista de tokens `word(atomo, string)`, con cada token correspondiente a una de las palabras de la oracion.
lex(Input, Tokens) :-
	string_chars(Input, Chars),
	lex(Chars, Tokens, []).
lex([], Tokens, Tokens) :-
	!.
lex([Alpha | Rest], Tokens, Previous) :-
	is_alpha(Alpha),
	!,
	lex(Rest, Tokens, Previous, [Alpha]).
lex([Space | Rest], Tokens, Previous) :-
	is_space(Space),
	!,
	lex(Rest, Tokens, Previous).
lex([Punct | Rest], Tokens, Previous) :-
	!,
	append(Previous, [punct(Punct)], Next),
	lex(Rest, Tokens, Next).
lex([Alpha | Rest], Tokens, Previous, WordChars) :-
	(
		is_alpha(Alpha);
		Alpha = '\''
	),
	!,
	append(WordChars, [Alpha], NextChars),
	lex(Rest, Tokens, Previous, NextChars).
lex(Rest, Tokens, Previous, WordChars) :-
	atom_string(WordChars, WordString),
	string_lower(WordString, Lowered),
	string_chars(Lowered, LoweredChars),
	undecorate(LoweredChars, UndecoratedChars),
	atom_chars(Undecorated, UndecoratedChars),
	append(Previous, [word(Undecorated, WordString)], Next),
	lex(Rest, Tokens, Next).

%Regla: undecorate(Cs, Us).
%Ejemplo:
%?- undecorate(['a','e','i','o','u'], Us).
%Us = ['a','e','i','o','u'].
%Descripcion: Toma una lista de caracteres y obtiene su version sin decoraciones(acentos y dieresis) para evitar conflictos a la hora de procesar datos. 
undecorate([], []).
undecorate(['a' | Cs], ['a' | Us]) :- !, undecorate(Cs, Us).
undecorate(['e' | Cs], ['e' | Us]) :- !, undecorate(Cs, Us).
undecorate(['i' | Cs], ['i' | Us]) :- !, undecorate(Cs, Us).
undecorate(['o' | Cs], ['o' | Us]) :- !, undecorate(Cs, Us).
undecorate(['u' | Cs], ['u' | Us]) :- !, undecorate(Cs, Us).
undecorate([C | Cs], [C | Us])     :- undecorate(Cs, Us).

%Regla: classify(word(Atom, Orig), Type).
%Ejemplo:
%?- classify(word(encuentro, "encuentro"), Type).
%Type = verbal(encuentro). 
%Descripcion: Clasifica palabras segun su funcion en una oracion, ya sea en tipo verbal, nominal, exclamacion o relleno.
classify(punct(_), punct).
classify(word(Atom, Orig), filler(Atom, Orig)) :-
	filler(Atom),
	!.
classify(word(Atom, _), verbal(Atom)) :-
	verbal(Atom),
	!.
classify(word(Atom, _), exclamation(Type)) :-
	exclamation(Atom, Type),
	!.
classify(word(Atom, Original), nominal(Atom, Original)).

%Regla: clause(oracion).
%Ejemplo:
%  ?- clause(nominal(yo, "yo", "yo")).
%  true.
%  ?- clause(exclamation(affirmative)).
%  true.
%Descripcion: Tiene exito solo si la oracion en
%cuestion es una oracion valida. Esto ocurre para
%todas las subexpresiones validas, excepto formas
%verbales solitarias sin asociacion jerarquica.
clause(verbal(_)) :-
	!,
	fail.
clause(Clause) :-
	well_formed(Clause).

%Regla: well_formed(Expresion). 
%Ejemplo:
%?- well_formed(nominal(yo,"yo","yo")).
%true.
%Descripcion: Evalua si una expresion esta formada de manera correcta. La expresion puede ser de tipo verbal, nominal, exclamacion, u otro tipo. 
well_formed(exclamation(_)).
well_formed(verbal([_ | _])).
well_formed(nominal('', _, _)) :-
	!,
	fail.
well_formed(nominal(_, _, _)).
well_formed(svo(S, V, O)) :-
	(not(well_formed(V)); well_formed(O)),
	(well_formed(V); well_formed(S)).

%Regla: ast_join(existente, agregado, Salida).
%Ejemplo:
%  ?- ast_join(nomatch, filler(la, "la"), R1), ast_join(R1, nominal(mesa, "mesa"), R2).
%  R1 = nominal('', "la", ""),
%  R2 = nominal(mesa, "la mesa", "mesa").
%Descripcion: Construye un arbol de sintaxis, con algunas
%interpretaciones semanticas incluidas, a partir de un estado
%previo del mismo arbol y un componente siguiente a agregar.
%El atomo `nomatch` se utiliza como arbol previo para indicar
%que no existia uno anteriormente.
ast_join(nomatch, nominal(A, Orig), nominal(A, Orig, Orig)).
ast_join(nomatch, verbal(V), verbal([V])).
ast_join(nomatch, filler(F, Orig), nominal('', Orig, "")) :-
	before_nominal(F).
ast_join(nominal(LA, LOrig, LBare), nominal(RA, ROrig), nominal(NextA, NextOrig, NextBare)) :-
	nominal_join(nominal(LA, LOrig, LBare), nominal(RA, ROrig), nominal(NextA, NextOrig, NextBare)).
ast_join(nominal(A, Orig, Bare), verbal(V), svo(nominal(A, Orig, Bare), verbal([V]), nominal('', "", ""))).
ast_join(nominal(A, LeftOrig, Bare), filler(F, RightOrig), nominal(A, NextOrig, Bare)) :-
	before_nominal(F),
	nominal_join(nominal(A, LeftOrig, Bare), nominal('', RightOrig), nominal(A, NextOrig, _)).
ast_join(verbal(V), nominal(A, Orig), svo(nominal('', "", ""), verbal(V), nominal(A, Orig, Orig))).
ast_join(verbal(LeftV), verbal(RightV), verbal(NextV)) :-
	append(LeftV, [RightV], NextV).
ast_join(verbal(V), filler(F, Orig), svo(nominal('', "", ""), verbal(V), nominal('', Orig, ""))) :-
	before_nominal(F).
ast_join(svo(S, verbal(LeftV), nominal('', "", "")), verbal(RightV), svo(S, verbal(NextV), nominal('', "", ""))) :-
	!,
	ast_join(verbal(LeftV), verbal(RightV), verbal(NextV)).
ast_join(svo(S, V, O), verbal(Verbal), svo(S, V, svo(O, verbal([Verbal]), nominal('', "", "")))) :-
	!.
ast_join(svo(S, V, O), Term, svo(S, V, NextO)) :-
	!,
	ast_join(O, Term, NextO).
ast_join(Tree, filler(_, _), Tree).

%Regla: nominal_join(izquierdo, derecho, Salida).
%Ejemplo:
%  ?- nominal_join(nominal('', "la", ""), nominal("sabana", sabana), R).
%  R = nominal(sabana, "la sabana", "sabana").
%Descripcion: Concatena dos formas nominales en un nominal compuesto.
nominal_join(nominal(LA, LOrig, LBare), nominal(RA, ROrig), nominal(NextA, NextOrig, NextBare)) :-
	atom_concat(LA, RA, NextA),
	append_space(LOrig, OrigWithSpace),
	append_space(LBare, BareWithSpace),
	string_concat(OrigWithSpace, ROrig, NextOrig),
	string_concat(BareWithSpace, ROrig, NextBare).

%Regla: append_space(sin_espacio, ConEspacio).
%Ejemplo:
%  ?- append_space("a", "a ").
%  true.
%Descripcion: Agrega un espacio al final de una cadena
%solamente si la entrada no es la cadena vacia.
append_space("", "") :-
	!.
append_space(String, WithSpace) :-
	string_concat(String, " ", WithSpace).

%Regla: unbounded(tokens, Salida).
%Ejemplo: Ver `sentence/3`.
%Descripcion: Parsea una entrada completa ("no delimitada",
%por tanto el nombre del predicada). El resultado es o
%una lista de oraciones o una indicacion de fallo.
unbounded(Tokens, Result) :-
	unbounded(Tokens, [], Result).
unbounded([], Sentences, ok(Sentences)) :-
	!.
unbounded([punct(Sep) | Tokens], Previous, Result) :-
	sentence_sep(Sep),
	!,
	unbounded(Tokens, Previous, Result).
unbounded(Tokens, Previous, Result) :-
	sentence(Tokens, Rest, Sentence),
	!,
	append(Previous, [Sentence], Next),
	unbounded(Rest, Next, Result).
unbounded([FailureHead | _], _, fail(FailureHead)).

%Regla: sentence(tokens, Resto, Oracion).
%Ejemplo:
%  ?- sentence([word(yo, "Yo"), word(estoy, "estoy"), word(en, "en"), word(cartago, "Cartago"), punct('.')], R, S).
%  R = [],
%  S = svo(nominal(yo, "Yo", "Yo"), verbal([estoy]), nominal(cartago, "Cartago", "Cartago")) ;
%Descripcion: Parsea una oracion a partir de un flujo
%de entrada. Su salida es tanto la oracion como la lista
%de tokens que la suceden y que deben luego parsearse como
%mas oraciones. Una oracion puede ser una forma exclamativa,
%una forma nominal independiente o una estructura
%subjeto-verbo-objeto (SVO).
sentence(Tokens, Rest, Sentence) :- sentence(Tokens, Rest, Sentence, nomatch).
sentence([], [], Sentence, Sentence) :-
	!,
	clause(Sentence).
sentence([punct(Sep) | Rest], Rest, Sentence, Acc) :-
	sentence_sep(Sep),
	!,
	sentence([], [], Sentence, Acc).
sentence([word(Exclamation, _) | Tokens], Rest, Sentence, nomatch) :-
	exclamation(Exclamation, Type),
	!,
	sentence(Tokens, Rest, Sentence, exclamation(Type)).
sentence([word(Word, _) | Tokens], Rest, Sentence, exclamation(E)) :-
	not(verbal(Word)),
	!,
	sentence(Tokens, Rest, Sentence, exclamation(E)).
sentence(Rest, Rest, exclamation(E), exclamation(E)) :-
	!.
sentence([T | Tokens], Rest, Sentence, Ast) :-
	classify(T, Term),
	ast_join(Ast, Term, NextAst),
	sentence(Tokens, Rest, Sentence, NextAst).


%Regla: key_nominal(SVO, nominal(A, Orig, Bare)).
%Ejemplo: 
%?- key_nominal([svo(nominal('', "", ""), verbal([voy]), nominal(alto, "el Alto", "Alto"))], nominal(A, Orig, Bare).
%A = alto.
%Orig = "el Alto".
%Bare = "Alto".
%Descripcion: La regla toma una oracion representada en SVO como una estructura sujeto-verbo-objeto y busca su sustantivo complemento. Mayoritariamente utilizada para obtener el nombre de una ciudad, el cual siempre se encuentra en la posicion de complemento en la voz activa.
key_nominal([nominal(A, Orig, Bare)], nominal(A, Orig, Bare)) :-
	!.
key_nominal([svo(_, _, O)], N) :-
	!,
	key_nominal([O], N).
key_nominal([_ | Es], N) :-
	key_nominal(Es, N).
