:- module(nlp, [parse_user_input/3,n/3]).

sentence_sep('.').
sentence_sep(',').
sentence_sep(';').
sentence_sep(':').

exclamation(si).
exclamation(no).
exclamation(hola).
exclamation(gracias).
verbal(estoy).
verbal(voy).
verbal(necesito).
verbal(ir).
verbal(es).
verbal(llegar).
verbal(pasar).
verbal(ubica).
verbal(gustaria).

filler(me).
filler(que).
filler(a).
filler(al).
%filler(el).
%filler(la).
filler(se).
filler(en).
filler(un).
filler(una).
filler(tengo).
filler(por).

parse_user_input(Input, Sentences, FailureHead) :-
	lex(Input, Tokens),
	unbounded(Tokens, Sentences, FailureHead).

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
	is_alpha(Alpha),
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

undecorate([], []).
undecorate(['á' | Cs], ['a' | Us]) :- !, undecorate(Cs, Us).
undecorate(['é' | Cs], ['e' | Us]) :- !, undecorate(Cs, Us).
undecorate(['í' | Cs], ['i' | Us]) :- !, undecorate(Cs, Us).
undecorate(['ó' | Cs], ['o' | Us]) :- !, undecorate(Cs, Us).
undecorate(['ú' | Cs], ['u' | Us]) :- !, undecorate(Cs, Us).
undecorate(['ü' | Cs], ['u' | Us]) :- !, undecorate(Cs, Us).
undecorate([C | Cs], [C | Us])     :- undecorate(Cs, Us).

nominal(N) :-
	not(exclamation(N)), not(verbal(N)), not(filler(N)).

classify(punct(_), punct).
classify(word(Atom, _), filler) :-
	filler(Atom),
	!.
classify(word(Atom, _), verbal(Atom)) :-
	verbal(Atom),
	!.
classify(word(Atom, _), exclamation(Atom)) :-
	exclamation(Atom),
	!.
classify(word(Atom, Original), nominal(Atom, Original)).

clause(_). /* to do
clause(exclamation(_, _)).
clause(nominal([_ | _])).
clause(svo(nominal([_ | _]), _, _)).
clause(svo(nominal([]), verbal([_ | _]), O)) :-
	clause(O).*/

ast_join(Tree, filler, Tree).
ast_join(nomatch, nominal(A, Orig), nominal(A, Orig)).
ast_join(nomatch, verbal(V), verbal([V])).
ast_join(nominal(LeftA, LeftOrig), nominal(RightA, RightOrig), nominal(NextA, NextOrig)) :-
	nominal_join(nominal(LeftA, LeftOrig), nominal(RightA, RightOrig), nominal(NextA, NextOrig)).
ast_join(nominal(A, Orig), verbal(V), svo(nominal(A, Orig), verbal([V]), nominal('', ""))).
ast_join(verbal(V), nominal(A, Orig), svo(nominal('', ""), verbal(V), nominal(A, Orig))).
ast_join(verbal(LeftV), verbal(RightV), verbal(NextV)) :-
	append(LeftV, [RightV], NextV).
ast_join(svo(S, V, svo(InnerS, InnerV, InnerO)), Term, svo(S, V, O)) :-
	!,
	ast_join(svo(InnerS, InnerV, InnerO), Term, O).
ast_join(svo(S, V, nominal(LeftA, LeftOrig)), nominal(RightA, RightOrig), svo(S, V, nominal(NextA, NextOrig))) :-
	!,
	ast_join(nominal(LeftA, LeftOrig), nominal(RightA, RightOrig), nominal(NextA, NextOrig)).
ast_join(svo(S, verbal(LeftV), nominal('', "")), verbal(RightV), svo(S, verbal(NextV), nominal('', ""))) :-
	!,
	ast_join(verbal(LeftV), verbal(RightV), verbal(NextV)).
ast_join(svo(S, V, O), verbal(Verbal), svo(S, V, svo(O, verbal([Verbal]), nominal('', "")))).

nominal_join(nominal('', ""), nominal(A, Orig), nominal(A, Orig)) :-
	!.
nominal_join(nominal(A, Orig), nominal('', ""), nominal(A, Orig)) :-
	!.
nominal_join(nominal(LeftA, LeftOrig), nominal(RightA, RightOrig), nominal(NextA, NextOrig)) :-
	atom_concat(LeftA, RightA, NextA),
	string_concat(LeftOrig, " ", WithSpace),
	string_concat(WithSpace, RightOrig, NextOrig).

unbounded(Tokens, Sentences, FailureHead) :-
	unbounded(Tokens, Sentences, [], FailureHead).
unbounded([], Sentences, Sentences, ok) :-
	!.
unbounded([punct(Sep) | Tokens], Sentences, Previous, FailureHead) :-
	sentence_sep(Sep),
	!,
	unbounded(Tokens, Sentences, Previous, FailureHead).
unbounded(Tokens, Sentences, Previous, FailureHead) :-
	sentence(Tokens, Rest, Sentence),
	!,
	append(Previous, [Sentence], Next),
	unbounded(Rest, Sentences, Next, FailureHead).
unbounded([FailureHead | _], Sentences, Sentences, FailureHead).

sentence(Tokens, Rest, Sentence) :- sentence(Tokens, Rest, Sentence, nomatch).
sentence([], [], Sentence, Sentence) :-
	!,
	clause(Sentence).
sentence([punct(Sep) | Rest], Rest, Sentence, Acc) :-
	sentence_sep(Sep),
	!,
	sentence([], [], Sentence, Acc).
sentence([word(Exclamation, _) | Tokens], Rest, Sentence, nomatch) :-
	exclamation(Exclamation),
	!,
	sentence(Tokens, Rest, Sentence, exclamation(Exclamation, nominal('', ""))).
sentence([word(Nominal, Original) | Tokens], Rest, Sentence, exclamation(E, Detail)) :-
	nominal(Nominal),
	!,
	nominal_join(Detail, nominal(Nominal, Original), Next),
	sentence(Tokens, Rest, Sentence, exclamation(E, Next)).
sentence(Rest, Rest, exclamation(E, Detail), exclamation(E, Detail)) :-
	!.
sentence([T | Tokens], Rest, Sentence, Ast) :-
	classify(T, Term),
	ast_join(Ast, Term, NextAst),
	sentence(Tokens, Rest, Sentence, NextAst).


n([nominal(A, Orig)], A, Orig):-!.
n([svo(_, _, O)], A, Orig) :-!, n([O], A, Orig).
n([_ | Es], A, Orig) :- n(Es, A, Orig).

