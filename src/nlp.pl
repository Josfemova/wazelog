:- module(nlp, [parse_user_input/3]).

sentence_sep('.').
sentence_sep(',').
sentence_sep(';').
sentence_sep(':').

exclamation(si).
exclamation(no).
exclamation(hola).

verbal(estoy).
verbal(llegar).
verbal(pasar).
verbal(ubica).
verbal(gustaria).

filler(me).
filler(que).
filler(a).
filler(al).
filler(el).
filler(la).
filler(se).
filler(en).
filler(un).
filler(una).
filler(tengo).

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

ast_join(_, _, _) :- fail.
nominal_join(_, _, _) :- fail.

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
	!.
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
