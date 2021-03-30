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
	fail.

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
	string_lower(WordString, LoweredString),
	atom_string(Lowered, LoweredString),
	append(Previous, [word(Lowered, WordString)], Next),
	lex(Rest, Tokens, Next).
