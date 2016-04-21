% vim: syntax=prolog
% first of all we need to define parser for algol16

% symbole przystankowe
% slowa kluczowe
% program --> "program" identyfikator blok
% blok --> deklaracje "begin" instrukcja_zlozona "end"
% procedura --> "procedure" nazwa_procedury "(" argumenty_formalne ")" blok
% argumenty_formalne --> puste | ciąg_argumentow_formalnych
% ciąg_argumentów_formalnych --> argument_formalny | ciąg_argumentów_formalnych "," argument_formalny
% instrukcja_zlozona --> instrukcja | instrukcja_zlozona ";" instrukcja
% instrukcja --> zmiena ":=" wyrazenie_arytmetyczne | "if" wyrazenie_logiczne "then" instrukcja_zlozona "fi" | "if" wyrazenie_logiczne "then" instrukcja_zlozona "else" instrukcja_zlozona "fi" | "while" wyrazenie_logiczne "do" instrukcja_zlozona "done" | "call" wywolanie_procedury | "return" wyrazenie_arytmetyczne | "read" zmienna | "write" wyrazenie_arytmetyczne
% wyrazenie_arytmetyczne --> skladnik | wyrazenie_arytmetyczne operator_addytywny skladnik
% skladnik --> czynnik | skladnik operator_multiplikatywny czynnik
% czynnik --> wyrazenie_proste | "-" wyrazenie_proste
% wyrazenie_proste --> wyrazenie_atomowe | "(" wyrazenie_arytmetyczne ")"
% wyrazenie_atomowe --> zmienna | wywolanie_procedury | literal_calkowitoliczbowy
% wywolanie_procedury --> nazwa_procedury "(" argumenty_faktyczne ")"
% argumenty_faktyczne --> puste | ciag_argumentow_faktycznych
% ciag_argumentow_faktycznych --> argument_faktyczny | ciag_argumentow_faktycznych "," argument_faktyczny
% argument_faktyczny --> wyrazenie_arytmetyczne
% wyrazenie_logiczne --> koniunkcja | wyrazenie_logiczne "or" koniunkcja
% koniunkcja --> warunek | koniunkcja "and" warunek
% warunek --> wyrazenie_relacyjne | "not" wyrazenie_relacyjne
% wyrazenie_relacyjne --> wyrazenie_arytmetyczne operator_relacyjny wyrazenie_arytmetyczne | "(" wyrazenie_logiczne ")"



% relational operators
rel_op --> "<".
rel_op --> "<=".
rel_op --> ">".
rel_op --> ">=".
rel_op --> "=".
rel_op --> "<>".

% multiplikative operators
mul_op --> "*".
mul_op --> "div".
mul_op --> "mod".

% additive operators
add_op --> "+".
add_op --> "-".

% digits and numbers
% digit(D) --> [D], {code_type(D, digit)}.
% digits([]) --> [].
% digits([H|T]) --> digit(H),!, digits(T).
% number(D, N) --> digits(Ds), { number_chars(N, [D|Ds]) }.
digit --> [D], {code_type(D,digit)}.
digits --> digit.
digits --> digits, digit.
number --> digits.

% letters and words
% letter(L) --> [L], {code_type(L, alpha)}.
letter --> [L], {code_type(L, alpha)}.

% identifier
% ident([H|T]) --> [H], {code_type(H, csym); H=39},!, ident(T).
% ident([]) --> [].
% identifier(L, Id) --> ident(As),{atom_codes(Id, [L|As])}.
special_char --> "_".
special_char --> "'".
ident --> letter, ident.
ident --> special_char, ident.
ident --> digit, ident.
ident --> [].
identifier --> letter, ident.

% variable
% variable(L,Id) --> identifier(L,Id).
variable --> identifier.
variables --> variable.
variables --> variables, ",", variable.

% formal argument
formal_arg --> variable.
formal_arg --> "value", variable.

% procedure name
proc_name --> identifier.

% declarator
declarator --> "local", variables.

% declaration
declaration --> declarator.
declaration --> procedure.

% declarations
declarations --> [].
declarations --> declarations, declaration.


% Testing
test_phrase(String, Pred) :- atom_codes(String, Codes), phrase(Pred, Codes).

test_identifier([]) :- test_phrase("a111", identifier).
test_identifier(["a111 not parsed by identifer"]).

test_all([]).
test_all([H | T]) :- call(H, E), (E = [] ; print(E)), test_all(T).

:- test_all([
  test_identifier
]).
