% first of all we need to define parser for algol16

% symbole przystankowe
% slowa kluczowe
% program --> "program" identyfikator blok
% blok --> deklaracje "begin" instrukcja_zlozona "end"
% deklaracje --> pusta | deklaracje deklaracja
% deklaracja --> deklarator | procedura
% deklarator --> "local" zmienne
% zmienne --> zmienna | zmienna "," zmienne
% zmienna --> identyfikator
% procedura --> "procedure" nazwa_procedury "(" argumenty_formalne ")" blok
% nazwa_procedury --> identyfikator
% argumenty_formalne --> puste | ciąg_argumentow_formalnych
% ciąg_argumentów_formalnych --> argument_formalny | ciąg_argumentów_formalnych "," argument_formalny
% argument_formalny --> zmienna | "value" zmienna
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
digit(D) --> [D], {code_type(D, digit)}.
digits([]) --> [].
digits([H|T]) --> digit(H),!, digits(T).
number(D, N) --> digits(Ds), { number_chars(N, [D|Ds]) }.

% letters and words
letter(L) --> [L], {code_type(L, alpha)}.

% identifier
ident([H|T]) --> [H], {code_type(H, csym); H=39},!, ident(T).
ident([]) --> [].
identifier([H|T]) --> letter(H), ident(T).
