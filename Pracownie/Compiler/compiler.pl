% vim: syntax=prolog
% first of all we need to define parser for algol16

% symbole przystankowe
% slowa kluczowe
% program --> "program" identyfikator blok
% blok --> deklaracje "begin" instrukcja_zlozona "end"

% instrukcja_zlozona --> instrukcja | instrukcja_zlozona ";" instrukcja
% instrukcja --> zmiena ":=" wyrazenie_arytmetyczne | "if" wyrazenie_logiczne "then" instrukcja_zlozona "fi" | "if" wyrazenie_logiczne "then" instrukcja_zlozona "else" instrukcja_zlozona "fi" | "while" wyrazenie_logiczne "do" instrukcja_zlozona "done" | "call" wywolanie_procedury | "return" wyrazenie_arytmetyczne | "read" zmienna | "write" wyrazenie_arytmetyczne
% wyrazenie_arytmetyczne --> skladnik | wyrazenie_arytmetyczne operator_addytywny skladnik
% skladnik --> czynnik | skladnik operator_multiplikatywny czynnik
% czynnik --> wyrazenie_proste | "-" wyrazenie_proste

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
digits --> digit, digits.
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
variables --> variables,!, ",", variable.

% formal argument
formal_arg --> "value", variable.
formal_arg --> variable.

% string of formal arguments
formal_arg_str --> formal_arg,",",formal_arg_str.
formal_arg_str --> formal_arg.

% formal arguments
formal_args --> [].
formal_args --> formal_arg_str.

% real argument
real_arg --> arithmetic_expr.

% real arguments string
real_args_str --> real_arg, ",", real_args_str.
real_args_str --> real_arg.

% real arguments
real_args --> real_args_str.
real_args --> [].

% procedure name
proc_name --> identifier.

% procedure
procedure --> "procedure", proc_name, "(", formal_args, ")", block.

% procedure call
procedure_call --> proc_name, "(", real_args, ")".

% atom expression
atom_expr --> procedure_call.
atom_expr --> variable.
atom_expr --> number.

% simple expression
simple_expr --> "(", arithmetic_expr, ")".
simple_expr --> atom_expr.

% declarator
declarator --> "local", variables.

% declaration
declaration --> declarator.
declaration --> procedure.

% declarations
declarations --> [].
declarations --> declarations, declaration.


% Testing
test_phrase(String, Pred) :- 
  atom_codes(String, Codes), phrase(Pred, Codes).

test_identifier([]) :- 
  test_phrase("a111", identifier).
test_identifier(["a111 not parsed by identifer"]).

test_digit([]) :- 
  test_phrase("9", digit).
test_digit(["9 not parsed by digit"]).

test_digits([]) :- 
  test_phrase("987", digits).
test_digits(["987 not parsed by digits"]).

test_variable([]) :- 
  test_phrase("a112", variable).
test_variable(["a112 not parsed by variable"]).

test_variables([]) :- 
  test_phrase("a112", variables), 
  test_phrase("a112,aa", variables).
test_variables(["a112 or a112,aa not parsed by variables"]).

test_formal_arg([]) :- 
  test_phrase("a112", formal_arg), 
  test_phrase("valuea112", formal_arg).
test_formal_arg(["a112 or valuea112 not parsed by formal_arg"]).

test_formal_arg_str([]) :- 
  test_phrase("aqwer", formal_arg_str), 
  test_phrase("awer,wet3", formal_arg_str), 
  test_phrase("awe,valuewe2", formal_arg_str).
test_formal_arg_str(["aqwer, <<awer,wet3>> and <<awe,valuewe2>> not parsed by formal_arg_str"]).

test_formal_args([]) :- 
  test_phrase("",formal_args),
  test_phrase("awe,valuewe2", formal_args).
test_formal_args(["empty and awe,valuewe2 not parsed by formal_args"]).

test_proc_name([]) :- 
  test_phrase("a112", proc_name).
test_proc_name(["a112 not parsed by proc_name"]).

test_declarator([]) :- 
  test_phrase("locala112", declarator).
test_declarator(["locala112 not parsed by delarator"]).

test_all([]).
test_all([H | T]) :- 
  call(H, E), (E = [] ; print(E)), 
  test_all(T).

:- test_all([
  test_identifier,
  test_digit,
  test_digits,
  test_variable,
  test_variables,
  test_formal_arg,
  test_proc_name,
  test_declarator,
  test_formal_arg_str,
  test_formal_args
]).
