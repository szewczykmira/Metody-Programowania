% vim: syntax=prolog
% first of all we need to define parser for algol16

% white spaces
white --> [C], {code_type(C, space) },!, white.
white --> [].
white_space --> [C], {code_type(C,space)},white.
white_or_blank --> white_space, !.
white_or_blank --> [].

% relational operators
rel_op --> "<=",!.
rel_op --> ">=",!.
rel_op --> "<>",!.
rel_op --> "<",!.
rel_op --> ">",!.
rel_op --> "=".

% multiplikative operators
mul_op --> "*",!.
mul_op --> "div",!.
mul_op --> "mod",!.

% additive operators
add_op --> "+",!.
add_op --> "-",!.

% digits and numbers
digit(D) --> [D], {code_type(D,digit)}.
digits_acc([H|T]) --> digit(H), !, digits_acc(T).
digits_acc([]) --> [].
digits([H|T]) --> digit(H), digits_acc(T).
number(D) --> digits(Ds), {number_chars(D,Ds)}.

% letters and words
letter(L) --> [L], {code_type(L, alpha)}.

% identifier
% ident([H|T]) --> [H], {code_type(H, csym); H=39},!, ident(T).
% ident([]) --> [].
% identifier(L, Id) --> ident(As),{atom_codes(Id, [L|As])}.
special_char(X) --> "_",!,{atom_codes("_",[X])}.
special_char(X) --> "'", {atom_codes("'", [X])}.
ident([L|T]) --> letter(L), !, ident(T).
ident([C|T]) --> special_char(C), !, ident(T).
ident([D|T]) --> digit(D), !, ident(T).
ident([]) --> [].
identifier(A) --> letter(L), ident(I), {atom_codes(A,[L|I])}.

% variable
% variable(L,Id) --> identifier(L,Id).
variable(A) --> identifier(A).
variables([H|T]) --> variable(H), white_or_blank, ",", white_space, !, variables(T).
variables([A]) --> variable(A).

% formal argument
formal_arg(value(A)) --> "value", white_space, !, variable(A).
formal_arg(name(A)) --> variable(A).

% string of formal arguments
formal_arg_str([H|T]) --> formal_arg(H), white_or_blank, ",", white_space, !, formal_arg_str(T).
formal_arg_str([H]) --> formal_arg(H).

% formal arguments
formal_args --> formal_arg_str,!.
formal_args --> [].

% real argument
real_arg --> arithmetic_expr.

% real arguments string
real_args_str --> real_arg, white_or_blank, ",", white_space, !, real_args_str.
real_args_str --> real_arg.

% real arguments
real_args --> real_args_str, !.
real_args --> [].

% procedure name
proc_name --> identifier.

% TO TEST!
% procedure
procedure --> "procedure", white_space, proc_name, "(", white_or_blank, formal_args, white_or_blank, ")", white_space, block.

% procedure call
procedure_call --> proc_name, "(", white_or_blank, real_args, white_or_blank, ")".

% atom expression
atom_expr --> procedure_call, !.
atom_expr --> variable, !.
atom_expr --> number.

% simple expression
simple_expr --> "(", white_or_blank,  arithmetic_expr, white_or_blank, ")", !.
simple_expr --> atom_expr.

% factor (czynnik)
factor --> "-", simple_expr,!.
factor --> simple_expr.

% indigrient (skladnik)
indigrient --> factor, white_or_blank, mul_op, white_or_blank,!, indigrient.
indigrient --> factor.

% arithmetic expression
arithmetic_expr --> indigrient, white_or_blank, add_op, white_or_blank, !,  arithmetic_expr.
arithmetic_expr --> indigrient.

% instruction
instruction --> "write", !, white_space, arithmetic_expr.
instruction --> "read", !, white_space,  variable.
instruction --> "return", !, white_space, arithmetic_expr.
instruction --> "call", !, white_space, procedure_call.
instruction --> "while", !, white_space,  logical_expr, white_space, "do", white_space, compound_instruction, white_space, "done".
instruction --> "if", white_space, logical_expr, white_space, "then", white_space, compound_instruction, white_space, "else", !, white_space, compound_instruction, white_space, "fi".
instruction --> "if", !, white_space, logical_expr, white_space, "then", white_space, compound_instruction, white_space, "fi".
instruction --> variable, white_or_blank, ":=", white_or_blank, arithmetic_expr.

% compound instruction
compound_instruction --> instruction, white_or_blank, ";", white_space, !, compound_instruction.
compound_instruction --> instruction.

% relational expression
rel_expr --> "(", !, white_or_blank, logical_expr,white_or_blank, ")".
rel_expr --> arithmetic_expr, white_or_blank, rel_op, white_or_blank, arithmetic_expr.

% condition (warunek)
condition --> "not", white_space, !, rel_expr.
condition --> rel_expr.

% conjunction
conjunction --> condition, white_space, "and", !, white_space, conjunction.
conjunction --> condition.

% logical expression
logical_expr --> conjunction, white_space, "or", !, white_space, logical_expr.
logical_expr --> conjunction.

% declarator
declarator --> "local", white_space, variables.

% declaration
declaration --> declarator.
declaration --> procedure.

% declarations
declarations_acc --> declaration, white_space, declarations_acc.
declarations_acc --> declaration.
declarations --> declarations_acc, !.
declarations --> [].

% block
block --> declarations, white_space, "begin", white_space, compound_instruction, white_space, "end".

% program
program --> "program", white_space, identifier, white_space, block.

% Testing
test_phrase(String, Pred) :- 
  atom_codes(String, Codes), phrase(Pred, Codes).

test_identifier([]) :- 
  test_phrase("a111", identifier("a111")).
test_identifier(["a111 not parsed by identifer"]).

test_digit([]) :- 
  test_phrase("9", digit(57)).
test_digit(["9 not parsed by digit"]).

test_digits([]) :- 
  test_phrase("987", digits([57,56,55])).
test_digits(["987 not parsed by digits"]).

test_variable([]) :- 
  test_phrase("a112", variable("a112")).
test_variable(["a112 not parsed by variable"]).

test_variables([]) :- 
  test_phrase("a112", variables(["a112"])), 
  test_phrase("a112, aa", variables(["a112", "aa"])).
test_variables(["a112 or a112,aa not parsed by variables"]).

test_formal_arg([]) :- 
  test_phrase("a112", formal_arg(name("a112"))), 
  test_phrase("value a112", formal_arg(value("a112"))).
test_formal_arg(["a112 or valuea112 not parsed by formal_arg"]).

test_formal_arg_str([]) :- 
  test_phrase("aqwer", formal_arg_str([name("aqwer")])), 
  test_phrase("awer, wet3", formal_arg_str([name("awer"), name("wet3")])), 
  test_phrase("awe, value we2", formal_arg_str([name("awe"), value("we2")])).
test_formal_arg_str(["aqwer, <<awer,wet3>> and <<awe,valuewe2>> not parsed by formal_arg_str"]).

test_formal_args([]) :- 
  test_phrase("",formal_args),
  test_phrase("awe, value we2", formal_args).
test_formal_args(["empty and awe,valuewe2 not parsed by formal_args"]).

test_proc_name([]) :- 
  test_phrase("a112", proc_name).
test_proc_name(["a112 not parsed by proc_name"]).

test_declarator([]) :- 
  test_phrase("local a112", declarator).
test_declarator(["locala112 not parsed by delarator"]).

test_atom_expr([]) :- 
  test_phrase("45", atom_expr),
  test_phrase("a45", atom_expr).
test_atom_expr(["45 and a45 not parsed by atom expression"]).

test_simple_expr([]) :-
  test_phrase("a45", simple_expr),
  test_phrase("( -45*4+5)", simple_expr).
test_simple_expr(["a45 or (-45*4+5) not parsed by simple expression"]).

test_factor([]) :-
  test_phrase("a45", factor),
  test_phrase("-45", factor).
test_factor(["a45 or -45 not parsed by factor"]).

test_indigirient([]) :-
  test_phrase("-45", indigrient),
  test_phrase("-45 div 4", indigrient).
test_indigirient(["-45 or -45div4 not parsed by indigrient"]).

test_arithmetic_expr([]) :-
  test_phrase("-45*4", arithmetic_expr),
  test_phrase("-45 *4+ 5", arithmetic_expr).
test_arithmetic_expr(["-45*4 or -45*4+5 not parsed by arithmetic_expr"]).

test_real_arg([]) :-
  test_phrase("-45*4+5", real_arg).
test_real_arg(["-45*4+5 not parsed by real_arg"]).

test_real_args_str([]) :-
  test_phrase("-45*4+5, 45, 56+6", real_args_str),
  test_phrase("-45 *4 + 6", real_args_str).
test_real_args_str(["-45*4+5,45,56+6 or -45*4+6 not parsed by real_args_str"]).

test_real_args([]) :-
  test_phrase("-45*4, 45, 56+6", real_args),
  test_phrase("", real_args).
test_real_args(["-45*4,45,56+6 or empty not parsed by real_args"]).

test_procedure_call([]) :-
  test_phrase("ea23( 45, 5*6)", procedure_call).
test_procedure_call(["ea23(45,5*6) not parsed by procedure_call"]).

test_instruction([]) :-
  test_phrase("write 45 * 5* 6", instruction),
  test_phrase("read b45", instruction),
  test_phrase("return 45* 5*6", instruction),
  test_phrase("call a23( 45, 5*6)", instruction),
  test_phrase("while not45>5 do read r4 done", instruction),
  test_phrase("if not34>4 then read r4 else write 3* 4 fi", instruction),
  test_phrase("if not 34>4 then read r4 fi", instruction),
  test_phrase("awe := 3*4", instruction).
test_instruction(["<<write45*5*6>> or <<readb45>> or <<return45*5*6>> or ... not parsed by instruction"]).

test_compound_instruction([]) :-
  test_phrase("call ea23(45, 5*6); write 45", compound_instruction).

test_rel_expr([]) :-
  test_phrase("45*5 <> 5", rel_expr),
  test_phrase("(not 45 <> 5 and 45 > 6 or 45<> 7)", rel_expr).
test_rel_expr(["45*5<>5 not parsed by rel_expr"]).

test_condition([]) :-
  test_phrase("45* 5 <> 5", condition),
  test_phrase("not 45 <> 5", condition).
test_condition(["45*5<>5 or not45<>5 not parsed by condition"]).

test_conjunction([]) :-
  test_phrase("not 45 <>5 and 45 >6", conjunction).
test_conjunction(["not45<>5and45>6 not parsed by conjunction"]).

test_logical_expr([]) :-
  test_phrase("not 45 <>5 and 45> 6 or not 5 > 4", logical_expr).
test_logical_expr(["not45<>5and45>6ornot5>4 not parsed by logical_expr"]).

test_declaration([]) :-
  test_phrase("local w23, aaa, e", declaration).
test_declaration(["localw23,aaa,e not parsed by declaration"]).

test_declarations([]) :-
  test_phrase("local we, aaa, e", declarations).
test_declarations(["C not parsed by declarations"]).

test_block([]) :-
  test_phrase("local qwe, awe begin write 3*4 end",block).
test_block(["XYX not parsed by block"]).

test_procedure([]) :-
  test_phrase("procedure qwer( awe, valuewe) local w23, aaa, e begin write 3*4 end",procedure).
test_procedure(["X not parsed by procedure"]).

test_program([]) :-
  test_phrase("program qwer local w23, aaa, e begin write 3*4 end", program).
test_program(["X not parsed by program"]).

test_all([]).
test_all([H | T]) :- 
  call(H, E), (E = [] ; print(E)), 
  test_all(T).

:- test_all([
  test_identifier
  ,test_digit
  ,test_digits
  ,test_variable
  ,test_variables
  ,test_formal_arg
  %,test_proc_name
  %,test_declarator
  ,test_formal_arg_str
  %,test_formal_args
  %,test_atom_expr
  %,test_simple_expr
  %,test_factor
  %,test_indigirient
  %,test_arithmetic_expr
  %,test_real_arg
  %,test_real_args_str
  %,test_real_args
  %,test_procedure_call
  %,test_instruction
  %,test_compound_instruction
  %,test_rel_expr
  %,test_condition
  %,test_conjunction
  %,test_logical_expr
  %,test_declaration
  %,test_block
  %,test_declarations
  %,test_procedure
  %,test_program
]).
