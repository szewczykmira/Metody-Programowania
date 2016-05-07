% vim: syntax=prolog

% white spaces
smt --> [_], smt.
smt --> [].
comment --> "(*", smt, "*)",!.
white --> [C],{code_type(C, space) },!, white.
white --> [].
white_space --> [C], {code_type(C,space)},white,!.
white_space --> comment.
white_or_blank --> white_space, !.
white_or_blank --> [].

% relational operators
rel_op("<=") --> "<=",!.
rel_op(">=") --> ">=",!.
rel_op("<>") --> "<>",!.
rel_op("<") --> "<",!.
rel_op(">") --> ">",!.
rel_op("=") --> "=".

% multiplikative operators
mul_op("*") --> "*",!.
mul_op("div") --> "div",!.
mul_op("mod") --> "mod",!.

% additive operators
add_op("+") --> "+",!.
add_op("-") --> "-",!.

% digits and numbers
digit(D) --> [D], {code_type(D,digit)}.
digits_acc([H|T]) --> digit(H), !, digits_acc(T).
digits_acc([]) --> [].
digits([H|T]) --> digit(H), digits_acc(T).
number(D) --> digits(Ds), {number_chars(D,Ds)}.

% letters and words
letter(L) --> [L], {code_type(L, alpha)}.

% identifier
special_char(X) --> "_",!,{atom_codes("_",[X])}.
special_char(X) --> "'", {atom_codes("'", [X])}.
ident([L|T]) --> letter(L), !, ident(T).
ident([C|T]) --> special_char(C), !, ident(T).
ident([D|T]) --> digit(D), !, ident(T).
ident([]) --> [].
identifier(A) --> letter(L), ident(I), {atom_codes(A,[L|I])}.

% variable
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
formal_args(A) --> formal_arg_str(A),!.
formal_args([]) --> [].

% real argument
real_arg(A) --> arithmetic_expr(A).

% real arguments string
real_args_str([H|T]) --> real_arg(H), white_or_blank, ",", white_space, !, real_args_str(T).
real_args_str([H]) --> real_arg(H).

% real arguments
real_args(A) --> real_args_str(A), !.
real_args([]) --> [].

% procedure name
proc_name(A) --> identifier(A).

% procedure
procedure(procedure(A, B, C)) --> "procedure", white_space, proc_name(A), white_or_blank, "(", white_or_blank, formal_args(B), white_or_blank, ")", white_space, block(C).

% procedure call
procedure_call(p_call(A, T)) --> proc_name(A), "(", white_or_blank, real_args(T), white_or_blank, ")".

% atom expression
atom_expr(A) --> procedure_call(A), !.
atom_expr(variable(A)) --> variable(A), !.
atom_expr(number(A)) --> number(A).

% simple expression
simple_expr(A) --> "(", white_or_blank,  arithmetic_expr(A), white_or_blank, ")", !.
simple_expr(A) --> atom_expr(A).

% factor (czynnik)
factor(-(A)) --> "-", simple_expr(A),!.
factor(+(A)) --> simple_expr(A).

% indigrient (skladnik)
indigrient(op(A, F, I)) --> factor(F), white_or_blank, mul_op(A), white_or_blank,!, indigrient(I).
indigrient(F) --> factor(F).

% arithmetic expression
arithmetic_expr(op(O,I,A)) --> indigrient(I), white_or_blank, add_op(O), white_or_blank, !,  arithmetic_expr(A).
arithmetic_expr(A) --> indigrient(A).

% instruction
instruction(iwrite(A)) --> "write", !, white_space, arithmetic_expr(A).
instruction(iread(V)) --> "read", !, white_space,  variable(V).
instruction(ireturn(A)) --> "return", !, white_space, arithmetic_expr(A).
instruction(icall(P)) --> "call", !, white_space, procedure_call(P).
instruction(while(Le, Ci)) --> "while", !, white_space,  logical_expr(Le), white_space, "do", white_space, compound_instruction(Ci), white_space, "done".
instruction(ifelse(Le, I, E)) --> "if", white_space, logical_expr(Le), white_space, "then", white_space, compound_instruction(I), white_space, "else", !, white_space, compound_instruction(E), white_space, "fi".
instruction(if(Le, I)) --> "if", !, white_space, logical_expr(Le), white_space, "then", white_space, compound_instruction(I), white_space, "fi".
instruction(assign(V,W)) --> variable(V), white_or_blank, ":=", white_or_blank, arithmetic_expr(W).

% compound instruction
compound_instruction([H|T]) --> instruction(H), white_or_blank, ";", white_space, !, compound_instruction(T).
compound_instruction([H]) --> instruction(H).

% relational expression
rel_expr(A) --> "(", !, white_or_blank, logical_expr(A),white_or_blank, ")".
rel_expr(op(O,A,B)) --> arithmetic_expr(A), white_or_blank, rel_op(O), white_or_blank, arithmetic_expr(B).

% condition (warunek)
condition(not(A)) --> "not", white_space, !, rel_expr(A).
condition(A) --> rel_expr(A).

% conjunction
conjunction(and([H|T])) --> condition(H), white_space, "and", !, white_space, conjunction(and(T)).
conjunction(and([H])) --> condition(H).

% logical expression
logical_expr(or([H|T])) --> conjunction(H), white_space, "or", !, white_space, logical_expr(or(T)).
logical_expr(or([H])) --> conjunction(H).

% declarator
declarator(local(A)) --> "local", white_space, variables(A).

% declaration
declaration(A) --> declarator(A).
declaration(A) --> procedure(A).

% declarations
declarations_acc([H|T]) --> declaration(H), white_space, declarations_acc(T).
declarations_acc([H]) --> declaration(H).
declarations(declarations(A)) --> declarations_acc(A), !.
declarations(declarations([])) --> [].

% block
block(block(A,I)) --> declarations(A), white_space, !, "begin", white_space, compound_instruction(I), white_space, "end".
block(block([], I)) --> "begin", white_space, compound_instruction(I), white_space, "end".

% program
program(program(Id, B)) --> "program", white_space, identifier(Id), white_space, block(B).

% ===== INTERPRETER ==========

parsing(String) :-
  atom_codes(String, List),
  phrase(program(Expr), List),
  interpret(Expr, [], _).

reify(Proc, Val) :- (Proc, !, Val=true) ; (Val=false).

parse_args([], [], EnvIn, [eof | EnvIn]).
parse_args([value(Fh)|Ft], [Rh|Rt], EnvIn, EnvOut) :-
  parse_args(Ft, Rt, EnvIn, EnvOut1), !,
  eval(Rh, EnvOut1, EnvOut2, Val),
  append([(value, Fh, Val)], EnvOut2, EnvOut).
parse_args([name(_)|Ft], [_|Rt], EnvIn, EnvOut) :-
  parse_args(Ft, Rt, EnvIn, EnvOut), !,
  print("Not handling call-by-name"),
  abort.

retract_local_args([eof | EnvOut], EnvOut) :- !.
retract_local_args([_ | EnvOut1], EnvOut) :-
  retract_local_args(EnvOut1, EnvOut).

:- dynamic(returns/2).
eval(p_call(Id, Ra), EnvIn, RealEnvOut, Val) :-
  member((procedure, Id, Fa, B), EnvIn),
  parse_args(Fa, Ra, EnvIn, EnvOut1),
  ( (interpret(B, EnvOut1, EnvOut), Val = 0)
  ; (returns(EnvOut, Val), !, retractall(returns(_, _)))),!,
  retract_local_args(EnvOut, RealEnvOut).

eval(number(Arg), B, B, Arg).

eval(variable(Var), Env, Env, Arg) :-
  !, member((_,Var,Arg), Env).

eval(-(Arg), EnvIn, EnvOut, Val) :-
  eval(Arg, EnvIn, EnvOut, Val1),
  Val is (-1)*Val1.

eval(+(Arg), Env, EnvOut, Val) :-
  eval(Arg, Env, EnvOut, Val).

eval(op("*", Var1, Var2), Env, EnvOut, Val) :-
  eval(Var1, Env, EnvOut1, Val1),
  eval(Var2, EnvOut1, EnvOut, Val2), !,
  Val is Val1 * Val2.

eval(op("div", Var1, Var2), EnvIn, EnvOut, Val) :-
  eval(Var1, EnvIn, EnvOut1, Val1),
  eval(Var2, EnvOut1, EnvOut, Val2), !,
  Val is Val1 div Val2.

eval(op("mod", Var1, Var2), EnvIn, EnvOut, Val) :-
  eval(Var1, EnvIn, EnvOut1, Val1), !,
  eval(Var2, EnvOut1, EnvOut, Val2),
  Val is Val1 mod Val2.

eval(op("+", Var1, Var2), EnvIn, EnvOut, Val) :-
  !, eval(Var1, EnvIn, EnvOut1, Val1),
  eval(Var2, EnvOut1, EnvOut, Val2),
  Val is Val1 + Val2.

eval(op("-", Var1, Var2), EnvIn, EnvOut, Val) :-
  !, eval(Var1, EnvIn, EnvOut1, Val1),
  eval(Var2, EnvOut1, EnvOut, Val2),
  Val is Val1 - Val2.

eval(not(A), EnvIn, EnvOut, Val) :-
  eval(A, EnvIn, EnvOut, B), !, reify(\+ B, Val).

eval(op("<=", Var1, Var2), EnvIn, EnvOut, X) :-
  eval(Var1, EnvIn, EnvOut1, Val1),
  eval(Var2, EnvOut1, EnvOut, Val2),
  reify(Val1 =< Val2, X), !.

eval(op(">=", Var1, Var2), EnvIn, EnvOut, X) :-
  eval(Var1, EnvIn, EnvOut1, Val1),
  eval(Var2, EnvOut1, EnvOut, Val2),
  reify(Val1 >= Val2, X), !.

eval(op("<", Var1, Var2), EnvIn, EnvOut, X) :-
  eval(Var1, EnvIn, EnvOut1, Val1),
  eval(Var2, EnvOut1, EnvOut, Val2),
  reify(Val1 < Val2, X), !.

eval(op(">", Var1, Var2), EnvIn, EnvOut, X) :-
  eval(Var1, EnvIn, EnvOut1, Val1),
  eval(Var2, EnvOut1, EnvOut, Val2),
  reify(Val1 > Val2, X), !.

eval(op("<>", Var1, Var2), EnvIn, EnvOut, X) :-
  eval(Var1, EnvIn, EnvOut1, Val1),
  eval(Var2, EnvOut1, EnvOut, Val2),
  reify(Val1 \= Val2, X),!.


eval(or([]), Env, Env, false) :- !.
eval(or([H|_]), EnvIn, EnvOut, true) :-
  eval(H, EnvIn, EnvOut, Y),
  Y, !.
eval(or([_|T]), EnvIn, EnvOut, X) :-
  eval(or(T), EnvIn, EnvOut, X).

eval(and([]), Env, Env, true) :- !.
eval(and([H|_]), EnvIn, EnvOut, false) :-
  eval(H, EnvIn, EnvOut, Y),
  \+ Y, !.
eval(and([_|T]), EnvIn, EnvOut, X) :-
  eval(and(T), EnvIn, EnvOut, X).

puts(_, _, [], _) :- print("Not in scope."), abort.
puts(Arg, X, [(I, Arg, _) | T], [(I, Arg, X) |T]) :- !.
puts(Arg, X, [H|T], [H|EnvOut]) :-
  puts(Arg, X, T, EnvOut).

interpret(iwrite(Arg), EnvIn, EnvOut) :-
  eval(Arg, EnvIn, EnvOut, Val), nl,
  print(Val).

interpret(iread(Arg), EnvIn, EnvOut) :-
  read(X),
  puts(Arg, X, EnvIn, EnvOut).

interpret(ireturn(Arg), EnvIn, _) :-
  eval(Arg, EnvIn, EnvOut, Val),
  assert(returns(EnvOut, Val)),
  fail.

interpret(icall(A), EnvIn, EnvOut) :-
  eval(A, EnvIn, EnvOut, _).

interpret(while(Logic, Compound), EnvIn, EnvOut) :-
  eval(Logic, EnvIn, EnvOut1, Val),
  Val,
  interpret(Compound, EnvOut1, EnvOut2),
  interpret(while(Logic, Compound), EnvOut2, EnvOut).
interpret(while(_,_), A, A).

interpret(ifelse(Logic,If,_), EnvIn, EnvOut) :-
  eval(Logic, EnvIn, EnvOut1, Val),
  Val, !,
  interpret(If, EnvOut1, EnvOut).
interpret(ifelse(_, _, Else), EnvIn, EnvOut) :-
  interpret(Else, EnvIn, EnvOut).

interpret(if(Logic,If), EnvIn, EnvOut) :-
  eval(Logic, EnvIn, EnvOut1, Val),
  Val, !,
  interpret(If, EnvOut1, EnvOut).
interpret(if(_, _), A, A).

interpret(assign(Var, Val), EnvIn, EnvOut) :-
  eval(Val, EnvIn, EnvOut1, Val1),
  puts(Var, Val1, EnvOut1, EnvOut).

interpret([], EnvIn, EnvIn):-!.
interpret([H|T], EnvIn, EnvOut) :-
  interpret(H, EnvIn, EnvOut1),
  interpret(T, EnvOut1, EnvOut).

interpret(declarations([]), EnvIn, EnvIn) :-!.
interpret(declarations([H|T]), EnvIn, EnvOut) :-
  interpret(H, EnvIn, EnvOut1),!,
  interpret(declarations(T), EnvOut1, EnvOut).

interpret(local([]), EnvIn, EnvIn):-!.
interpret(local([H|T]), EnvIn, EnvOut):-
  interpret(local(T), [(local, H, 0) | EnvIn], EnvOut).

interpret(procedure(Id, FA, B), EnvIn, [(procedure, Id, FA, B) | EnvIn]).

interpret(block(Dec, Ci), EnvIn, EnvOut) :-
  interpret(Dec, EnvIn, EnvOut1),
  interpret(Ci, EnvOut1, EnvOut).

interpret(program(_, B), EnvIn, EnvOut) :-
  interpret(B, EnvIn, EnvOut).

interpreter(Program) :- interpret(Program, [], _).

% =========== COMPILER ===============
jump(Future, (ACC, AR, DR, MEM), History, Jump) :- 
  reverse(History, RHistory), 
  append(RHistory, Future, All), 
  skip(Jump, All,[], NHistory, NFuture),
  asm(NFuture, (ACC, AR, DR, MEM), NHistory).

skip(0, P, H, H, P) :- !.
skip(N, [H|T], Acc, History, Future) :- 
  N1 is N - 1, 
  skip(N1, T, [H | Acc], History, Future).

replace(Index, NVal, [(Index, _) | List], [(Index, NVal) | List]).
replace(Index, NVal, [H | T], [H | Result]) :-
  replace(Index, NVal, T, Result).

asm([], _, _).

% NOP -- do nothing
asm([nop | T], (ACC, AR, DR, MEM), History) :-
  !, asm(T, (ACC, AR, DR, MEM), [nop | History]).

% SYSCALL (syscall(ACC))
asm([syscall | _], (0, _, _, _), _) :- 
  !, abort.
asm([syscall | T], (1, AR, DR, MEM), History) :- 
  !, read(ACC), 
  asm(T, (ACC, AR, DR, MEM), [syscall | History]).
asm([syscall | T], (2, AR, DR, MEM), History) :- 
  !, write(DR), 
  asm(T, (2, AR, DR, MEM), [syscall | History]).

% LOAD (MEM[AR] -> ACC)
asm([load | T], (_, AR, DR, MEM), History) :- 
  !, member((AR, Val), MEM), 
  asm(T, (Val, AR, DR, MEM), [load |History]).

% STORE (ACC -> MEM[AR])
asm([store | T], (ACC, AR, DR, MEM), History) :- 
  \+ member((AR,_), MEM),
  !, asm(T, (ACC, AR, DR, [(AR, ACC) | MEM]), [store | History]).
asm([store | T], (ACC, AR, DR, MEM), History) :-
  !, replace(AR, ACC, MEM, Result),
  asm(T, (ACC, AR, DR, Result), [store | History]).

% SWAPA (ACC <-> AR)
asm([swapa | T], (ACC, AR, DR, MEM), History) :- 
  !, asm(T, (AR, ACC, DR, MEM), [swapa | History]).

% SWAPD (ACC <-> DR)
asm([swapd | T], (ACC, AR, DR, MEM), History) :- 
  !, asm(T, (DR, AR, ACC, MEM), [swapd | History]).

% BRANCHZ (if ACC = 0 then AR -> PC)
asm([branchz | T], (ACC, AR, DR, MEM), History) :- 
  ACC = 0, !, 
  jump([branchz | T], (ACC, AR, DR, MEM), History, AR).
asm([branchz | T], (ACC, AR, DR, MEM), History) :- 
  asm(T, (ACC, AR, DR, MEM), [branchz | History]).

% BRANCHN (if ACC < 0 then  AR -> PC)
asm([branchn | T], (ACC, AR, DR, MEM), History) :- 
  ACC < 0, !, 
  jump([branchn | T], (ACC, AR, DR, MEM), History, AR).
asm([branchn | T], (ACC, AR, DR, MEM), History) :- 
  asm(T, (ACC, AR, DR, MEM), [branchn | History]).

% JUMP (ACC -> PC)
asm([jump | T], (ACC, AR, DR, MEM), History) :- 
  !, jump([jump|T], (ACC, AR, DR, MEM), History, ACC).

% CONST (MEM[PC++] -> ACC)
asm([const, N | T], (_, AR, DR, MEM), History) :- 
  !, asm(T, (N, AR, DR, MEM), [const | History]).

% ADD (ACC + DR -> ACC)
asm([add | T], (ACC, AR, DR, MEM), History) :- 
  !, ACC1 is ACC + DR, 
  asm(T, (ACC1, AR, DR, MEM), [add | History]).

% SUB (ACC - DR -> ACC)
asm([sub | T], (ACC, AR, DR, MEM), History) :- 
  !, ACC1 is ACC - DR, 
  asm(T, (ACC1, AR, DR, MEM), [sub | History]).

% MUL (ACC x DR -> ACC)
asm([mul | T], (ACC, AR, DR, MEM), History) :- 
  !, ACC1 is ACC * DR, 
  asm(T, (ACC1, AR, DR, MEM), [mul | History]).

% DIV (ACC/DR -> ACC)
asm([div | T], (ACC, AR, DR, MEM), History) :- 
  !, ACC1 is ACC div DR, 
  asm(T, (ACC1, AR, DR, MEM), [div | History]).


% We are assuming that every compilation is finishing in ACC

increase_stack([const, 0, swapa, load, swapd, const, 1, add, swapd, const, 0, swapa, swapd, store]).
decrease_stack([const, 0, swapa, load, swapd, const, -1, add, swapd, const, 0, swapa, swapd, store]).
save_acc_to_stack([swapd, const, 0, swapa, load, swapa, swapd, store]).


compile(number(Arg), [const, Arg]).

compile(variable(Var), [const, Var, swapa, load]).

compile(-(Arg), Commands) :- 
  compile(Arg, C1), 
  append(C1, [swapd, const, -1, mul], Commands).

compile(+(Arg), Commands) :- 
  compile(Arg, Commands). 

compile(op("*", E1, E2), Commands) :- 
  compile(E1, C1),
  save_acc_to_stack(Stack),
  increase_stack(Incr),
  compile(E2, C2),
  % save_acc_to_stack
  decrease_stack(Decr),
  X = [const, 0, swapa, load, swapd, const, 1,
  add, swapa, load, swapd], % take C2 = stack + 1
  Y = [const, 0, swapa, load,swapa, load, mul], % take C1 = stack,
  append(C1, Stack, S1),
  append(Incr, C2, S2),
  append(Stack, Decr, S3),
  append(X, Y, S4),
  append(S1, S2, U1),
  append(S3, S4, U2),
  append(U1, U2, Commands).

compile(op("div", E1, E2), Commands) :- 
  compile(E1, C1),
  save_acc_to_stack(Stack),
  increase_stack(Incr),
  compile(E2, C2),
  % save_acc_to_stack
  decrease_stack(Decr),
  X = [const, 0, swapa, load, swapd, const, 1,
  add, swapa, load, swapd], % take C2 = stack + 1
  Y = [const, 0, swapa, load,swapa, load, div], % take C1 = stack,
  append(C1, Stack, S1),
  append(Incr, C2, S2),
  append(Stack, Decr, S3),
  append(X, Y, S4),
  append(S1, S2, U1),
  append(S3, S4, U2),
  append(U1, U2, Commands).

compile(op("mod", E1, E2), Commands) :-
  compile(E1, C1),
  save_acc_to_stack(Stack),
  increase_stack(Incr),
  compile(E2, C2),
  % save_acc_to_stack
  % get DIV = C1 div C2 to acc
  DIV = [const, 0, swapa, load, swapa, load, swapa, swapd, const, -1, add, swapa, swapd, load, div],
  decrease_stack(Decr),
  % get MUL = DIV * C2
  MUL = [mul, swapd, const, 0, swapa, load, swapa, swapd, store | Decr],
  % get C1 - MUL.
  SUB = [const, 0, swapa, load, swapd, const, 1, add, swapa, load, swapd, const, 0, swapa, load, swapa, load, sub],
  append(C1, Stack,S1),
  append(Incr, C2, S2),
  append(Stack, DIV, S3),
  append(MUL, SUB, S4),
  append(S1, S2, U1),
  append(S3, S4, U2),
  append(U1, U2, Commands).

compile(op("+", E1, E2), Commands) :- 
  compile(E1, C1),
  save_acc_to_stack(Stack),
  increase_stack(Incr),
  compile(E2, C2),
  % save_acc_to_stack
  decrease_stack(Decr),
  X = [const, 0, swapa, load, swapd, const, 1,
  add, swapa, load, swapd], % take C2 = stack + 1
  Y = [const, 0, swapa, load,swapa, load, add], % take C1 = stack,
  append(C1, Stack, S1),
  append(Incr, C2, S2),
  append(Stack, Decr, S3),
  append(X, Y, S4),
  append(S1, S2, U1),
  append(S3, S4, U2),
  append(U1, U2, Commands).

compile(op("-", E1, E2), Commands) :- 
  compile(E1, C1),
  save_acc_to_stack(Stack),
  increase_stack(Incr),
  compile(E2, C2),
  % save_acc_to_stack
  decrease_stack(Decr),
  X = [const, 0, swapa, load, swapd, const, 1,
  add, swapa, load, swapd], % take C2 = stack + 1
  Y = [const, 0, swapa, load,swapa, load, sub], % take C1 = stack,
  append(C1, Stack, S1),
  append(Incr, C2, S2),
  append(Stack, Decr, S3),
  append(X, Y, S4),
  append(S1, S2, U1),
  append(S3, S4, U2),
  append(U1, U2, Commands).

%eval(not(A), EnvIn, EnvOut, Val) :-

%eval(op("<=", Var1, Var2), EnvIn, EnvOut, X) :-

%eval(op(">=", Var1, Var2), EnvIn, EnvOut, X) :-

%eval(op("<", Var1, Var2), EnvIn, EnvOut, X) :-

%eval(op(">", Var1, Var2), EnvIn, EnvOut, X) :-

%eval(op("<>", Var1, Var2), EnvIn, EnvOut, X) :-

%eval(or([H|_]), EnvIn, EnvOut, true) 

%eval(and([]), Env, Env, true) :- !.

compile(iwrite(E), Commands) :-
  compile(E, C),
  append(C, [swapd, const, 2, syscall], Commands).

compile(iread(E), [const, E, swapa, const, 1, syscall, store]).

%interpret(ireturn(Arg), EnvIn, _) :-

%interpret(icall(A), EnvIn, EnvOut) :-

%interpret(while(Logic, Compound), EnvIn, EnvOut) :-

%interpret(ifelse(Logic,If,_), EnvIn, EnvOut) :-

%interpret(if(Logic,If), EnvIn, EnvOut) :-

compile(assign(Var, Val), [const, Var, swapa, const, Val, store]).

compile([], []).
compile([H|T], Commands) :-
  compile(H, HCommands),
  compile(T, TCommands),
  append(HCommands, TCommands, Commands).

%interpret(declarations([H|T]), EnvIn, EnvOut) :-

%interpret(local([]), EnvIn, EnvIn):-!.

%interpret(procedure(Id, FA, B), EnvIn, [(procedure, Id, FA, B) | EnvIn]).

%interpret(block(Dec, Ci), EnvIn, EnvOut) :-

%interpret(program(_, B), EnvIn, EnvOut) :-


program(Ast, [const, 0, swapa, const, 1, store | Compiled]) :- 
  compile(Ast, Compiled).
