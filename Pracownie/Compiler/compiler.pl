% vim: syntax=prolog
% Miroslawa Szewczyk, 241752
% Nie zostalo zaimplementowane zagniezdzanie procedur oraz przekazywanie parametrow do procedur przez nazwe

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
block(block(declarations([]), I)) --> "begin", white_space, compound_instruction(I), white_space, "end".

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
  write("Not handling call-by-name"), nl,
  abort.

retract_local_args([eof | EnvOut], EnvOut) :- !.
retract_local_args([_ | EnvOut1], EnvOut) :-
  retract_local_args(EnvOut1, EnvOut).

:- dynamic(returns/2).
eval(p_call(Id, Ra), EnvIn, RealEnvOut, Val) :-
  member((procedure, Id, Fa, B), EnvIn), !,
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

skip(0, P, H, H, P):- !.
skip(N, [H|T], Acc, History, Future) :-
  N1 is N - 1,
  skip(N1, T, [H | Acc], History, Future).

replace(Index, NVal, [(Index, _) | List], [(Index, NVal) | List]) :- !.
replace(Index, NVal, [H | T], [H | Result]) :-
  replace(Index, NVal, T, Result).

asm([], (A, _,_, _), _) :- write(A), !.

% NOP -- do nothing
asm([nop | T], (ACC, AR, DR, MEM), History) :-
  !, asm(T, (ACC, AR, DR, MEM), [nop | History]).

asm([next(Q) | T], (ACC, AR, DR, MEM), History) :-
  !, write(Q),nl,
  asm(T, (ACC, AR, DR, MEM), [next(Q) |History]).

% SYSCALL (syscall(ACC))
asm([syscall | _], (0, _, _, _), _) :-
  !, abort.
asm([syscall | T], (1, AR, DR, MEM), History) :-
  !, read(ACC),
  asm(T, (ACC, AR, DR, MEM), [syscall | History]).
asm([syscall | T], (2, AR, DR, MEM), History) :-
  !, write(DR),nl,
  asm(T, (2, AR, DR, MEM), [syscall | History]).

% LOAD (MEM[AR] -> ACC)
asm([load | T], (_, AR, DR, MEM), History) :-
  !,
  %print(memory(MEM)), nl,
  member((AR, Val), MEM), !,
  asm(T, (Val, AR, DR, MEM), [load |History]).

% STORE (ACC -> MEM[AR])
asm([store | T], (ACC, AR, DR, MEM), History) :-
  \+ member((AR,_), MEM),
  !, asm(T, (ACC, AR, DR, [(AR, ACC) | MEM]), [store | History]).
asm([store | T], (ACC, AR, DR, MEM), History) :-
  !, replace(AR, ACC, MEM, Result), !,
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
  !, asm(T, (N, AR, DR, MEM), [N, const | History]).

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


% We are assuming that every evaluation result is finishing in ACC

increase_stack([const, evalstack, swapa, load, swapd, const, 1, add, swapd, const, evalstack, swapa, swapd, store]).
decrease_stack([const, evalstack, swapa, load, swapd, const, -1, add, swapd, const, evalstack, swapa, swapd, store]).
save_acc_to_stack([swapd, const, evalstack, swapa, load, swapa, swapd, store]).

%% COMPILER

zip_args([], [], _):-!.
zip_args([H|T], Commands, Offset) :-
  compile(H, H1),
  O1 is Offset - 1,
  LT1 is Offset - 2,
  HCommands = [swapd, % push RESULT to ACC_D
    const, funcstack, swapa, load, % load FS to ACC
    swapd, % now ACC has RESULT, ACC_D has FS
    swapa, % RESULT -> ACC_A
    const, LT1, % ACC = LT
    swapd, % ACC = FS, ACC_D = LT
    add, % ACC = FS + OFFSET - LT
    swapa, store % store RESULT in $(FS-LT)
    ],
  zip_args(T, TCommands, O1),
  append(H1, HCommands, HCom),
  append(HCom, TCommands, Commands).

:- dynamic proc/3.
:- dynamic inside_proc/2.

compile(p_call(ID, AArgs), Commands) :-
  proc(ID, FArgs, LVars), !,
  length(AArgs, LAA),
  %% allocate function stack frame
  % RP
  % FArgs
  % LVars
  %% evaluate AArgs, save results to stack (FArgs)
  length(FArgs, LFA),
  (LAA = LFA ; (print(bad_function_arguments(FArgs, AArgs)), abort)),
  Allocate is 1 + LFA + LVars,
  zip_args(AArgs, ZipCom, Allocate),
  Com = [const, funcstack, swapa, load, swapd, const, Allocate,
    add, store | SLbl],
  %% set RP to label X
  SLbl = [const, funcstack, swapa, load,
    swapd, const, -1, add,
    swapa, const, X, store | Jmp],
  %% jump to Label
  Jmp = [const, function_jump(ID), jump | Lbl],
  %% Label(X)
  Lbl = [label(X) | Deall],
  %% deallocate stack frame
  Deall = [const, funcstack, swapa, load, swapd,
    const, Allocate, swapd, sub, store | CopyRV],
  %% copy RV to ACC
  CopyRV = [const, rv, swapa, load],
  append(ZipCom, Com, Commands).

compile(p_call(ID, _), _) :- print(function_by_id_not_found(ID)), abort.

compile(number(Arg), [const, Arg]).

compile(variable(Var), Commands) :-
  inside_proc(FArg, LVal), !,
  append(FArg, LVal, AllArg),
  find_arg(Var, AllArg, 1, P), !,
  P1 is P + 1,
  Commands = [const, funcstack, swapa, load,
  swapd, const, P1, swapd, sub, swapa, load].

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
  X = [const, evalstack, swapa, load, swapd, const, 1,
  add, swapa, load, swapd], % take C2 = stack + 1
  Y = [const, evalstack, swapa, load,swapa, load, mul], % take C1 = stack,
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
  X = [const, evalstack, swapa, load, swapd, const, 1,
  add, swapa, load, swapd], % take C2 = stack + 1
  Y = [const, evalstack, swapa, load,swapa, load, div], % take C1 = stack,
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
  DIV = [const, evalstack, swapa, load, swapa, load, swapa,
  swapd, const, -1, add, swapa, swapd, load, div],
  decrease_stack(Decr),
  % get MUL = DIV * C2
  MUL = [mul, swapd, const, evalstack, swapa, load, swapa,
  swapd, store | Decr],
  % get C1 - MUL.
  SUB = [const, evalstack, swapa, load, swapd, const, 1,
  add, swapa, load, swapd, const, evalstack, swapa, load,
  swapa, load, sub],
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
  X = [const, evalstack, swapa, load, swapd, const, 1,
  add, swapa, load, swapd], % take C2 = stack + 1
  Y = [const, evalstack, swapa, load,swapa, load, add], % take C1 = stack,
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
  X = [const, evalstack, swapa, load, swapd, const, 1,
  add, swapa, load, swapd], % take C2 = stack + 1
  Y = [const, evalstack, swapa, load,swapa, load, sub], % take C1 = stack,
  append(C1, Stack, S1),
  append(Incr, C2, S2),
  append(Stack, Decr, S3),
  append(X, Y, S4),
  append(S1, S2, U1),
  append(S3, S4, U2),
  append(U1, U2, Commands).

compile(not(Arg), Commands) :-
  compile(Arg, C1),
  IF = [swapa, const, EQ, swapa, branchz, const, 0, swapa,
  const, Fin, jump, label(EQ), const, 1, swapa,
  label(Fin), swapa],
  append(C1, IF, Commands).

% 1<=2 "1" / 1 <= 1 "0" / 2 <= 1 "-1"
compile(op("<=", Ex1, Ex2), Commands) :-
  compile(op("-", Ex2, Ex1), C1),
  % if Acc < 0 then jump to LT,
  IF = [swapa, const, LT, swapa, branchn, const, 1, swapa ],
  Lt = [const, Fin, jump, label(LT), const, 0, swapa, label(Fin), swapa],
  append(C1, IF, I1),
  append(I1, Lt, Commands).

% 1 >= 2 "-1" / 1 >= 1 "0" / 2 >= 1 "1"
compile(op(">=", Ex1, Ex2), Commands) :-
  compile(op("-", Ex1, Ex2), C1),
  % if Acc < 0 then jump to LT,
  IF = [swapa, const, LT, swapa, branchn, const, 1, swapa ],
  Lt = [const, Fin, jump, label(LT), const, 0, swapa, label(Fin), swapa],
  append(C1, IF, I1),
  append(I1, Lt, Commands).

% 1 > 2 "-1" / 1 > 1 "0" / 2 > 1 "1"
compile(op(">", Ex1, Ex2), Commands) :-
  compile(op("-", Ex2, Ex1), C1),
  % if Acc < 0 then jump to LT,
  IF = [swapa, const, LT, swapa, branchn, const, 0, swapa],
  Lt = [const, Fin, jump, label(LT), const, 1,  swapa| F],
  F = [label(Fin), swapa],
  append(C1, IF, I1),
  append(I1 , Lt, Commands).

% 1 < 2 "-1" / 1 < 1 "0" / 2 > 1 "1"
compile(op("<", Ex1, Ex2), Commands) :-
  compile(op("-", Ex1, Ex2), C1),
  IF = [swapa, const, LT, swapa, branchn, const, 0, swapa],
  Lt = [const, Fin, jump, label(LT), const, 1, swapa | F],
  F = [label(Fin), swapa],
  append(C1, IF, I1),
  append(I1, Lt, Commands).

% 1 <> 2 "-1" / 1 <> 1 "0" / 2 <> 1 "1"
compile(op("<>", Ex1, Ex2), Commands) :-
  compile(op("-", Ex1, Ex2), C1),
  IF = [swapa, const, LT, swapa, branchz, const, 1, swapa],
  Lt = [const, Fin, jump, label(LT), const, 0, swapa | F],
  F = [label(Fin), swapa],
  append(C1, IF, I1),
  append(I1, Lt, Commands).

compile(or([]), []).
compile(or([H]), Commands) :- !,
  compile(H, Commands).
compile(or([H|T]), Commands) :-
  compile(H, C1),
  save_acc_to_stack(Stack),
  increase_stack(Incr),
  compile_or(or(T), Commands1),
  append(C1, Stack, S1),
  append(Incr, Commands1, S2),
  append(S1, S2, Commands).

compile(and([]), []).
compile(and([H]), Commands) :- !,
  compile(H, Commands).
compile(and([H|T]),Commands) :-
  compile(H, C1),
  save_acc_to_stack(Stack),
  increase_stack(Incr),
  compile_and(and(T), Commands1),
  append(C1, Stack, S1),
  append(Incr, Commands1, S2),
  append(S1, S2, Commands).

compile(iwrite(E), Commands) :-
  compile(E, C),
  append(C, [swapd, const, 2, syscall], Commands).

compile(iread(E), [const, E, swapa, const, 1, syscall, store]).

compile(ireturn(Arg), Commands) :-
  %% Set RV equal to evaluted Arg
  compile(Arg, CArg),
  Com = [swapa, const, rv, swapa, store | ReadRP],
  %% Jump to RP (StackPointer - 1)
  ReadRP = [const, funcstack, swapa, load, swapd,
  const, -1, add, swapa, load, jump],
  append(CArg, Com, Commands).

compile(icall(A), Commands) :- compile(A, Commands).

compile(while(Logic, Compound), Commands) :-
  compile(Logic, CLogic),
  compile(Compound, CCompound),
  Fail = [swapa, const, Not, swapa, branchz | CCompound],
  Fin = [const, Beg, jump, label(Not)],
  append([label(Beg) | CLogic], Fail, C1),
  append(C1, Fin, Commands).

compile(ifelse(Logic, Ex1, Ex2), Commands) :-
  compile(Logic, CLogic),
  compile(Ex1, CEx1),
  compile(Ex2, CEx2),
  JumpTrue = [swapa, const, Eq, swapa, branchz | CEx1],
  JumpFalse = [const, Fin, jump, label(Eq) | CEx2 ],
  Rest = [label(Fin)],
  append(CLogic, JumpTrue, Elem1),
  append(JumpFalse, Rest, Elem2),
  append(Elem1, Elem2, Commands).

compile(if(Logic, Ex1), Commands) :-
  compile(Logic, CLogic),
  compile(Ex1, CEx1),
  JumpTrue = [swapa, const, Eq, swapa, branchz | CEx1],
  JumpFalse = [label(Eq)],
  append(CLogic, JumpTrue, Elem1),
  append(Elem1, JumpFalse, Commands).

compile(assign(Var, Val), Commands) :-
  inside_proc(FArg, LVal),
  append(FArg, LVal, AllArgs),
  find_arg(Var, AllArgs, 1, P), !,
  compile(Val, CVal),
  P1 is P + 1,
  Assign = [swapd, const, funcstack, swapa, load,
  swapd, swapa, const, P1, swapd, sub, swapa, store],
  append(CVal, Assign, Commands).

compile(assign(Var, Val), Commands) :-
  compile(Val, CVal),
  Rest = [swapa, const, Var, swapa, store],
  append(CVal, Rest, Commands).

compile([], []).
compile([H|T], Commands) :-
  compile(H, HCommands),
  compile(T, TCommands),
  append(HCommands, TCommands, Commands).

compile(block(Dec, Comp), Commands) :-
  compile(Dec, C1, Functions),
  compile(Comp, C2),
  ( inside_proc(_, _), !, C2a = []
  ; C2a = [const, 0, syscall]),
  compile_functions(Functions, C3),
  append(C1, C2, C12),
  append(C12, C2a, C12a),
  append(C12a, C3, Commands).

compile(program(_, B), Commands) :-
  compile(B, Commands).

compile(declarations([]), [], []).
compile(declarations([H|T]), Commands, Functions) :-
  compile(H, Ch, F),
  compile(declarations(T), Ct, Fs),
  append(F, Fs, Functions),
  append(Ch, Ct, Commands).

compile(local([]), [], []).
compile(local([H | T]), Commands, []) :-
  inside_proc(FormArg, LVars), !,
  length(FormArg, LFormArg),
  compile(local(T), TCommands, []),
  find_arg(H, LVars, 1, Place),
  Sub is LFormArg + Place + 1,
  Commands = [const, funcstack, swapa, load, swapd, const,
  Sub, swapd, sub, swapa, const, 0, store | TCommands].

compile(local([H|T]),
  [const, H, swapa, const, 0, store | Commands],
  []) :-
  compile(local(T), Commands, []).

compile(procedure(ID, FA, B), [],
  [procedure(ID, FA, B)]) :-
  extract_locals(B, LLoc),
  length(LLoc, Loc),
  asserta(proc(ID, FA, Loc)).

find_arg(Elem, [], _, _) :-
  write("Argument "), write(Elem), write(" not in scope."),
  nl,
  fail.
find_arg(Elem, [Elem|_], N, N) :- !.
find_arg(Elem, [value(Elem)|_], N, N) :- !.
find_arg(Elem, [name(Elem)|_], N, N) :- !.
find_arg(Elem, [_|T], N, E) :-
  N1 is N + 1,
  find_arg(Elem, T, N1, E).

extract_locals([], []).
extract_locals(declarations([]), []).
extract_locals(block(H, _), Loc) :-
  extract_locals(H, Loc).
extract_locals(declarations([local(H) | T]), Sum) :-
  extract_locals(declarations(T), TSum),
  append(H, TSum, Sum).

compile_functions([], []).
compile_functions([procedure(Id, FA, Body)|T], Commands) :-
  C1 = [next(function(Id)), function_label(Id)],
  extract_locals(Body, LV),
  asserta(inside_proc(FA, LV)),
  % Compile Body, knowing that local variables and function arguments should be loaded from stack, not global variables
  compile(Body, CBody),
  % set RV to 0
  compile(ireturn(+number(0)), ReturnCmds),
  retractall(inside_proc(_, _)),
  compile_functions(T, C2),
  append(C1, CBody, S1),
  append(S1, ReturnCmds, Function),
  append(Function, C2, Commands).

compile_and(and([]), []).
compile_and(and([H|T]), Commands) :-
  compile(H, C1),
  Next = [swapd,
  const, evalstack, swapa, load, swapd, swapa,
  const, -1, add, swapa, swapd, load,
  mul, store],
  compile_and(and(T), CRest),
  append(C1, Next, S1),
  append(S1, CRest, Commands).

compile_or(or([]), []).
compile_or(or([H|T]), Commands) :-
  compile(H, C1),
  Next = [swapd,
  const, evalstack, swapa, load, swapd, swapa,
  const, -1, add, swapa, swapd, load,
  add, store],
  compile_or(or(T), CRest),
  append(C1, Next, S1),
  append(S1, CRest, Commands).

% need to fix labels :)
fix_labels([], _, [], []).
fix_labels([next(_)|T], N, Tc, X) :-
  fix_labels(T, N, Tc, X).
fix_labels([H | T], N, [H | Tc], X) :- var(H), !, N1 is N+1, fix_labels(T, N1, Tc, X).
fix_labels([function_label(V) | T], N, Tc, [(V, N)|Rs]) :-
  !, fix_labels(T, N, Tc, Rs).
fix_labels([label(V) | T], N, Tc, X) :- !,
  V = N, fix_labels(T, N, Tc, X).
fix_labels([H | T], N, [H | Tc], X) :- N1 is N+1,
  fix_labels(T, N1, Tc, X).

fix_func_jumps([], _, []).
fix_func_jumps([function_jump(ID)|T], Ls, [V|T1]) :-
  fix_func_jumps(T, Ls, T1),
  member((ID, V), Ls).
fix_func_jumps([H|T], Ls, [H|T1]) :- fix_func_jumps(T, Ls, T1).

program(Ast, Compiled) :-
  unique_variables(Ast, UAst),
  compile(UAst, LwL),!,
  Full = [const, evalstack, swapa, const, 8193, store,
 const, funcstack, swapa, const, 16384, store, next("__main__") | LwL],
  fix_labels(Full, 0, LwPL, Ls),
  fix_func_jumps(LwPL, Ls, Compiled1),
  rewrite_variables(Compiled1, Compiled),
  !.

indent(0) :- !.
indent(N) :- write(" "), N1 is N-1, indent(N1).
pretty_print(Pr) :- pretty_print(Pr, 0).

pretty_print_args([H]) :- pretty_print(H).
pretty_print_args([H, H1|T]) :- pretty_print(H), write(", "),
  pretty_print_args([H1|T]).
pretty_print(p_call(Id, Ra), _) :- write(Id),
  write("("), pretty_print_args(Ra), write(")").
pretty_print(number(Arg), _) :- write(Arg).
pretty_print(variable(Var), _) :- write(Var).
pretty_print(-(Arg), _) :- write(-), pretty_print(Arg).
pretty_print(+(Arg), _) :- pretty_print(Arg).
pretty_print(op("div", Var1, Var2), _) :- !,
  write("("),
  pretty_print(Var1),
  write(")"), write(" / "), write("("),
  pretty_print(Var2),
  write(")").
pretty_print(op("mod", Var1, Var2), _) :- !,
  write("("),
  pretty_print(Var1),
  write(")"), write(" % "), write("("),
  pretty_print(Var2),
  write(")").
pretty_print(op(X, Var1, Var2), _) :-
  write("("),
  pretty_print(Var1),
  write(")"), write(X), write("("),
  pretty_print(Var2),
  write(")").
pretty_print(not(A), _) :- write(not),
  write(" "),
  pretty_print(A), write(" ").
pretty_print(or([H]), _) :- pretty_print(H).
pretty_print(or([H, H2|T]), _) :-
  pretty_print(H),
  write(" | "),
  pretty_print(or([H2|T])).
pretty_print(and([H]), _) :- pretty_print(H).
pretty_print(and([H, H2|T]), _) :-
  pretty_print(H),
  write(" & "),
  pretty_print(or([H2|T])).
pretty_print(iwrite(Arg), N) :-
  indent(N), write("write "), pretty_print(Arg), nl.
pretty_print(iread(Arg), N) :-
  indent(N), write("read "), pretty_print(Arg), nl.
pretty_print(ireturn(Arg), N) :-
  indent(N), write("return "), pretty_print(Arg), nl.
pretty_print(icall(A), N) :-
  indent(N), pretty_print(A).
pretty_print(while(Logic, Compound), N) :-
  indent(N), write("while "), pretty_print(Logic), nl,
  N1 is N+2,
  pretty_print(Compound, N1).
pretty_print(ifelse(Logic,Then,Else), N) :-
  indent(N), write("if "), pretty_print(Logic),
  N1 is N+2,
  indent(N), write("then"), nl,
  pretty_print(Then, N1),
  indent(N), write("else"), nl,
  pretty_print(Else, N1).
pretty_print(if(Logic,Then), N) :-
  indent(N), write("if "), pretty_print(Logic), nl,
  N1 is N+2,
  indent(N), write("then"), nl,
  pretty_print(Then, N1).
pretty_print(assign(Var, Val), N) :-
  indent(N), write(Var), write(" := "), pretty_print(Val), nl.
pretty_print([], _).
pretty_print([H|T], N) :-
  pretty_print(H, N),
  pretty_print(T, N).
pretty_print(declarations([]), _).
pretty_print(declarations([H|T]), N) :-
  pretty_print(H, N),
  pretty_print(T, N).
pretty_print(local(Vars), N) :-
  indent(N), write("local "), write(Vars),nl.
pretty_print(procedure(Id, FA, B), N) :-
  indent(N), write("procedure "), write(Id), write(" ("),
  write(FA), write(")"), nl,
  N1 is N+2,
  pretty_print(B, N1).
pretty_print(block(Dec, Ci), N) :-
  indent(N), write("with"), nl,
  N1 is N+2,
  pretty_print(Dec, N1),
  indent(N), write("begin"), nl,
  pretty_print(Ci, N1),
  indent(N), write("end"), nl.
pretty_print(program(Name, B), N) :-
  indent(N), write("program "), write(Name), nl,
  pretty_print(B, N).

pretty_print_syscall(0) :- write(abort), write("!!!").
pretty_print_syscall(1) :- write(read).
pretty_print_syscall(2) :- write(write).

pretty_print_asm(A) :- pretty_print_asm(A, 0).
pretty_print_num(N) :- format('~4d: ', N).
pretty_print_asm([], _).
pretty_print_asm([const, X, syscall | T], N) :- !,
  pretty_print_num(N),
  N1 is N+3,
  indent(2), pretty_print_syscall(X), nl,
  pretty_print_asm(T, N1).
pretty_print_asm([const, X | T], N) :- !,
  pretty_print_num(N),
  N1 is N+2,
  indent(2), write(const), write(" "), write(X), nl,
  pretty_print_asm(T, N1).
pretty_print_asm([label(Q) | T], N) :- !,
  pretty_print_num(N), N1 is N+1,
  indent(0), write(label), write(" "), write(Q), nl,
  pretty_print_asm(T, N1).
pretty_print_asm([next(Q) | T], N) :- !,
  pretty_print_num(N), N1 is N+1,
  indent(0), write(next), write(" "), write(Q), nl,
  pretty_print_asm(T, N1).
pretty_print_asm([H|T], N) :-
  pretty_print_num(N), N1 is N+1,
  indent(2), write(H), nl, pretty_print_asm(T, N1).


% Change variable names for unique ones
unique_variables(AST, NewAST) :-
  unique_variables(AST, [], NewAST).

unique_variables(p_call(Id, Ra), VariableList, p_call(Id, NRa)) :-
  unique_variables(Ra, VariableList, NRa).

unique_variables(number(Arg), _, number(Arg)).

unique_variables(variable(Var), VariableList, variable(NVar)) :-
  member((Var, NVar), VariableList).

unique_variables(-(Arg), VariableList, -(NArg)) :-
  unique_variables(Arg, VariableList, NArg).

unique_variables(+(Arg), VariableList, NArg) :-
  unique_variables(Arg, VariableList, NArg).

unique_variables(op(X, Var1, Var2), VariableList, op(X, NVar1, NVar2)) :-
  unique_variables(Var1, VariableList, NVar1),
  unique_variables(Var2, VariableList, NVar2).

unique_variables(not(Arg), VariableList, not(NArg)) :-
  unique_variables(Arg, VariableList, NArg).

unique_variables(or([]), _, or([])) :-!.
unique_variables(or([H|T]), VariableList, or([NH | NT])) :-
  unique_variables(H, VariableList, NH),!,
  unique_variables(or(T), VariableList, or(NT)).

unique_variables(and([]), _, and([])) :-!.
unique_variables(and([H|T]), VariableList, and([NH | NT])) :-
  unique_variables(H, VariableList, NH),!,
  unique_variables(and(T), VariableList, and(NT)).

unique_variables(iwrite(Arg), VariableList, iwrite(NewArg)) :-
  unique_variables(Arg, VariableList, NewArg).

unique_variables(iread(Arg), VariableList, iread(NewArg)) :-
  unique_variables(Arg, VariableList, NewArg).

unique_variables(ireturn(Arg), VariableList, ireturn(NewArg)) :-
  unique_variables(Arg, VariableList, NewArg).

unique_variables(icall(Arg), VariableList, icall(NewArg)) :-
  unique_variables(Arg, VariableList, NewArg).

unique_variables(while(Logic, Compound), VariableList, while(NLogic, NComp)) :-
  unique_variables(Logic, VariableList, NLogic),
  unique_variables(Compound, VariableList, NComp).

unique_variables(ifelse(Logic,Then,Else), VariableList, ifelse(NLogic, NThen, NElse)) :-
  unique_variables(Logic, VariableList, NLogic),
  unique_variables(Then, VariableList, NThen),
  unique_variables(Else, VariableList, NElse).

unique_variables(if(Logic,Then), VariableList, if(NLogic, NThen)) :-
  unique_variables(Logic, VariableList, NLogic),
  unique_variables(Then, VariableList, NThen).

unique_variables(assign(Var, Val), VariableList, assign(NVar, NVal)) :-
  unique_variables(Val, VariableList, NVal),
  member((Var, NVar), VariableList).

unique_variables([], _, []):-!.

unique_variables([H|T], VariableList, [NH | NT]) :-
  unique_variables(H, VariableList, NH),
  unique_variables(T, VariableList, NT).

unique_variables(program(Id, Block), VariableList, program(Id, NewBlock)) :-
  unique_variables(Block, VariableList, NewBlock, 1, _).


unique_variables(declarations([]), A, A, F, F, declarations([])) :-!.
unique_variables(declarations([H|T]), ValueList, NewValueList, Free, Taken1, declarations([NH | NT])):-
  unique_variables(H, ValueList, NewVL, Free, Taken, NH),
  unique_variables(declarations(T), NewVL, NewValueList, Taken, Taken1, declarations(NT)).

unique_variables(local([]), ValList, ValList, F, F, local([])):-!.
unique_variables(local([H|T]), ValueList, NVal,Free, Taken, local([uvar(Free) | NT])):-
  F1 is Free + 1,
  unique_variables(local(T), [(H, uvar(Free)) | ValueList], NVal, F1, Taken, local(NT)).

unique_variables(procedure(Id, FA, B), ValList, ValList, Free, Taken1, procedure(Id, NFA, NB)):-
  handleFA(FA, ValList, Nv, Free, Taken, NFA),
  unique_variables(B, Nv, NB, Taken, Taken1).

unique_variables(block(Dec, CI), VariableList, block(NewDec, NewCI), Free, Taken) :-
  unique_variables(Dec, VariableList, NewValList, Free, Taken, NewDec),
  unique_variables(CI, NewValList, NewCI).

handleFA([], NV, NV, F, F, []).
handleFA([value(H)|T], NV, NV1, F, Tk, [value(uvar(F)) | NT]) :-
  F1 is F+1,
  handleFA(T, [(H, uvar(F)) | NV], NV1, F1, Tk, NT).
handleFA([name(H)|T], NV, NV1, F, Tk, [name(uvar(F)) | NT]) :-
  F1 is F+1,
  handleFA(T, [(H, uvar(F)) | NV], NV1, F1, Tk, NT).

rewrite_variables(Code, NewCode) :-
  length(Code, LC),
  LCode is LC + 1,
  rewrite(Code, NewCode, LCode, []).

rewrite([], [], _, _) :- !.
rewrite([uvar(X) | T], [R | RewriteT], N, List) :-
  member((uvar(X), R), List),!,
  rewrite(T, RewriteT, N, List).
rewrite([uvar(X) | T], [N | RewriteT], N, List) :-
  N1 is N+1,
  rewrite(T, RewriteT, N1, [(uvar(X), N) | List]).
rewrite([rv | T], [R | RewriteT], N, List) :-
  member((rv, R), List),!,
  rewrite(T, RewriteT, N, List).
rewrite([rv | T], [N | RewriteT], N, List) :-
  N1 is N+1,
  rewrite(T, RewriteT, N1, [(rv, N) | List]).
rewrite([evalstack | T], [R | RewriteT], N, List) :-
  member((evalstack, R), List),!,
  rewrite(T, RewriteT, N, List).
rewrite([evalstack | T], [N | RewriteT], N, List) :-
  N1 is N+1,
  rewrite(T, RewriteT, N1, [(evalstack, N) | List]).
rewrite([funcstack | T], [R | RewriteT], N, List) :-
  member((funcstack, R), List),!,
  rewrite(T, RewriteT, N, List).
rewrite([funcstack | T], [N | RewriteT], N, List) :-
  N1 is N+1,
  rewrite(T, RewriteT, N1, [(funcstack, N) | List]).
rewrite([H|T],[H|RT], N, List) :-
  rewrite(T, RT, N, List).

to_binary([], []):-!.
to_binary([nop | T], [0 | BT]) :-
  !,to_binary(T, BT).

to_binary([syscall | T], [1 | BT]) :-
  !,to_binary(T, BT).

to_binary([load | T], [2 | BT]) :-
  !,to_binary(T, BT).

to_binary([store | T], [3 | BT]) :-
  !,to_binary(T, BT).

to_binary([swapa | T], [4 | BT]) :-
  !,to_binary(T, BT).

to_binary([swapd | T], [5 | BT]) :-
  !,to_binary(T, BT).

to_binary([branchz | T], [6 | BT]) :-
  !,to_binary(T, BT).

to_binary([branchn | T], [7 | BT]) :-
  !,to_binary(T, BT).

to_binary([jump | T], [8 | BT]) :-
  !,to_binary(T, BT).

to_binary([const | T], [9 | BT]) :-
  !,to_binary(T, BT).

to_binary([add | T], [10 | BT]) :-
  !,to_binary(T, BT).

to_binary([sub | T], [11 | BT]) :-
  !,to_binary(T, BT).

to_binary([mul | T], [12 | BT]) :-
  !,to_binary(T, BT).

to_binary([div | T], [13 | BT]) :-
  !,to_binary(T, BT).

to_binary([Num | T], [Num | BT]) :-
  number(Num),
  Num >= 0,!,
  to_binary(T, BT).

to_binary([Num | T], [RNum | BT]) :-
  number(Num),
  Num < 0, !,
  RNum is 65535 + Num,
  to_binary(T, BT).

% main program
algol16(Source, Sextium) :-
  phrase(program(AST), Source),
  program(AST, ASM),
  to_binary(ASM, Sextium).
