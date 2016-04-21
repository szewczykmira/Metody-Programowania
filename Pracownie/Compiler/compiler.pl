% first of all we need to define parser for algol16

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
