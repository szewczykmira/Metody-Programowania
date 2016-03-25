:-op(200, fx, ~).
:-op(500, xfy, v).

isAlt(_ v _).
isNeg(~_).

find(X, X):-!.
find(X, X v _):-!.
find(X, _ v Y) :- find(X, Y). 

neg(~A, A):-!.
neg(A, ~A).

convert([],[]).
convert([H|T], [(H, axiom) | Z]) :- convert(T, Z).

numerize([],_, []).
numerize([H|T], A, [B|C]) :- B = (H, A),A1 is A +1, numerize(T, A1, C).

for(_, [], []):-!.
for(A, [H|T], [X|Y]) :- X = [A, H], for(A, T, Y).
