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



% Temporary unneeded
numerize([],_, []).
numerize([H|T], A, [B|C]) :- B = (H, A),A1 is A +1, numerize(T, A1, C).

for(_, [], []):-!.
for(A, [H|T], [X|Y]) :- X = (A, H), for(A, T, Y).

forfor(A,B) :- for2(A, [], [], B).

for2([], _, B, B).
for2([H|T],Z,A, _) :- for(H, Z, B), append(Z,[H],Y), append(A, B, C), for2(T, Y, C, _).
