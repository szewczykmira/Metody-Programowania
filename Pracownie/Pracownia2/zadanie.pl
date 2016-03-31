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

remove(A,A,[]).
remove(A, A v B, B).
remove(A, B v A, B):-!.
remove(A, B v C, B v D) :- remove(A, C, D).

getFirst([],_):-fail,!.
getFirst(A v _, A):-!.
getFirst(A,A).

c([], _,[]):-!.
c(B, C, [F|D]):-getFirst(B,A), neg(A,E), find(E, C), remove(E, C, F), remove(A,B, H), c(H, C, D).
c(B, C, D):-getFirst(B,A), remove(A,B,H), c(H,C,D).

parse([],_,_,[]).
parse([H|T], IndA, IndB, [(H, (IndA, IndB))|Z]):-parse(T, IndA, IndB, Z).
%iterate(_, [], _, _, _).
%iterate((A,B),[(C,_)|T],IndexA, IndexH, Result):- compare(A, C, E), F=(E,(IndexA,IndexH)), append(Result, F, R1), Ind is IndexH + 1, iterate((A,B), T, IndexA, Ind, R1).

%f([],_, _, _):-!.
%f([H|T], Index, Spoil, Result) :- iterate(H, Spoil, Index, 1, R), Index1 is Index + 1,append(Spoil, [H], Spoil1), append(Result, R, Result1), f(T, Index1, Spoil1, Result1).

