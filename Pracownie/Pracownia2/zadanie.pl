:-op(200, fx, ~).
:-op(500, xfy, v).
% Check if it is alternative
isAlt(_ v _).
% Check if it is negation
isNeg(~_).

% Check if second element contains first element
find(X, X):-!.
find(X, X v _):-!.
find(X, _ v Y) :- find(X, Y). 

% Creating negation
neg(~A, A):-!.
neg(A, ~A).

% Convert axiom to proper display
convert([],[]).
convert([H|T], [(H, axiom) | Z]) :- convert(T, Z).

% Remove first element from second and create third
remove(A,A,[]):-!.
remove(A, A v B, B):-!.
remove(A, B v A, B):-!.
remove(A, B v C, B v D) :- remove(A, C, D).

% Get first element
getFirst([],_):-fail,!.
getFirst(A v _, A):-!.
getFirst(A,A).

% concatenation of two alternatives
concatenate([], B, B).
concatenate(A,[], A).
concatenate(A,B, A v B).

% merge two alternatives
merge(ElA, ElB, A, Res):- remove(A, ElA,RA), neg(A,B), remove(B,ElB, RB), concatenate(RA, RB, Res).

% Get two elements and check if there is any resolution
c([], _,[]):-!.
c(_,[],[]):-!.
%c(B, C, [F|D]):-getFirst(B,A), neg(A,E), find(E, C),remove(E, C, F), remove(A,B, H),!, c(H, C, D).
c(B, C, [F|D]):-getFirst(B,A), neg(A,E), find(E,C),merge(B,C,A, R),remove(A,B,H),!, c(H,C,D).
c(B, C, D):-getFirst(B,A), remove(A,B,H), c(H,C,D).

% Parse for proper answear
parse([],_,_,[]).
parse([H|T], IndA, IndB, [(H, (IndA, IndB))|Z]):-parse(T, IndA, IndB, Z).

isEmpty([]).
% Check element with every previous
iterate([], _, _, _,[]):-!.
iterate(_, [], _, _, []):-!.
iterate((A,B),[(C,_)|T],IndexA, IndexH, R1):- c(A, C, E), parse(E,IndexA,IndexH, F), append(Result, F, R1), Ind is IndexH + 1, iterate((A,B), T, IndexA, Ind, Result),!.

f([],_, _, X, X):-!.
f([H|T], Index, Spoil, Result, A) :- iterate(H, Spoil, Index, 1, R), Index1 is Index + 1, append(Spoil, [H], S1), append(Result, R, Result1), append(T,R,T1), f(T1, Index1, S1, Result1, A).

prove(X, Y):- convert(X,A),!, f(A, 1, [], A, Y).
