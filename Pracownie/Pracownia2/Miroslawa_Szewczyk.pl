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
concatenate([], B, B):-!.
concatenate(A,[], A):-!.
concatenate(A v B, C, A v D):- !,concatenate(B, C, D).
concatenate(A, C, A v C).

% merge two alternatives
merge(ElA, ElB, A, Res):- remove(A, ElA,RA), neg(A,B), remove(B,ElB, RB), concatenate(RA, RB, Res).

% Get two elements and check if there is any resolution
c(_,[], _,[]):-!.
c(_,_,[],[]):-!.
c(U,B, C, [F|D]):-getFirst(B,A), neg(A,E), find(E,C),merge(U,C,A, F),remove(A,B,H),!, c(U,H,C,D).
c(U,B, C, D):-getFirst(B,A), remove(A,B,H),!, c(U,H,C,D).

% Parse for proper answear
parse([],_,_,[]).
parse([H|T], IndA, IndB, [(H, (IndA, IndB))|Z]):-parse(T, IndA, IndB, Z).

% Check element with every previous
iterate([], _, _, _,[]):-!.
iterate(_, [], _, _, []):-!.
iterate((A,B),[(C,_)|T],IndexA, IndexH, R1):- c(A,A, C, E), parse(E,IndexA,IndexH, F), append(Result, F, R1), Ind is IndexH + 1, iterate((A,B), T, IndexA, Ind, Result),!.

% remove duplicates from second list
noDup(_, [], []):-!.
noDup(A, [(H, I, J)|T], [(H,I,J)|Z]):- \+member((H,_,_),A),!, noDup(A,T,Z).
noDup(A, [_|T], Z):-noDup(A, T, Z).

f([],0, 0, X, X):-!.
f([], _, _, _, _,):-fail.
f([H|_], Index, Spoil, Result, A) :- iterate(H, Spoil, Index, 1, R), member(([], _, _), R),!, noDup(Result, R,RZ), append(Result, RZ, Result1), f([], 0, 0, Result1, A).
f([H|T], Index, Spoil, Result, A) :- iterate(H, Spoil, Index, 1, R), Index1 is Index + 1, append(Spoil, [H], S1),noDup(Result,R,RZ) ,append(Result, RZ, Result1), append(T,RZ,T1), f(T1, Index1, S1, Result1, A).

prove(X, Y):- convert(X,A),!, f(A, 1, [], A, Y).
