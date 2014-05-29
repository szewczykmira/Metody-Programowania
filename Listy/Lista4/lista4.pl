%Zadanie1
length2(X,Y):-length(X,0,Y).
length([],A,A).
length([_|T],A,Y):-A\==Y,A1 is A+1,length(T,A1,Y).
%Zadanie2
connection(gliwice, wroclaw).
connection(wroclaw, warszawa).
connection(wroclaw, katowice).
connection(katowice, warszawa).
trip(Source, Destination, [Source|Route]):-trip(Source,Destination,Route,[Destination]).
trip(Source,Destination,Route,Route):-connection(Source,Destination).
trip(Source,Destination,Route,Visited):-connection(Current,Destination),\+member(Current,Visited),trip(Source,Current,Route,[Current|Visited]).
%Zadanie3
zrob_liste([_]).
zrob_liste([_|Y]):-zrob_liste(Y).
fill([]).
fill([0|Xs]):-fill(Xs).
fill([1|Xs]):-fill(Xs).

bin([0]).
bin([1]).
bin([1|Xs]):-zrob_liste(Xs),fill(Xs).

rfill([1]).
rfill([0|Xs]):-fill(Xs).
rfill([1|Xs]):-fill(Xs).

rbin([0]).
rbin(X):-zrob_liste(X),rfill(X).
%Zadanie4
mirror(leaf,leaf).
mirror(node(Left,V,Rigth),node(MRight,V,MLeft)):-mirror(Left,MLeft),mirror(Right,MRight).
flatten(leaf,[]).
flatten(node(Left,V,Right),List):-flatten(Left,FLeft),flatten(Right,FRight),append(FLeft,[V|FRight],List).
%Zadanie5
insert(Elem,leaf,node(leaf,Elem,leaf)).
insert(Elem,node(Left,Root,Right),node(Left,Root,InsRight)):-Elem>Root,insert(Elem,Right,InsRight).
insert(Elem,node(Left,Root,Right),node(InsLeft,Root,Right)):-Elem=<Root,insert(Elem,Left,InsLeft).
treesort(List, SortedList):-
  treesort(List, SortedTree, leaf),
  flatten(SortedTree, SortedList).
treesort([], SortedTree, SortedTree).
treesort([H|T], SortedTree, Acc):-
  insert(Acc, H, NextAcc),
  treesort(T, SortedTree, NextAcc).
%Zadanie6
sublist(_,[]).
sublist([H|T],[H|S]):-
  sublist(T,S).
sublist([_|T],S):-
  sublist(T,S).


concat_number(Digits, Num):-
  concat_number(Digits, 0, Num).
concat_number([], Res, Res).
concat_number([H|T], Acc, Res):-
  NextAcc is (Acc * 10) + H,
  concat_number(T, NextAcc, Res).

%?- length(_L, 7),
%  sublist([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], _L),
%  permutation([_A, _C, _E, _P, _R, _S, _U], _L),
%  _U \= 0,
%  _P \= 0,
%  concat_number([_U, _S, _A], USA),
%  concat_number([_U, _S, _S, _R], USSR),
%  concat_number([_P, _E, _A, _C, _E], PEACE),
%  PEACE is USA + USSR.
%
%  Zadanie7


