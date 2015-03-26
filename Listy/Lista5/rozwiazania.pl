%Zadanie 1
apn(List, Apn):- apn(List, Apn, []).
apn([], Apn, Apn).
apn([H|T], Apn, N) :- append(N,H, Nn), apn(T, Apn, Nn).
%Zadanie 2
flatten(List, Flat):- flatten(List, Flat, []).
flatten([], Flat, Flat).
flatten([H|T],[H|F],Acc):- atomic(H),!,flatten(T,F,Acc).
flatten([H|T],Flat,Acc):-flatten(T, TailF, Acc), flatten(H, Flat, TailF).

%Zadanie 2
halve(List, Left, Right):- halve(List, List, Left, Right).
halve(Right, [],[],Right):-!.
halve(Right, [_],[],Right):-!.
halve([H|T],[_,_|Rest],[H|Left],Right):-halve(T,Rest, Left,Right).

%Zadanie 3
merge([],List, List):-!.
merge(List,[],List):-!.
merge([H1|T1],[H2|T2],[H1|Rest]):- H1=<H2, merge(T1,[H2|T2],Rest).
merge(List, [H|T],[H|Rest]):- merge(List, T,Rest).

merge_sort([],[]).
merge_sort([X],[X]).
merge_sort(List,Result):-halve(List, Left, Right), merge_sort(Left,ResLeft),merge_sort(Right, ResRight), merge(ResLeft,ResRight,Result).

%Zadanie 5
split([],_,[],[]):-!.
split([E|List],Med,Small,[E|Big]):- E>=Med,!, split(List, Med, Small, Big).
split([E|List],Med, [E|Small], Big):- split(List, Med, Small, Big).

qsort(List, Sorted):- qsort(List, Sorted, []).
qsort([], Sorted,Sorted).
qsort([E|Tail], Sorted, Acc):- split(Tail, E,Small, Big), qsort(Big, NextAcc, Acc), qsort(Small, Sorted, [E|NextAcc]).
