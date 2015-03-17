%Zadanie 1
flatten(List, Flat):- flatten(List, Flat, []).
flatten([], Flat, Flat).
flatten([H|T],[H|F],Acc):- atomic(H),!,flatten(T,F,Acc).
flatten([H|T],Flat,Acc):-flatten(T, TailF, Acc), flatten(H, Flat, TailF).

%Zadanie 2
halve(List, Left, Right):- halve(List, List, Left, Right).
halve(Right, [],[],Right):-!.
halve(Right, [_],[],Right):-!.
halve([H|T],[_,_|Rest],[H|Left],Right):-halve(T,Rest, Left,Right).
