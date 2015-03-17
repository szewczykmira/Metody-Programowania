%Zadanie 1
flatten(List, Flat):- flatten(List, Flat, []).
flatten([], Flat, Flat).
flatten([H|T],[H|F],Acc):- atomic(H),!,flatten(T,F,Acc).
flatten([H|T],Flat,Acc):-flatten(T, TailF, Acc), flatten(H, Flat, TailF).
