%Zadanie 1
%sprawdzic przez trace
%Zadanie 2
%sprawdzic przez trace
%
%Zadanie 3
%?- append(X,X,Y).
%?- select(X,[a,b,c,d],[a,c,d]).
%?- append([a,b,c],X,[a,b,c,d,e]).
%
%Zadanie 4
even([]).
even([_,_|E]):-even(E).
palindrom(X):-reverse(X,X).
singleton([_]).
%Zadanie 5
head(H,[H|T]).
last(H,T):-reverse(T,X),head(H,X).
tail(T,[H|T]).
init(T,L):-reverse(L,X),tail(T,X).
prefix(P,L):-append(P,X,L).
sufix(S,L):-append(X,S,L).
%Zadanie 6 -> tylko na odwrot sa podane zmienne
sublist([],[_]).
sublist([H|T],[H|D]):-sublist(T,D).
sublist([H|T],[E|D]):-sublist([H|T],D).
%Zadanie 7
perm([],[]).
perm(L,[H|T]):-select(H,L,LT),perm(LT,PT).
