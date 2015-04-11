%Zadanie1

put(E, L, [E|L]).
get([E|L],E,L).
empty([]).
%addall
%roznicowe
putr(E, Li-[E|L], Li-L).
getr([E|L]-X,E, L-X).
emptyr(Y-Y).
%addallr

%Zadanie3

insert(Elem, leaf, node(leaf, Elem, leaf).
insert(Elem, node(Left,Root,Right),node(Left,Root,InsRight):- Elem > Root, insert(Elem,Right,InsRight).
insert(Elem, node(Left,Root,Right), node(InsLeft,Root,Right):- Elem < Root, insert(Elem, Left, InsLeft).
insert(Elem, node(Left,E,Rigth), node(Left,E,Right).

