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

find(Elem, node(_,Root,_)):-Elem==Root,!.
find(Elem, node(Left,Root,Right)):- Elem<Root, find(Elem,Left).
find(Elem, node(Left,Root,Right)):- Elem>Root, find(Elem,Right).

findMax(node(_,Root,leaf),Root):-!.
findMax(node(_,_,Right),Max):- findMax(Right,Max).

delete(_,leaf,leaf):-!.
delete(Elem,node(leaf,Elem,leaf),leaf):-!.
delete(Elem,node(Left,Elem,leaf),Left):-!.
delete(Elem,node(leaf,Elem,Right),Right):-!.
delete(Elem,node(Left,Elem,Right),node(LeftWM,LeftMax,Right):- findMax(Left,LeftMax), delete(LeftMax,Left,LeftWM).

delMax(Begin,End,Elem):- findMax(Begin,Elem), delete(Elem,Begin,End).

empty(leaf).
