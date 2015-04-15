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
delete(Elem,node(Left,Elem,Right),node(LeftWM,LeftMax,Right)):- findMax(Left,LeftMax), delete(LeftMax,Left,LeftWM).

delMax(Begin,End,Elem):- findMax(Begin,Elem), delete(Elem,Begin,End).

empty(leaf).
%Zadanie5
TBA

%Zadanie6
% 1
% {0^n 1^m 0^n | n,m<N}
% <G={0,1},{S,M}, S, S-> 0S0 | M | E, M -> 1M | E>

% 2
% {0^n 1^n 0^m | n,m <N}
% <G={0,1}, {S,M}, S, S-> S0 | T0 | E, T-> 0T1 | E >

% 3
% {0^n 1^m 0^k | n,m,k<N }
3. G = <E,V,S,P>

E = {0,1}
V = {S,T}
S = S
P = {
  S -> S0
  S -> T
  S -> e

  T -> T1
  T -> 0T1
  T -> e
    }

4. G = <E,V,S,P>

  E = {0,1}
  V = {S}
  S = S
  P = {
  S -> 01S00
  S -> e
  }

5. G = <E,V,S,P>

  E = {0,1}
  V = {S}
  S = S
  P = {
    S -> 0S1
    S -> 1S0
    S -> SS // Zauważmy, że to przyda się przy generowaniu 1010
    S -> e
  }

6. G = <E,V,S,P>

  E = {0,1}
  V = {S}
  S = S
  P = {
    S -> 1S0S0
    S -> 0S1S0
    S -> 0S0S1
    S -> e
  }

 %Zadanie7 

1)  G = <E,V,S,P>
    E = {0}
    V = {S}
    S = S
    P = {
    S -> 00S
    S -> e
 }

2)  G = <E,V,S,P>
    E = {0,1}
    V = {S,T}
    S = S,
    P = {
    S -> 0S
    S -> M
    S -> e
    M -> 1M
    M -> e
 }

3)  G = <E,V,S,P>
    E = {0,1}
    V = {S,T}
    S = S
    P = {
    S -> 0S
    S -> 1M
    S -> 11M
    S -> e
    M -> 0S
    M -> e
  }

4)  G = <E,V,S,P>
    E = {0,1}
    V = {S,T}
    S = S
    P = {
    S -> 0T
    S -> 1S
    T -> 0S
    T -> 1T
    }

5)  G = <E,V,S,P>
    E = {0,1}
    V = {S,T,U,W}
    S = S
    P = {
    S -> 0T
    S -> 1W // być może koniec
    T -> 0S
    T -> 1U

    U -> 0W // być może koniec
    U -> 1T
    W -> 0U
    W -> 1S
    W -> e
    }

6)  G = <E,V,S,P>
    E = {0,1}
    V = {S,T}
    S = S
    P = {
    S -> 0T
    S -> 1T
    S -> e
    T -> 0S
    T -> 1T
    }
