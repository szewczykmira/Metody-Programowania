%Zadanie 1
perm([],[]).
perm([H|T],P):-perm(T,X),select(H,P,X).
%Zadanie 2
filter([],_).
filter([H|T],[H|S]):-H>=0,perm(T,[H|S]).
filter([H|T],S):-H<0,perm(T,S).
%
count(_,[],0).
count(X,[X|T],Y):-count(X,T,Y1),!, Y is Y1+1.
count(X,[_|T],Y):-count(X,T,Y).
%
exp(_,0,1):-!.
exp(A,B,C):-exp(A,B1,C1),B is B1+1,C is C1*A.
exp2(A,B,C):-C is A^B.
%Zadanie 3
factorial(0,1):-!.
factorial(X,Y):-factorial(X1,Y1),X is X1+1,Y is Y1*X.
%
concat_number(Digits, Num):-
  concat_number(Digits, 0, Num).
concat_number([], Res, Res).
concat_number([H|T], Acc, Res):-
  NextAcc is (Acc * 10) + H,
  concat_number(T, NextAcc, Res).
%
decimal(0, Res, Res):-!.
decimal(0, Acc, Res):-
  !,
  Acc = Res.
decimal(Num, Acc, Res):-
  CurrentDigit is Num mod 10,
  NewNum is Num // 10,
  decimal(NewNum, [CurrentDigit|Acc], Res).
%Zadanie 4
selectmin([X],X,[]).
selectmin([H|T],H,[M|R]):-H<M,selectmin(T,M,R).
selectmin([H|T],M,[H|R]):-H>M,selectmin(T,M,R).
sel_sort([],[]).
sel_sort(X,[H|T]):-selectmin(X,H,R),sel_sort(R,T).
%Zadanie 5
insert([],Elem,[Elem]).
insert([H|T],Elem, [Elem,H|T]):-Elem=<H,!.
insert([H|T],Elem,[H|R]):-Elem>H,insert(T,Elem,[H|R]).

ins_sort([],[]).
ins_sort([H|T],SL):-ins_sort(T,SL1),insert(SL1,H,SL).
%Zadanie 6
reverse(X,Y):-reverse(X,[],Y).
reverse([],A,A).
reverse([H|T],A,Y):-reverse(T,[H|A],Y).
