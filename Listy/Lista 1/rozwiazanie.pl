%Zadanie 1
kot(my_cat).
dzdzownica(_):-fail.
ptak(_):-fail.
ryba(_):-fail.
lubi(X,Y):-ptak(X),dzdzownica(Y).
lubi(X,Y):-kot(X),ryba(Y).
lubi(X,Y):-przyjaciele(X,Y).
lubi(X,Y):-przyjaciele(Y,X).
przyjaciele(me,my_cat).
jada(my_cat,Y):-lubi(my_cat,Y).

%Zadanie 2
nie_smok(X):-mieszka(X),szczesliwy(X).
szczesliwy(X):-zwierze(X),styka(X,Y),mily(Y).
mily(X):-czlowiek(X),odwiedza(X).
styka(X,Y):-zwierze(X),mieszka(X),czlowiek(Y),odwiedza(Y).
% smok to zwierze

%Zadanie 3
man(socrates).
mortal(X):-man(X). 

%zadanie 4
sibling(X,Y):-parent(Z,X),parent(Z,Y).
sister(X,Y):-female(X),sibling(X,Y).
grandson(X,Y):-male(X),parent(P,X),parent(Y,P).
cousine(X,Y):-parent(P1,X),parent(P2,Y),sibling(P1,P2).
descendant(X,Y):-parent(Y,X).
descendant(X,Y):-parent(Y,Z),descendant(X,Z).
is_mother(X):-female(X),parent(X,_).
is_father(X):-male(X),parent(X,_).
male(adam).
female(eve).
male(john).
female(helen).
female(ivonne).
female(anna).
male(mark).
male(joshua).
male(david).
parent(john,joshua).
parent(helen,joshua).
parent(ivonne,david).
parent(david,mark).
parent(adam,helen).
parent(eve,helen).
parent(adam,ivonne).
parent(eve,ivonne).
parent(adam,anna).
parent(eve,anna).

%Zadanie 5
%descendant(john, mark).
%descendant(X, adam).
%sister(X, ivonne).
%cousin(X, Y).

%Zadanie 6
polaczenie(wroclaw,warszawa).
polaczenie(wroclaw,krakow).
polaczenie(wroclaw,szczecin).
polaczenie(szczecin,lublin).
polaczenie(szczecin,gniezno).
polaczenie(warszawa,katowice).
polaczenie(gniezno,gliwice).
polaczenie(lublin,gliwice).
polaczenie(warszawa,wroclaw).

% ?- polaczenie(wroclaw,lublin).
% false.

% ?- polaczenie(wroclaw,X).
% X = warszawa ;
% X = krakow ;
% X = szczecin.
% ?- polaczenie(X,wroclaw).
% false.

% ?- polaczenie(X,Y),polaczenie(Y,gliwice).
% X = szczecin,
% Y = lublin ;
% X = szczecin,
% Y = gniezno ;
% false.

% ?- polaczenie(X,gliwice).
% X = gniezno ;
% X = lublin.
% ?- polaczenie(X,Y),polaczenie(Y,gliwice).
% X = szczecin,
% Y = lublin ;
% X = szczecin,
% Y = gniezno ;
% false.
% ?- polaczenie(X,Y),polaczenie(Y,Z),polaczenie(Z,gliwice).
% X = wroclaw,
% Y = szczecin,
% Z = lublin ;
% X = wroclaw,
% Y = szczecin,
% Z = gniezno ;
% false.

% Pozostałe.
% Jeśli dodamy do tego połączenie np. z Warszawy do Wrocławia, to gdy zapytamy
% gdzie możemy się dostać z Wrocławia, to na liście będzie Wrocław oraz wszystko się zapętla.

connection(X,Y):-
  polaczenie(X,Y).
  connection(X,Y):-
    polaczenie(X,Z),
      connection(Z,Y).
