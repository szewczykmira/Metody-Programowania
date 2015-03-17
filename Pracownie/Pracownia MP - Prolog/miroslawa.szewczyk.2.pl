%Miroslawa Szewczyk
%nr indeksu 241752
%Zadanie nr 2 - Kwadraty
% Zadanie polega na znalezieniu wspolrzednych kwadratow, ktore posiadaja
% w jednym z rogow cyfre rowna liczbie cyfr znajdujacych sie wewnatrz
% kwadratu. Boki kwadratow nigdzie nie moga sie pokrywac, a ich rogi
% stykac.
%
% Moje rozwiazanie generuje dla kazdej cyfry wszystkie poprawne kwadraty
% a nastepnie wybiera takie ich zestawienie, w ktorym
% ustawione one sa zgodnie z wymogami zadania.
% Wiem, ze ten algorytm nie jest efektywny, jednak generuje rozwiazanie.
%
%DODATKOWE INFORMACJE:
%brak dolaczonych testow studenta.
%
% W rozwiazaniu trojka
% (m,n,_) m = numer wiersza, n = numer kolumny.
%
%
%Funkcje pomocnicze
%
%Predykat jest_spoko:
% Sprawdza czy na bokach kwadratu znajduje sie jakas cyfra. Predykat
% pomocniczy jest_niespoko zwraca true, jezeli na krawedziach znajduje
% sie jakas cyfra. Natomiast jest_spoko odwraca jego wartosc.
jest_niespoko((Wier,Kol,D),ListaCyfr):-
	Kol1 is Kol+D,
	Wier1 is Wier+D,
	(   (  member((NW,Kol,_),ListaCyfr),
	    NW >= Wier,
	    NW =< Wier1);
	(   member((NWier,Kol1,_),ListaCyfr),
	    NWier >=Wier,
	    NWier =< Wier1);
	(   member((Wier,NK,_),ListaCyfr),
	    NK >= Kol,
	    NK =< Kol1);
	(   member((Wier1,NKol,_),ListaCyfr),
	    NKol >= Kol,
	    NKol =< Kol1)    ).
jest_spoko(Punkt,ListaCyfr):-
	\+jest_niespoko(Punkt,ListaCyfr).
%Predykat ile_cyfr:
%Zlicza ile cyfr znajduje sie w kwadracie.
ile_cyfr(Punkt,ListaCyfr,NoweCyfry):-
	ile_cyfr(Punkt,ListaCyfr,0,NoweCyfry).
ile_cyfr(_,[],Acc,Acc):-!.
ile_cyfr((Wier,Kol,D),[(W,K,_)|T],Acc,NoweCyfry):-
	K>Kol,
	K< Kol+D,
	W > Wier,
	W < Wier+D,
	Acc1 is Acc+1,
	ile_cyfr((Wier,Kol,D),T,Acc1,NoweCyfry).
ile_cyfr(Punkt,[_|T],Acc,NoweCyfry):-
	ile_cyfr(Punkt,T,Acc,NoweCyfry).


% Predykat append_roz:
% Pozwala na polaczenie czterech list w jedna.
append_roz(A,B,C,D,Wynik):-append(A,B,W1),append(C,D,W2),append(W1,W2,Wynik).
%
%
%Predykat generator_gora_lewo:
% Czesc skladowa predykatu generator; generuje wszystkie poprawne
% kwadraty, ktore posiadaja cyfre w prawym-dolnym rogu.
generator_gora_lewo((Wier,Kol,Cyfra),ListaCyfr,Wynik):-
	NKol is Kol-1,
	NWie is Wier-1,
	generator_gl(Cyfra,ListaCyfr,(NWie,NKol,1),[],Wynik).
generator_gl(_,_,(0,_,_),A,A):-!.
generator_gl(_,_,(_,0,_),A,A):-!.
generator_gl(Cyfra,Lista,Punkt,A,A):-
	ile_cyfr(Punkt,Lista,Cyf),
	Cyf > Cyfra,!.
generator_gl(Cyfra,ListaCyfr,(Wier,Kol,D),T,Wynik):-
	jest_spoko((Wier,Kol,D),ListaCyfr),
	ile_cyfr((Wier,Kol,D),ListaCyfr,Cyfra),
	K1 is Kol-1,
	W1 is Wier-1,
	D1 is D+1,
	append([(Wier,Kol,D)],T,T1),
	generator_gl(Cyfra,ListaCyfr,(W1,K1,D1),T1,Wynik).
generator_gl(Cyfra,ListaCyfr,(Wier,Kol,D),T,Wynik):-
	K1 is Kol-1,
	W1 is Wier-1,
	D1 is D+1,
	generator_gl(Cyfra,ListaCyfr,(W1,K1,D1),T,Wynik).
%Predykat generator_gora_prawo:
% Czesc skladowa predykatu generator; generuje wszystkie poprawne
% kwadraty, ktore posiadaja cyfre w prawym-dolnym rogu.
generator_gora_prawo((Wier,Kol,Cyfra),ListaCyfr,M,Wynik):-
	NWie is Wier-1,
	generator_gp(Cyfra,ListaCyfr,(NWie,Kol,1),M,[],Wynik).
generator_gp(_,_,(_,M1,D),M,A,A):-M is M1+D-1,!.
generator_gp(_,_,(0,_,_),_,A,A):-!.
generator_gp(Cyfra,Lista,Punkt,_,A,A):-
	ile_cyfr(Punkt,Lista,Cyf),
	Cyf>Cyfra,!.
generator_gp(Cyfra,ListaCyfr,(Wier,Kol,D),M,T,Wynik):-
	jest_spoko((Wier,Kol,D),ListaCyfr),
	ile_cyfr((Wier,Kol,D),ListaCyfr,Cyfra),
	W1 is Wier-1,
	D1 is D+1,
	append([(Wier,Kol,D)],T,T1),
	generator_gp(Cyfra,ListaCyfr,(W1,Kol,D1),M,T1,Wynik).
generator_gp(Cyfra,ListaCyfr,(Wier,Kol,D),M,T,Wynik):-
	W1 is Wier-1,
	D1 is D+1,
	generator_gp(Cyfra,ListaCyfr,(W1,Kol,D1),M,T,Wynik).
%Predykat generator_dol_lewo:
% Czesc skladowa predykatu generator; generuje wszystkie poprawne
% kwadraty, ktore posiadaja cyfre w lewym-dolnym rogu.
generator_dol_lewo((Wier,Kol,Cyfra),ListaCyfr,M,Wynik):-
	NKol is Kol-1,
	generator_dl(Cyfra,ListaCyfr,(Wier,NKol,1),M,[],Wynik).
generator_dl(_,_,(_,0,_),_,A,A):-!.
generator_dl(_,_,(N1,_,D),N,A,A):-N is N1+D-1,!.
generator_dl(Cyfra,Lista,Punkt,_,A,A):-
	ile_cyfr(Punkt,Lista,Cyf),
	Cyf>Cyfra,!.
generator_dl(Cyfra,ListaCyfr,(Wier,Kol,D),M,T,Wynik):-
	jest_spoko((Wier,Kol,D),ListaCyfr),
	ile_cyfr((Wier,Kol,D),ListaCyfr,Cyfra),
	K1 is Kol-1,
	D1 is D+1,
	append([(Wier,Kol,D)],T,T1),
	generator_dl(Cyfra,ListaCyfr,(Wier,K1,D1),M,T1,Wynik).
generator_dl(Cyfra,ListaCyfr,(Wier,Kol,D),M,T,Wynik):-
	K1 is Kol-1,
	D1 is D+1,
	generator_dl(Cyfra,ListaCyfr,(Wier,K1,D1),M,T,Wynik).
%Predykat generator_dol_prawo:
% Czesc skladowa predykatu generator; generuje wszystkie poprawne
% kwadraty, ktore posiadaja cyfre w prawym-dolnym rogu.
generator_dol_prawo((Wier,Kol,Cyfra),ListaCyfr,M,N,Wynik):-
	generator_dp(Cyfra,ListaCyfr,(Wier,Kol,1),M,N,[],Wynik).
generator_dp(_,_,(_,M,D),_,M1,A,A):-M1 is M+D-1,!.
generator_dp(_,_,(N1,_,D),N,_,A,A):-N is N1+D-1,!.
generator_dp(Cyfra,Lista,Punkt,_,_,A,A):-
	ile_cyfr(Punkt,Lista,Cyf),
	Cyf>Cyfra,!.
generator_dp(Cyfra,ListaCyfr,(Wier,Kol,D),M,N,T,Wynik):-
	jest_spoko((Wier,Kol,D),ListaCyfr),
	ile_cyfr((Wier,Kol,D),ListaCyfr,Cyfra),
	D1 is D+1,
	append([(Wier,Kol,D)],T,T1),
	generator_dp(Cyfra,ListaCyfr,(Wier,Kol,D1),M,N,T1,Wynik).
generator_dp(Cyfra,ListaCyfr,(Wier,Kol,D),M,N,T,Wynik):-
	D1 is D+1,
	generator_dp(Cyfra,ListaCyfr,(Wier,Kol,D1),M,N,T,Wynik).

%
% Predykat generator:
% Generuje on liste wszystkich poprawnych kwadratow dla danej cyfry.
generator(Krotka,ListaPozostalychCyfr,M,N,ListaWynikowa):-
	generator_gora_lewo(Krotka,ListaPozostalychCyfr,WynikGL),
	generator_gora_prawo(Krotka,ListaPozostalychCyfr,N,WynikGP),
	generator_dol_lewo(Krotka,ListaPozostalychCyfr,M,WynikDL),
	generator_dol_prawo(Krotka,ListaPozostalychCyfr,M,N,WynikDP),!,
	append_roz(WynikGL,WynikGP,WynikDL,WynikDP,ListaWynikowa).
%Predykat mega_generator:
% Tworzy liste skladajaca sie z list zawierajacych poprawne kwadraty dla
% cyfry.
mega_generator(Lista,M,N,Wynik):-
	mega_generator(Lista,Lista,M,N,[],Wynik).
mega_generator([],_,_,_,Acc,Acc).
mega_generator([H|T],Lista,M,N,Wynik,SuperWynik):-
	select(H,Lista,Cos),
	generator(H,Cos,M,N,WynikMaly),
	append(Wynik,[WynikMaly],Wynik1),
	mega_generator(T,Lista,M,N,Wynik1,SuperWynik).

%Predykat wybierz_listy:
% Z listy poprawnych kwadratow wygenerowanej dla kazdej z cyfr wybiera
% jedna i tworzy z tego jedna liste.
wybierz_listy([],[]).
wybierz_listy([H|T],[Elem|Wynik]):-
	select(Elem,H,_),
	wybierz_listy(T,Wynik).

%Predykat czy_poprawne:
% Sprawdza czy wynik wygenerowany przez predykat wybierz_listy jest
% poprawnym rozwiazaniem zagadki: sprawdza czy kwadraty maja nakladajace
% sie krawedzie.
%
niepoprawne_wiersze((Wier,Kol,D),T):-
	Wier1 is Wier+D,
	Kol1 is Kol+D,!,
	(   Wiersz is Wier1;
	Wiersz is Wier),
	member((Wiersz,Kolumna,D1),T),
	Kolumna =< Kol1,
	K1 is Kolumna + D1,
	K1 >= Kol.
niepoprawne_kolumny((Wier,Kol,D),T):-
	Wier1 is Wier+D,
	Kol1 is Kol+D,!,
	(   Kolumna is Kol;
	Kolumna is Kol1),
	member((Wiersz,Kolumna,D1),T),
	Wiersz =< Wier1,
	W1 is Wiersz + D1,
	W1 >= Wier.




czy_niepoprawne([_]):-fail.
czy_niepoprawne([(Wier,Kol,D)|T]):-
	niepoprawne_wiersze((Wier,Kol,D),T);
	niepoprawne_kolumny((Wier,Kol,D),T).
czy_niepoprawne([_|T]):-
	czy_niepoprawne(T).
czy_poprawne(Lista):-
	\+czy_niepoprawne(Lista).
%Predykat solve:
%Glowny predykat zadania - wylicza i podaje rozwiazanie:
solve(M,N,PolaZLiczbami,ListaKwadratow):-
	mega_generator(PolaZLiczbami,M,N,ListaList),!,
	wybierz_listy(ListaList,ListaKwadratow),
	czy_poprawne(ListaKwadratow).
student_simple_test(4, 5, [(2,2,1), (3,3,1)],
  [(2,2,2), (1,1,2)]).
student_simple_test(6,7,[(5,2,1),(4,4,0),(5,7,0)],[(2,2,3),(1,4,3),(5,6,1)]).
student_count_test(4,5,[(2,2,1),(3,3,1)],1).

:-[checker].
:-check_solution.
