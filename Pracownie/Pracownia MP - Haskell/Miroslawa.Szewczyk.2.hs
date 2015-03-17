{- Miroslawa Szewczyk, nr indeksu 241752
Pracownia programistyczna z Metod Programowania - zadanie 2: KWADRATY

Krotki opis zadania:
na planszy m x n (gdzie m,n<32) znajduja sie cyfry. Zadaniem jest znalezienie takiego ustawienia kwadratow, ktore
maja w jednym z rogow cyfre aby w danym kwadracie znajdowaly sie dokladnie tyle pol z cyframi na ile wskazuje cyfra w
jego rogu. Boki kwadratow nie moga sie pokrywac ani nie moga posiadac wspolnych rogow.

Zadanie rozwiazuje bruteforcem - generuje wszystkie mozliwe kwadraty a nastepnie filtruje takie ustawienia, ktore sa
zgodne ze specyfikacja zadania.
-}
import Puzzle
import Checker

type Cyfra = (Int,Int,Int)
type Kwadrat = (Int,Int,Int)

--Funkcje uzyte w zadaniu:

-- Funkcja map_kwadraty generuje wszystkie mozliwe kwadraty dla kazdej z cyfr podanych na wejsciu zadania

map_kwadraty :: Int -> Int -> [Cyfra] -> [[Kwadrat]]
map_kwadraty wiersz kol xs = map (wszystkie_kwadraty wiersz kol xs) xs

-- Funkcja wszystkie_kwadraty tworzy wszystkie poprawne kwadraty dla danej cyfry -> najpierw tworzy wszystkie jakie sie
-- da a nastepnie przefiltrowywuje aby zostaly tylko dobre

wszystkie_kwadraty :: Int -> Int -> [Cyfra] -> Cyfra -> [Kwadrat]
wszystkie_kwadraty m_wiersz m_kol xs a@(wier,kol,cyf) = filter (filtr_cyfr (filter ((/=) a) xs) cyf) lista
	where gora_lewo = gen_gl (wier,kol,cyf)
	      gora_prawo = gen_gp m_kol (wier,kol,cyf)
	      dol_lewo = gen_dl m_wiersz (wier,kol,cyf)
	      dol_prawo = gen_dp m_wiersz m_kol (wier,kol,cyf)
	      lista = gora_lewo ++ gora_prawo ++ dol_lewo ++ dol_prawo

-- Cztery kolejne funkcje dzialaja analogicznie wobec siebie: Kazda z nich przy pomocy list skladanych tworzy wszystkie
-- mozliwe kwadraty jakie sie da uzyskac z pola z dana cyfra w jednym z czterech kierunkow (gora-lewo, gora-prawo,
-- dol-lewo, dol-prawo)

gen_gl :: Cyfra -> [Kwadrat]
gen_gl (wier,kol, cyf) = let k = min wier kol in [(wier-d,kol-d,d) | d<-[1,2..(k-1)]]

gen_gp :: Int -> Cyfra -> [Kwadrat]
gen_gp m_kol (wier,kol,cyf) = let k = min wier (m_kol-kol) in [(wier-d,kol,d)| d<-[1..k]]

gen_dp :: Int -> Int -> Cyfra -> [Kwadrat]
gen_dp m_wier m_kol (wier,kol,cyf) = let k = min (m_wier-wier) (m_kol-kol) in [(wier,kol,d) | d<-[1..k]]

gen_dl :: Int -> Cyfra -> [Kwadrat]
gen_dl m_wier (wier,kol,cyf) = let k = min (m_wier-wier) kol in [(wier,kol-d,d) | d<-[1..k]]

-- Funkcja pomocnicza wykorzystywana w wszystkie_kwadraty aby wyfiltrowac tylko te kwadraty, ktore posiadaja
-- odpowiednia ilosc cyfr wewnatrz.

filtr_cyfr :: [Cyfra] -> Int -> Kwadrat -> Bool
filtr_cyfr lista cyfra elem
	| cyfra == ile elem lista = True
	| otherwise = False

-- Funckja ile sprawdza ile cyfr znajduje sie w kwadracie. W wypadku, gdy kwadrat jest niepoprawny to zwraca 901
-- (poniewaz wg specyfikacji zadania jest to wieksza ilosc cyfr niz moze sie znalezc na planszy)

ile :: Kwadrat -> [Cyfra] -> Int
ile (0,_,_) _ = 901
ile (_,0,_) _ = 901
ile trojka lista = ile_cyfr trojka lista 0

-- Funkcja ile_cyfr jest rozszerzeniem funkcji ile, przy uzyciu akumulatora - przechowuje w nim ilosc cyfr znajdujacych
-- sie w kwadracie. Przy okazji sprawdza czy ten kwadrat nie posiada jakiejs innej cyfry na ktoryms ze swoich brzegow -
-- jezeli tak jest to zwraca 901 (z tych samych powodow, dla ktorych robi to funkcja ile)

ile_cyfr :: Cyfra -> [Kwadrat] -> Int -> Int
ile_cyfr _ [] acc = acc
ile_cyfr a@(wier, kol, d) ((w1,k1,d1):ys) acc
	| (w1==wier && k1<=(kol+d) && k1>=kol) = 901
	| (w1==(wier+d) && k1<=(kol+d) && k1>=kol) = 901
	| (k1==kol && w1<=(wier+d) && w1>=wier) = 901
	| (k1==(kol+d) && w1<=(wier+d) && w1>=wier) = 901
	| w1>wier && w1<(wier+d) && k1>kol && k1<(kol+d) = ile_cyfr a ys (acc+1)
	| otherwise = ile_cyfr a ys acc


-- Funkcja przefiltruj mapuje fukcje sprawdz_pop na wszystkie kwadraty w danym rozwiazaniu a nastepnie wykorzystuje and
-- aby sprawdzicz czy wszystkie wywolania sprawdz_pop zwrocily True. Jezeli nie to oznacza, ze rozwiazanie nie jest
-- poprawne

przefiltruj :: [Kwadrat] -> Bool
przefiltruj [] = True
przefiltruj xs = and (map (\x -> sprawdz_pop x (filter ((/=) x) xs )) xs)

-- Funkcja sprawdz_pop sprawdza czy w wygenerowanym rozwiazaniu ktorys z kwadratow wspolgra zgodnie z zalozeniami
-- zadania z pozostalymi kwadratami

sprawdz_pop :: Kwadrat -> [Kwadrat] -> Bool
sprawdz_pop x []= True
sprawdz_pop (wiersz,kol,d) ((w,k,d1):ys)
	|((wiersz==w) ||(wiersz==(w+d1))|| ((wiersz+d)==w)|| ((wiersz+d)==(w+d1))) && kol<=(k+d1) && kol>=k = False
	|((kol==k) ||(kol==(k+d1))|| ((kol+d)==k)|| ((kol+d)==(k+d1))) && wiersz<=(w+d1) && wiersz>=w = False
	|otherwise = sprawdz_pop (wiersz,kol,d) ys

-- Funkcja perm_kwadratow bierze liste list roznych kwadratow i permutuje ja z lista kwadratow, tak aby powstaly
--wszystkie mozliwosci.

perm_kwadratow :: [[Kwadrat]] -> [Kwadrat] -> [[Kwadrat]]
perm_kwadratow xy ys = [reverse(a:reverse(b))| b<- xy, a<-ys]


solve :: Solver
solve wiersz kolumna xs = filter przefiltruj (foldl perm_kwadratow [[]] (map_kwadraty wiersz kolumna xs))

tests :: [Test]
tests =   [ SimpleTest
        (Puzzle 4 5 [(2,2,1), (3,3,1)])
        [(2,2,2), (1,1,2)]
    , CountTest
        (Puzzle 4 5 [(2,2,1), (3,3,1)])
        1
    , SimpleTest
        (Puzzle 6 7 [(5,2,1),(4,4,0),(5,7,0)])
        [(2,2,3),(1,4,3),(5,6,1)]
    , CountTest
        (Puzzle 6 7 [(5,2,1),(4,4,0),(5,7,0)])
        5
    ]

main :: IO ()
main = checkerMain solve tests