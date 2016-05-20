module Slownie (Rodzaj(..), Waluta(..), slownie, generate_three) where
data Rodzaj = Meski | Zenski | Nijaki deriving Show

data Waluta = Waluta {
  mianownik_poj :: String, 
  mianownik_mn :: String,
  dopelniacz_mn :: String,
  rodzaj :: Rodzaj
  } deriving Show

slownie :: Waluta -> Integer -> String
slownie _ wal = generate_three wal
--slownie waluta@(Waluta _ _ _ Zenski) wartosc = zenski waluta wartosc

ones :: Integer -> String
ones 1 = "jeden"
ones 2 = "dwa"
ones 3 = "trzy"
ones 4 = "cztery"
ones 5 = "piec"
ones 6 = "szesc"
ones 7 = "siedem"
ones 8 = "osiem"
ones 9 = "dziewiec"

teen :: Integer -> String
teen 11 = "jedenascie"
teen 12 = "dwanascie"
teen 13 = "trzynascie"
teen 14 = "czternascie"
teen 15 = "pietnascie"
teen 16 = "szesnascie"
teen 17 = "siedemnascie"
teen 18 = "osiemnascie"
teen 19 = "dziewietnascie"

enty :: Integer -> String
enty 0 = ""
enty 2 = "dwadziescia"
enty 3 = "trzydziesci"
enty 4 = "czterdziesci"
enty 5 = "piecdziesiat"
enty 6 = "szescdziesiat"
enty 7 = "siedemdziesiat"
enty 8 = "osiemdziesiat"
enty 9 = "dziewiecdziesiat"

decimal :: Integer -> String
decimal number
  | number == 0 = ""
  | number < 10 = ones number
  | number == 10 = "dziesiec"
  | number < 20 && number > 10 = teen number
  | otherwise = enty (number `div` 10) ++ " " ++ ones (number `mod` 10) 

hundreds :: Integer -> String
hundreds 0 = ""
hundreds 1 = "sto"
hundreds 2 = "dwiescie"
hundreds 3 = "trzysta"
hundreds 4 = "czterysta"
hundreds 5 = "piecset"
hundreds 6 = "szescset"
hundreds 7 = "siedemset"
hundreds 8 = "osiemset"
hundreds 9 = "dziewiecset"

generate_three number = 
  let hund = number `div` 100 
  in hundreds hund ++ " " ++ decimal (number `mod` 100)
