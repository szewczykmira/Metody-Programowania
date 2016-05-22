module Slownie (Rodzaj(..), Waluta(..), make_latin) where
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
  | number == 0 = "zero"
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

partial 0 = []
partial number =
  let mods = number `mod` 1000
  in let divs = number `div` 1000
  in reverse (mods : (partial divs))
 
verbally = map generate_three . partial

parse_value a = acc 0 a where
acc n a@(x:xs) 
  | xs == [] = (n,x) : []
  | otherwise = let n1 = n+3 in (n, x) : acc n1 xs

-- define numbers in latin
latin_ones 0 = ""
latin_ones 1 = "un"
latin_ones 2 = "duo"
latin_ones 3 = "tres"
latin_ones 4 = "quattuor"
latin_ones 5 = "quin"
latin_ones 6 = "sex"
latin_ones 7 = "septen"
latin_ones 8 = "octo"
latin_ones 9 = "novem"

latin_tens 1 = "deci"
latin_tens 2 = "viginti"
latin_tens 3 = "triginti"
latin_tens 4 = "quadraginti"
latin_tens 5 = "quinquaginti"
latin_tens 6 = "sexaginti"
latin_tens 7 = "septuaginti"
latin_tens 8 = "octoginti"
latin_tens 9 = "nonaginti"

latin_hundreds 1 = "centil"
latin_hundreds 2 = "duocentil"
latin_hundreds 3 = "trecntil"
latin_hundreds 4 = "quadringentil"
latin_hundreds 5 = "quingentil"
latin_hundreds 6 = "sescentil"
latin_hundreds 7 = "septingentil"
latin_hundreds 8 = "octingentil"
latin_hundreds 9 = "nongentil"

latin 1 = "mi"
latin 2 = "bi"
latin 3 = "tril"
latin 4 = "quadril"
latin 5 = "quintil"
latin 6 = "sextil"
latin 7 = "septil"
latin 8 = "octil"
latin 9 = "nonil"

make_latin x 
  | x < 10 = latin x
  | otherwise = longlatin x

longlatin number
  | number < 10 = latin_ones number
  | number < 100 = longlatin (number `mod` 10) ++ latin_tens (number `div` 10)
  | number < 1000 = longlatin (number `mod` 100) ++ latin_hundreds (number `div` 100)
  | otherwise = longlatin (number `mod` 1000) ++ "millinil"
