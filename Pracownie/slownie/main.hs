module Slownie (Rodzaj(..), Waluta(..), verbally) where
data Rodzaj = Meski | Zenski | Nijaki deriving Show

data Waluta = Waluta {
  mianownik_poj :: String, 
  mianownik_mn :: String,
  dopelniacz_mn :: String,
  rodzaj :: Rodzaj
  } deriving Show

slownie :: Waluta -> Integer -> String
slownie _ wal = generate_three wal

ones :: Integer -> String
ones 0 = ""
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

generate_three 0 = ""
generate_three number = 
  let hund = number `div` 100 
  in hundreds hund ++ " " ++ decimal (number `mod` 100)

partial number = 
  reverse (acc number) where
  acc n 
    | n == 0 = []
    | otherwise = let mods = n `mod` 1000 
                  in let divs = n `div` 1000 in
                  mods : acc divs

verbally = map makename . parse_value . partial

makename (num, val) 
  | length (generate_three val) == 0 = ""
  | otherwise = generate_three val ++ " " ++ make_nbr val num

parse_value a = reverse (acc 0 (reverse a)) where
acc n a@(x:xs) 
  | xs == [] = (n,x) : []
  | otherwise = let n1 = n+3 in (n, x) : acc n1 xs

-- define numbers in latin
latin_ones 0 = ""
latin_ones 1 = "un"
latin_ones 2 = "duo"
latin_ones 3 = "tri"
latin_ones 4 = "kwatuor"
latin_ones 5 = "kwin"
latin_ones 6 = "seks"
latin_ones 7 = "septen"
latin_ones 8 = "okto"
latin_ones 9 = "nowem"

latin_tens 1 = "decy"
latin_tens 2 = "wicy"
latin_tens 3 = "trycy"
latin_tens 4 = "kwadragi"
latin_tens 5 = "kwintagi"
latin_tens 6 = "seksginty"
latin_tens 7 = "septagi"
latin_tens 8 = "oktagi"
latin_tens 9 = "nonagi"

latin_hundreds 1 = "centy"
latin_hundreds 2 = "duocenty"
latin_hundreds 3 = "trycenty"
latin_hundreds 4 = "kwadryge"
latin_hundreds 5 = "kwinge"
latin_hundreds 6 = "sescenty"
latin_hundreds 7 = "septynge"
latin_hundreds 8 = "oktynge"
latin_hundreds 9 = "nonge"

latin 1 = "mi"
latin 2 = "bi"
latin 3 = "try"
latin 4 = "kwadry"
latin 5 = "kwinty"
latin 6 = "seksty"
latin 7 = "septy"
latin 8 = "okty"
latin 9 = "noni"

make_latin x 
  | x < 10 = latin x
  | otherwise = longlatin x

longlatin number
  | number < 10 = latin_ones number
  | number < 100 = longlatin (number `mod` 10) ++ latin_tens (number `div` 10)
  | number < 1000 = longlatin (number `mod` 100) ++ latin_hundreds (number `div` 100)

reform_thousand number = 
  let dec = number `mod` 10 in
  let tens = number `mod` 100 in
  acc tens dec number where
  acc tens dec num
    | num == 0 = "" 
    | num == 1 = "tysiac"
    | tens > 10 && tens < 20 = "tysiecy"
    | dec > 1 && dec < 5 = "tysiace"
    | otherwise = "tysiecy"

reform_milions mod number = 
  acc mod ++ ext number where 
  acc num 
    | num == 0 = "lion"
    | otherwise = "liard"

ext number = 
  let dec = number `mod` 10 in
  let tens = number `mod` 100 in
  acc tens dec number where
  acc tens dec num
    | num == 0 = "" 
    | num == 1 = ""
    | tens > 10 && tens < 20 = "ow"
    | dec > 1 && dec < 5 = "y"
    | otherwise = "ow"

make_nbr _ 0 = ""
make_nbr a 3 = reform_thousand a
make_nbr a number = 
  let div6 = number `div` 6 in
  make_latin div6 ++ reform_milions (number `mod` 6) a
