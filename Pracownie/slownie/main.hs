module Main where
import Slownie
import System.Environment

-- data Waluta = Waluta {
--  mianownik_poj :: String,
--  mianownik_mn :: String,
--  dopelniacz_mn :: String,
--  rodzaj :: Rodzaj
--  } deriving Show

-- AUD : dolar australijski
defi "AUD" = (Waluta "dolar australijski" "dolary australijskie" "dolarow austalijskich" Meski)

-- BGN : lew bułgarski
defi "BGN" = (Waluta "lew bulgarski" "lwy bulgarskie" "lwow bulgarskich" Meski)

-- BRL : real brazylijski
defi "BRL" = (Waluta "real brazylijski" "reale brazylijskie" "reali brazylijskich" Meski)

-- BYR : białoruski rubel
defi "BYR" = (Waluta "bialoruski rubel" "bialoruskie ruble" "bialoruskich rubli" Meski)

-- CAD : dolar kanadyjski
defi "CAD" = (Waluta "dolar kanadyjski" "dolary kanadyjskie" "dolarow kanadyjskich" Meski)

-- CHF : frank szwajcarski
defi "CHF" = (Waluta "frank szwajcarski" "franki szwajcarskie" "frankow szwajcarskich" Meski)

-- CNY : juan
defi "CNY" = (Waluta "juan" "juany" "juanow" Meski)

-- CZK : korona czeska
defi "CZK" = (Waluta "korona czeska" "korony czeskie" "koron czeskich" Zenski)

-- DKK : korona dunska
defi "DKK" = (Waluta "korona dunska" "korony dunskie" "koron dunskich" Zenski)

-- EUR : euro
defi "EUR" = (Waluta "euro" "euro" "euro" Nijaki)

-- GBP : funt brytyjski
defi "GBP" = (Waluta "funt brytyjski" "funty brytyjskie" "funtow brytyjskich" Meski)

-- HKD : dolar
defi "HKD" = (Waluta "dolar" "dolary" "dolarow" Meski)

-- HRK : kuna chorwacka
defi "HKR" = (Waluta "kuna chorwacka" "kuny chorwackie" "kun chorwackich" Zenski)

-- HUF : forint węgierki
defi "HUF" = (Waluta "forint wegierski" "fortinty wegierskie" "forintow wegierskich" Meski)

-- IDR : rupia
defi "IDR" = (Waluta "rupia" "rupie" "rupii" Zenski)

-- ISK : korona
defi "ISK" = (Waluta "korona" "korony" "koron" Zenski)

-- JPY : jen japonski
defi "JPY" = (Waluta "jen japonski" "jeny japonskie" "jenow japonskich" Meski)

-- KRW : won
defi "KRW" = (Waluta "won" "wony" "wonow" Meski)

-- MXN : peso
defi "MXN" = (Waluta "peso" "peso" "peso" Nijaki)

-- MYR : ringgit
defi "MYR" = (Waluta "ringgit" "ringgity" "ringgitow" Meski)

-- NOK : korona norweska
defi "NOK" = (Waluta "korona norweska" "korony norweskie" "koron norweskich" Zenski)

-- NZD : dolar
defi "NZD" = (Waluta "dolar" "dolary" "dolarow" Meski)

-- PHP : peso
defi "PHO" = (Waluta "peso" "peso" "peso" Nijaki)

-- PLN : zloty
defi "PLN" = (Waluta "zloty" "zlote" "zlotych" Meski)

-- RON : lej rumunski
defi "RON" = (Waluta "lej rumunski" "leje rumunskie" "leji rumunskich" Meski)

-- RUB : rubel rosyjski
defi "RUB" = (Waluta "rubel rosyjski" "ruble rosyjskie" "rubli rosyjskich" Meski)

-- SEK : korona szwedzka
defi "SEK" = (Waluta "korona szwecka" "korony szweckie" "koron szweckich" Zenski)

-- SGD : dolar
defi "SGD" = (Waluta "dolar" "dolary" "dolarow" Meski)

-- THB : bat
defi "THB" = (Waluta "bat" "baty" "batow" Meski)

-- TRY : lira turecka
defi "TRY" = (Waluta "lira turecka" "liry tureckie" "lir tureckich" Zenski)

-- UAH : hrywna ukraińska
defi "UAH" = (Waluta "hrywna ukrainska" "hrywny ukrainskie" "hrywien ukrainskich" Zenski)

-- USD : dolar amerykański
defi "USD" = (Waluta "dolar amerykanski" "dolary amerykanskie" "dolarow amerykanskich" Meski)

-- ZAR : rand
defi "ZAR" = (Waluta "rand" "randy" "rand" Meski)

getInt str = read str ::Integer
second lst = head (tail lst)

-- main function
main = do
  args <- getArgs
  putStrLn $ Slownie.slownie (defi (second args)) (getInt (head args))
