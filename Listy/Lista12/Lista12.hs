-- Zadanie 1
import Control.Monad 
import Data.List 

permi :: [a] -> [[a]]
permi [] = [[]]
permi (x:xs) = concatMap (insert' x) $ permi xs

permi2 :: [a] -> [[a]] 
permi2 [] = [[]] 
permi2 (x:xs) = [zs | ys <-permi2 xs, zs <- insert' x ys] 

permi3 :: [a] -> [[a]]
permi3 [] = [[]]
permi3 (x:xs) = do
        ys <- permi3 xs
        zs <- insert' x ys
        return zs

insert' x [] = [[x]] 
insert' x l@(y:ys) = [x:l] ++ (map (y:) $ insert' x ys)

-- Zadanie 2
perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms xs = concatMap (\x -> map (x:) (perms $ delete x xs)) xs

perms2 :: (Eq a) => [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = [y:ys | y <- xs, ys <- perms2 (delete y xs)]

perms3 :: (Eq a) => [a] -> [[a]]
perms3 [] = [[]]
perms3 xs = do
        x <- xs
        ys <- perms3 (delete x xs)
        return (x:ys)

-- Zadanie 3
sublist :: [a] -> [[a]]
sublist [] = [[]]
sublist (x:xs) = concatMap (\ys -> [x:ys, ys]) $ sublist xs

sublist2 :: [a] -> [[a]]
sublist2 [] = [[]]
sublist2 (x:xs) = [res | ys <- sublist2 xs, res <- [x:ys, ys]]

sublist3 :: [a] -> [[a]]
sublist3 [] = [[]]
sublist3 (x:xs) = do
        ys <- sublist3 xs
        res <- [x:ys, ys]
        return res

-- Zadanie 4 (Zrozumieæ)
prod :: [Integer] -> Integer
prod xs = if wynik == Nothing then 0 else let Just n = wynik in n where 
      wynik = foldr (\n acc -> 
            if acc /= Nothing
            then 
                 let Just acc' = acc
                 in if n == 0
                    then Nothing
                    else Just (n*acc')
            else acc) (Just 1) xs

-- Zadanie 5 i 6

data Cyclist a = Elem (Cyclist a) a (Cyclist a) deriving (Show)

fromList :: [a] -> Cyclist a
fromList (x:xs) = let
                this = Elem prev x next 
                (next, prev) = aux this xs this
                in this

aux :: Cyclist a -> [a] -> Cyclist a -> (Cyclist a, Cyclist a) 
aux prev [] first = (first, prev) 
aux prev (x:xs) first = let
                        this = Elem prev x tmp 
                        (tmp, last) = aux this xs first 
                        in (this, last)

forward :: Cyclist a -> Cyclist a
forward (Elem _ _ next) = next

backward :: Cyclist a -> Cyclist a
backward (Elem prev _ _) = prev

label :: Cyclist a -> a
label (Elem _ a _)  = a

enumInts :: Cyclist Integer
enumInts = Elem (left enumInts) 0 (right enumInts)
        where   left current@(Elem prev x _) = Elem (left prev) (x-1) current
                right current@(Elem _ x next) = Elem current (x+1) (right next)

-- Zadanie 7
newtype Cyc a b = Cyc (Cyclist a -> (b, Cyclist a)) 

instance Monad (Cyc a) where 
  (>>=) (Cyc st1) f = Cyc (\m -> let (val,li) = st1 m 
                                     Cyc st2 = f val                                  
                                 in 
                                  st2 li) 
  return val = Cyc (\x -> (val,x)) 

runCyc :: Cyclist a -> (Cyc a b) -> b 
runCyc a (Cyc obl) = fst (obl a) 

fwd :: Cyc a () 
fwd = (Cyc (\m -> ((),forward m))) 

bkw :: Cyc a () 
bkw = (Cyc (\m -> ((),backward m))) 

lbl = (Cyc (\m -> (label m,m))) 

example :: Integer 
example = runCyc enumInts (do 
                              bkw 
                              bkw 
                              bkw 
                              bkw 
                              x <- lbl 
                              fwd 
                              fwd 
                              y <- lbl 
                              fwd 
                              z <- lbl 
                              return (x+y+z)) 

