-- Zadanie 2
ssm :: Ord a => [a] -> [a]
ssm xs = foldr f [] xs where
    f a [] = [a]
    f a x = a : filter( (<) a ) x



ssm' :: [Integer] -> [Integer]
ssm' = reverse . foldl aux' []

aux' :: [Integer] -> Integer -> [Integer]
aux' [] a = [a]
aux' (x:xs) a
	|	a > x			=	a:x:xs
	|	otherwise	=	x:xs

-- Zadanie 5
lengthr :: [a] -> Int
lengthr xs = foldr (\_ x -> x+1) 0 xs

lengthl :: [a] -> Int
lengthl xs = foldl (\x _ -> x+1) 0 xs

app = flip $ foldr (:)

concat1 = foldr (++) []

reverse1 xs = foldl (flip (:)) [] xs

sum1 = foldl (+) 0

-- Zadanie 7
--1 head $ 1 : loop  (skoñczy siê)
--2 fst (1,loop) (skoñczy siê)
--3 length [loop,loop,loop] (skoñczy siê)
--4 length ones (rozbiega siê)
--5 sume ones (rozbiega siê)
--6 last ones (pêtli siê w nieskoñczonoœæ)
--7 last [1..] (pêtli siê)
--8 let f [] = 0;
--f (_:xs) = 2 + f xs in f ones (rozbiega siê)

