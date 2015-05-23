-- Zadanie 2
ssm :: Ord a => [a] -> [a]
ssm xs = foldr f [] xs where
    f a [] = [a]
    f a x = a : filter( (<) a ) x

-- Zadanie 5
lengthr :: [a] -> Int
lengthr xs = foldr (\_ x -> x+1) 0 xs

lengthl :: [a] -> Int
lengthl xs = foldl (\x _ -> x+1) 0 xs

app = flip $ foldr (:)

concat1 = foldr (++) []

reverse1 xs = foldl (flip (:)) [] xs

sum1 = foldl (+) 0

