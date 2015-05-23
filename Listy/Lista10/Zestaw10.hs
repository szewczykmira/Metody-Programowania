-- Zadanie 1

nat2 = [(x,n-x) | n <- [0..], x <- [0,1..n]]

-- Zadanie 2


cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) = pair (f . fst, g . snd)

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs) 
	where n = length(xs) `div` 2

merge ([],xs) = xs
merge (xs,[]) = xs
merge (a@(x:xs),b@(y:ys)) 
	| x <= y    = x:merge(xs,b)
	| otherwise = y: merge(a,ys)

msort [] = []
msort [x] = [x]
msort xs = merge . cross (msort,msort) . halve $ xs 

-- Zadanie 3
merge_unique [] ys = ys
merge_unique ys [] = ys
merge_unique a@(x:xs) b@(y:ys) 
	| x<y		= x:merge_unique xs b
	| x==y		= merge_unique xs b
	| otherwise 	= y:merge_unique a ys
d235 :: [Integer]
d235 = 1 : foldl1 merge_unique [ map (n *) d235 | n <- [2,3,5] ] 

--Zadanie 4

msortn :: Ord a => [a] -> Int -> [a]
msortn _ 0 = []
msortn (x:_) 1 = [x]
msortn xs n = merge ((msortn xs half), msortn (drop half xs) (n - half))
    where half = n `div` 2

msort1 xs = msortn xs (length xs)
