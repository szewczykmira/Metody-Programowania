class Monoid a where
  (***) :: a -> a -> a
  e :: a
  infixl 6 ***

infixr 7 ^^^
(^^^) :: Monoid a => a -> Integer -> a
a^^^(n)
  | n < 0            = error "Exponent below 0"
  | n == 0           = e
  | n `mod` 2 == 0   = a^^^half***a^^^half
  | n `mod` 2 == 1   = a***a^^^half***a^^^half
    where half = n `div` 2

instance Monoid Int where
  (***) = (+)
  e = 0

data Mtx2x2 a = Mtx2x2 a a a a deriving Show
instance Num a => Monoid (Mtx2x2 a) where
  (***) = mnozenie_macierzy
  e = zerowa_macierz

--Mtx2x2 a b c d
-- a b
-- c d
arrToMtx2x2 [a,b,c,d] = Mtx2x2 a b c d
zerowa_macierz :: Num a => Mtx2x2 a
zerowa_macierz =  arrToMtx2x2 $ map fromInteger [1,0,0,1]

-- a1 b1
-- c1 d1
--
-- a2 b2
-- c2 d2
mnozenie_macierzy (Mtx2x2 a1 b1 c1 d1) (Mtx2x2 a2 b2 c2 d2) =
  Mtx2x2 a3 b3 c3 d3
  where
    a3 = a1*a2+b1*c2
    b3 = a1*b2+b1*d2
    c3 = c1*a2+d1*c2
    d3 = c1*b2+d1*d2

zero_fib = arrToMtx2x2 $ map fromInteger [0,1,1,1]

fib n = let (Mtx2x2 _ _ _ f) = zero_fib ^^^ n in f
