# zadanie 4
fib n = aux (0,1) where
  aux n (a,b) =
  | n == 0 = b
  | otherwise = aux (n-1) (b, a+b)

# zadanie 5
roots (a,b,c) = 
  if a == 0 then [(-b)/c] else
    case compare delta of
    EQ -> [-b/(2*a)]
    LT -> []
    GT -> [(-b - sqrt(delta))/(2*a), (-b + sqrt(delta))/(2*a)]
  where delta = (b*b) - 4*a*c

data Roots = No | One Double | Two (Double, Double) deriving Show 
roots2 (a,b,c) = 
  | a == 0      = No
  | delta == 0  = One(-b/2*a)
  | delta > 0   = Two(((-b-sqrt(delta))/2*a), ((-b+sqrt(delta))/2*a))
  | otherwise   = No
