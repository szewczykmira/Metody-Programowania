# zadanie4
fib n = aux (0,1) where
  aux n (a,b) =
  | n == 0 = b
  | otherwise = aux (n-1) (b, a+b)
