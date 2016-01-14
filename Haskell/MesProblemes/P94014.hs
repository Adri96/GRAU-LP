-- really fast fibonacci: use 'fast doubling method'
fib :: Int -> Integer
fib n = a where (a,_) = fastFib n


fastFib :: Int -> (Integer,Integer)
fastFib n
  | n==0 = (0,1)
  | (mod n 2)==0 = (a,b)
  | otherwise = (b,a+b)
  where a = c*(2*d-c)
        b = c*c+d*d
        (c,d) = fastFib (div n 2)