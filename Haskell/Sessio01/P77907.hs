-- absolute values
absValue x
  | x >= 0 = x
  | otherwise = (-x)

  
-- slow fibonacci
slowFib 0 = 0
slowFib 1 = 1
slowFib x = slowFib(x - 1) + slowFib(x - 2)
  
  
-- power 
power _ 0 = 1
power a b = a * power a (b - 1)


-- primality
-- isPrime2 1 _ = False
-- isPrime2 _ 1 = True
-- isPrime2 x d
--   | mod x d == 0 = False
--   | otherwise = isPrime2 x (d - 1)
-- isPrime x = isPrime2 x (x - 1)
isPrime x
  | abs x <= 1 = False
  | otherwise  = all ((/= 0) . (x `mod`)) [2 .. abs x - 1]


-- faster fibonacci
quickFib2 0 = (0, 1)
quickFib2 x = (r, r + q)
  where (q, r) = quickFib2(x - 1)

quickFib x = fst(quickFib2 x)
-- quickFib x = q
--   where (q, p) = quickFib2 x