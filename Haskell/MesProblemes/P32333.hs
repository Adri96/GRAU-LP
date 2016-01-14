-- different flavors of factorial

-- recursively
fact1 :: Integer -> Integer
fact1 1 = 1
fact1 n = n * fact1 (n-1)

-- recursively, multiply at the end
fact2 :: Integer -> Integer
fact2 1 = 1
fact2 n = fact2 (n-1) * n

-- non-recursively
fact3 :: Integer -> Integer
fact3 n = product [1..n]



-- guards
fact4 :: Integer -> Integer
fact4 n
  | n==1 = 1
  | otherwise = n * fact4 (n-1)



-- if else
fact5 :: Integer -> Integer
fact5 n = if n==1 then 1 else n * fact5 (n-1)




-- map -really nasty one!
fact6 :: Integer -> Integer
fact6 n = let ys = 1 : map (\(a,b)->a*b) (zip [1..n] ys) in last ys



-- using fold
fact7 :: Integer -> Integer
fact7 n = foldl (*) 1 [1..n]




-- infinite list
fac = iterate (\(a,b)->(a+1,a*b)) (1,1)

fact8 :: Integer -> Integer
fact8 n = snd.head $ drop (fromIntegral n) fac 











