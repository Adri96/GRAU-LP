-- sieve of eratosthenes
primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5,7..]  

sieve :: [Int] -> [Int] -> [Int]
sieve (p:ps) xs = h ++ sieve ps [ x | x <- t, (rem x p)/=0 ]
  where (h,~(_:t)) = span (<p*p) xs



-- factorization
fac :: Int -> [Int] -> [Int]
fac n pl@(p:px)
  | n==1 = []
  | p==n = [n]
  | (mod n p)==0 = p : fac (div n p) pl
  | otherwise = fac n px


factorial :: Int -> [Int]
factorial n = fac n primes



-- number of divisors
countDivisors :: [Int] -> Int
countDivisors [] = 1
countDivisors l@(p:px) = (countDivisors rp) * ((length lp)+1)
  where (lp,rp) = span (==p) l
        




-- pseudoperfect numbers infinite list
a005835 n = a005835_list !! (n-1)
a005835_list = filter ((== 1) . a210455) [1..] 


-- Characteristic function of pseudoperfect (or semiperfect) numbers
a210455 n = fromEnum $ p (a027751_row n) n where
   p ks m = m == 0 || not (null ks) && head ks <= m &&
            (p (tail ks) (m - head ks) || p (tail ks) m)


-- Triangle read by rows in which row n lists the proper divisors of n 
-- (those divisors of n which are < n), with the first row {1} by convention.
a027751 n k = a027751_tabf !! (n-1) !! (k-1)
a027751_row n = a027751_tabf !! (n-1)
a027751_tabf = [1] : map init (tail a027750_tabf) 



-- pseudoperfect?
isPseudoperfect :: Int -> [Int] -> Bool
isPseudoperfect n (x:xs)
  | x*x>n = False
  | (mod n x)==0 = True
  | otherwise = isPseudoperfect n xs


-- Triangle read by rows in which row n lists the divisors of n. 
a027750 n k = a027750_row n !! (k-1)
a027750_row n = filter ((== 0) . (mod n)) [1..n]
a027750_tabf = map a027750_row [1..] 




-- analyze method
analyze :: Int -> Either Int Bool 
analyze 1 = Right False
analyze n
  | cd<=12 = Left cd -- (isPseudoperfect n a005835_list)
  | otherwise = Left cd
  where cd = (countDivisors fl)-1
        fl = factorial n
        

















