import Data.Char (digitToInt)



-- sum of squares of first n naturals minus squared sum of first n naturals
diffSqrs :: Integer -> Integer
diffSqrs n = a*a-b
  where a = n*(n+1) `quot` 2          -- 1   + 2   + 3   + ... + n
        b = n*(n+1)*(2*n+1) `quot` 6  -- 1^2 + 2^2 + 3^2 + ... + n^2



-- pythagorean triples
triples n = [ (x,y,z) | x <- [0..n], y <- [x..n], z <- [y..n], x^2+y^2==z^2, x>0, y>0 ]

pythagoreanTriplets :: Int -> [(Int,Int,Int)]
pythagoreanTriplets n = filter (\(a,b,c) -> a+b+c==n) $ triples n




-- tartaglia
-- next row function
nextRow :: [Integer] -> [Integer]
nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])
 
-- tartaglia triangle (binomial coefficients)
tartaglia :: [[Integer]]
tartaglia = iterate nextRow [1]



-- sum of digits
sumDigits :: Integer -> Integer
sumDigits n = toInteger.sum.map digitToInt $ show n



-- digitalRoot
digitalRoot :: Integer -> Integer
digitalRoot n = head.dropWhile (>9) $ iterate sumDigits n


































