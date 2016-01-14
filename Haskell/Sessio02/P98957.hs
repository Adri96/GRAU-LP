import Data.List


-- helper functions
fromOne :: [Integer]
fromOne = iterate (+1) 1
fromTwo :: [Integer]
fromTwo = iterate (+1) 2



-- ones
ones :: [Integer]
ones = iterate (+0) 1


-- nats
nats :: [Integer]
nats = iterate (+1) 0



-- ints
ints :: [Integer]
ints = 0 : (concatMap (\x -> [x, -x]) $ fromOne)



-- triangulars
triangulars :: [Integer]
triangulars = 0 : (scanl (+) 1 $ fromTwo)



-- factorials
factorials :: [Integer]
factorials = scanl (*) 1 $ fromOne


-- fibs
fibs :: [Integer]
--fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
--fibs = scanl (+) 0 (1:fibs)
fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)


-- primes
primes :: [Integer]
primes = lprimes $ fromTwo
  where lprimes (x:xs) = x:(lprimes $ filter (\y -> (mod y x)/=0) xs)



-- hammings
hammings :: [Integer]
hammings = 1 : map (2*) hammings `merge` map (3*) hammings `merge` map (5*) hammings
  where merge (x:xs) (y:ys)
          | x < y = x : xs `merge` (y:ys)
          | x > y = y : (x:xs) `merge` ys
          | otherwise = x : xs `merge` ys
-- import Data.List.Ordered (union)
-- hammings = 1 : map (2*) hammings `union` map (3*) hammings `union` map (5*) hammings



-- lookNsay
-- say function
say :: Integer -> Integer
say = read.concatMap grup.group.show
  where grup s = (show $ length s) ++ [head s]

-- lookNsay sequence
lookNsay :: [Integer]
lookNsay = 1 : map say lookNsay



-- tartaglia
-- next row function
nextRow :: [Integer] -> [Integer]
nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])
 
-- tartaglia triangle (binomial coefficients)
tartaglia :: [[Integer]]
tartaglia = iterate nextRow [1]













