import Data.Array
import Data.Array (listArray)
import Data.Array (range)


-- yet another fibonacci approach
fib :: Int -> Integer
fib max = go max
  where go 0 = 0
        go 1 = 1
        go n = fibs ! (n - 1) + fibs ! (n - 2)
        fibs = listArray (0, max) [go x | x <- [0..max]]




-- binomial coefficients
binomial :: Int -> Int -> Integer
binomial n k = go n k
  where go i j
          | j==0 || i==j = 1
          | otherwise = tbl ! (i-1,j) + tbl ! (i-1,j-1)
        tbl = listArray bounds [go i j | (i,j) <- range bounds]
        bounds = ((0,0),(n,k))




-- number of binary search trees with n nodes
-- catalan numbers, really
bst :: Int -> Integer
bst n = go n
  where go 0 = 1
        go k = let c = map (tbl !) [0..(k-1)]
          in sum $ zipWith (*) c (reverse c)
        tbl = listArray (0,n) [go k | k <- [0..n]]
          



-- minimum coins for ammount n
coins :: [Int] -> Int -> Maybe Int
coins l n 
  | x==(-1) = Nothing
  | otherwise = Just x
  where x = go n
        go 0 = 0
        go k = let c = filter (>=0) $ map (\x -> tbl ! (k-x)) $ filter (\x -> (k-x)>=0) l
          in (1 + (if (length c)==0 then (-2) else minimum c))
        tbl = listArray (0,n) [go k | k <- [0..n]]



-- cheapest matrix multiplication
mult :: [Int] -> Int
mult [] = 0
mult [_] = 1
mult l = go 0 (len-2)
  where go i j
          | i==j = 0
          | otherwise = minimum $ map (\x->(tbl!(i,x))+(tbl!(x+1,j))+((c!i)*(c!(x+1))*(c!(j+1)))) [i..(j-1)]
        tbl = listArray bounds [go a b | (a,b) <- range bounds]
        bounds = ((0,0),(len,len))
        c = listArray (0,len) l
        len = length l




































