import Data.List

-- length of a list
myLength :: [Int] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs


-- maximum value in list
myMaximum :: [Int] -> Int
myMaximum (x : xs)
  | xs == [] || x > y = x
  | otherwise = y
  where y = myMaximum xs
        

-- average
average :: [Int] -> Float
average l = fromIntegral (foldr (+) 0 l) / fromIntegral (length l)


-- palindrome int list
buildPalindrome :: [Int] -> [Int]
buildPalindrome l = reverse l ++ l


-- remove
remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove (x : xs) b
  | elem x b = remove xs b
  | otherwise = x : remove xs b
  

-- flatten
flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x : xs) = foldl (++) x xs


-- oddsNEvens
oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens [] = ([], [])
oddsNevens (x : xs)
  | even x = (a, x : b)
  | otherwise = (x : a, b)
  where (a, b) = oddsNevens xs
  

-- primeDivisors
primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors x
  | d == [] = [ x ]
  | otherwise = nub (d ++ primeDivisors(div x (head d)))
  where d = take 1 $ filter (\y -> (mod x y) == 0) [2 .. (x - 1)]

  
  
  
  
  
  