import Data.Char



-- flatten
flatten :: [[Int]] -> [Int]
flatten [] = []
flatten l = foldl (++) [] l




-- length
myLength :: String -> Int
myLength s = foldl (\x _ -> x+1) 0 s



-- reverse
myReverse :: [Int] -> [Int]
-- myReverse l = foldr (\x xs -> xs++[x]) [] l
myReverse l = foldl (\xs x -> x:xs) [] l



-- count occurrences of x in sublists
countIn :: [[Int]] -> Int -> [Int]
countIn l x = foldr (\ax axs -> (foldl (\y z -> if (z==x) then (y+1) else y) 0 ax) : axs) [] l



-- first word
firstWord :: String -> String
firstWord s = takeWhile (not.isSpace) $ dropWhile (isSpace) s



