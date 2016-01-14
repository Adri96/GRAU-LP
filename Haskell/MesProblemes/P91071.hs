import Control.Arrow ((&&&))
import Data.List ((!!), partition, sort)




indexOfMedian :: Int -> Int
indexOfMedian n = (n - 1) `div` 2

bruteForceMedian :: (Ord a) => [a] -> a
bruteForceMedian xs = (sort xs) !! (indexOfMedian $ length xs)




pivot :: (Ord a) => a -> [a] -> ([a], [a])
pivot x = filter (<=x) &&& filter (>x)





select :: Ord a => [a] -> Int -> a
select l n = mySelect l (n-1)

mySelect :: Ord a => [a] -> Int -> a
mySelect [x] 0 = x
mySelect xs n
  | n<k = mySelect smaller n
  | n==k = x
  | otherwise = mySelect larger (n-k-1) 
  where medians = map bruteForceMedian $ chunksOf 5 xs
        x = mySelect medians (indexOfMedian $ length medians)
        (smaller, larger) = pivot x xs
        k = length smaller




chunksOf n l
  | n>=len = [l]
  | otherwise = [ll] ++ chunksOf n rl
  where len = length l
        (ll,rl) = splitAt n l