import Data.List (sort)


-- find subset of numbers with sum x
sumEquals1 :: Int -> [Int] -> [[Int]]
sumEquals1 0 v = [] : sumEq1 0 v []
sumEquals1 x v = sumEq1 x v []

sumEq1 _ [] _ = []
sumEq1 x (vx:vxs) l
  | s==0 = c : (a ++ b)
  | otherwise = a ++ b
  where a = sumEq1 s vxs c
        b = sumEq1 x vxs l
        s = x-vx
        c = l ++ [vx]




sumEquals2 :: Int -> [Int] -> Maybe [Int]
sumEquals2 n v
  | (length l)==0 = Nothing
  | otherwise = Just $ last.sort $ map (reverse.sort) l
  where l = sumEquals1 n v




sumEquals3 :: Int -> [Int] -> [[Int]]
sumEquals3 n v = sumEquals1 n v






































