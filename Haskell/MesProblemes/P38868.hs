-- merge
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge a [] = a
merge [] b = b
merge a@(ax : axs) b@(bx : bxs)
  | ax < bx = ax : merge axs b
  | otherwise = bx : merge a bxs
  
-- merge sort
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort l = merge (msort a) (msort b)
  where (a,b) = (take n l, drop n l)
        n = (div (length l) 2)



-- 
minProd :: [Int] -> [Int] -> Int
minProd a b = sum $ zipWith (*) (msort a) (reverse (msort b))

















