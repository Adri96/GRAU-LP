import Data.List (sort)


unicycles :: Int -> [[Int]]
unicycles 0 = []
unicycles 1 = [[1]]
unicycles 2 = [[2,1]]
unicycles n = uni 1 [1..n]


uni _ [] = []
uni p l = foldl () [] $ filter allow l
  where allow [] = []
        allow (x:xs)
          | x==p



--myUni :: Int -> [Int] -> [(Int,Int)] -> [[Int]]
--myUni _ [] _ = [[]]
--myUni p l r = foldl (\xs x ->       xs ++ (map (x:) (       myUni (p+1) (remove x l) ((x,p):r)      )     )    ) [] $ filter allowed l
--  where allowed x = x/=p && (get x r)
--        get _ [] = True
--        get x ((a,b):xs) 
--          | a==p = b/=x
--          | otherwise = get x xs



remove _ [] = []
remove v (x:xs)
  | v==x = xs
  | otherwise = x:remove v xs





































