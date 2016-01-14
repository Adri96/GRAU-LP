-- countIf
countIf :: (Int -> Bool) -> [Int] -> Int
countIf f l = foldr (\x y -> if f x then (y+1) else y) 0 l


-- pam
pam :: [Int] -> [Int -> Int] -> [[Int]]
--pam l f = foldr (\fx fxs -> (foldr (\x xs -> (fx x):xs) [] l):fxs) [] f
--pam l f = map (\fx -> map fx l) f
pam l f = map (`map` l) f


-- pam2
pam2 :: [Int] -> [Int -> Int] -> [[Int]]
--pam2 l f = foldr (\x xs -> (foldr (\y ys -> (y x):ys) [] f):xs) [] l
--pam2 l f = map (\x -> map (\fx -> fx x) f) l
pam2 l f = map (\x -> map ($ x) f) l



-- filterFoldl  
filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl c f x l = foldl f x $ filter c l



-- insert
insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert c l x = takeWhile (not.c x) l ++ [x] ++ dropWhile (not.c x) l


-- insertion sort
insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort c l = foldr (\x xs -> insert c xs x) [] l












