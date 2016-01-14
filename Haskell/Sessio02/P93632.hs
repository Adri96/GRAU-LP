-- equals
eql :: [Int] -> [Int] -> Bool
-- eql [] [] = True
-- eql [] _ = False
-- eql _ [] = False
-- eql (ax:axs) (bx:bxs)
--   | ax == bx = eql axs bxs
--   | otherwise = False

eql a b = length a == length b && all (==True) (zipWith (==) a b)



-- product of elements
prod :: [Int] -> Int
prod l = foldr (*) 1 l



-- product of even elements
prodOfEvens :: [Int] -> Int
prodOfEvens l = foldr (*) 1 (filter even l)



-- powers of 2
powersOf2 :: [Int]
-- powersOf2 = scanl (*) 1 [2,2..]
powersOf2 = iterate (*2) 1



-- scalar product
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct a b = foldl (+) 0 (zipWith (*) a b)







