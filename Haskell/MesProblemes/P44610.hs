import Data.List (sort)


-- zeros and ones
zerosNones1 :: Int -> [[Int]]
zerosNones1 0 = [[]]
zerosNones1 n = map (0:) c ++ map (1:) c
  where c = zerosNones1 (n-1)




-- combinations of length n of zeros and ones with u ones and (n-u) zeros
-- medio guarro...
zerosNones2 :: Int -> Int -> [[Int]]
zerosNones2 0 _ = [[]]
zerosNones2 n u
  | (n-u)>0 && u>0 = a ++ b
  | (n-u)>0 = a
  | u>0 = b
  | otherwise = [[]]
  where a = map (0:) (zerosNones2 (n-1) u)
        b = map (1:) (zerosNones2 (n-1) (u-1))



-- subsets
subsets1 :: [a] -> [[a]]
subsets1 [] = [[]]
subsets1 (x:xs) = map (x:) s ++ s
  where s = subsets1 xs





-- subsets of size n
subsets2 :: Int -> [a] -> [[a]]
subsets2 0 _ = [[]]
subsets2 k l = mySub k (len-k) l
  where mySub _ _ [] = [[]]
        mySub y n t@(x:xs)
          | y>0 && n>0 = map (x:) (mySub (y-1) n xs) ++ mySub y (n-1) xs
          | y>0 = map (x:) (mySub (y-1) n xs)
          | n>0 = mySub y (n-1) xs
          | otherwise = []
        len = length l





















