import Data.List (sort)




permutations1 :: [a] -> [[a]]
permutations1 [] = [[]]
permutations1 xs = [ y:zs | (y,ys) <- select xs, zs <- permutations1 ys]
  where select []     = []
        select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]

--permutations1 xs0 =  xs0 : perms xs0 []
--  where
--    perms [] _  = []
--    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations1 is)
--      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
--            interleave' _ []     r = (ts, r)
--            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
--                                     in  (y:us, f (t:y:us) : zs)







permutations2 :: Int -> [[Int]]
permutations2 n = permutations1 [1..n]









permutations3 :: [a] -> [[a]]
permutations3 l = perms3 (length l)
  where perms3 0 = [[]]
        perms3 n = [(x:xs) | x <- l, xs <- perms3 (n-1)]





permutations4 :: Int -> [[Int]]
permutations4 x = permutations3 [1..x]








permutations5 :: [Char] -> [Char] -> [[Char]]
permutations5 [] [] = [[]]
permutations5 c v = [(b:a:xs) | b<-c, a<-v, xs<-permutations5 (remove b c) (remove a v)]


remove _ [] = []
remove v (x:xs)
  | v==x = xs
  | otherwise = x:remove v xs











































