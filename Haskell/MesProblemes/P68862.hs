import Data.List (sort)



-- multiconjunts 1
multisets1 :: Int -> Int -> [[Int]]
multisets1 a b = multi1 [1..a] (take a [b,b..])

multi1 [] _ = [[]]
multi1 v@(vx:vxs) (cx:cxs)
  | cx>0 = map (vx:) (multi1 v ((cx-1):cxs)) ++ multi1 vxs cxs
  | otherwise = multi1 vxs cxs






-- multiconjunts 2
multisets2 :: Int -> Int -> Int -> [[Int]]
multisets2 a b c = multi2 b c [1..a] (take a [0,0..])

multi2 _ _ [] _ = [[]]
multi2 mn mx v@(vx:vxs) (cx:cxs)
  | cx<mn = a
  | cx<mx = a ++ b
  | otherwise = b
  where a = map (vx:) (multi2 mn mx v ((cx+1):cxs))
        b = multi2 mn mx vxs cxs





-- multiconjunts 3
multisets3 :: Int -> Int -> [[Int]]
multisets3 _ 0 = [[]]
multisets3 n t = multi3 [1..n] t


multi3 _ 0 = [[]]
multi3 [] _ = []
multi3 v@(vx:vxs) t = a ++ b
  where a = map (vx:) (multi3 v (t-1))
        b = multi3 vxs t




-- multiconjunts 4
multisets4 :: Int -> Int -> Int -> Int -> [[Int]]
multisets4 n x y t = multi4 x y [1..n] (take n [0,0..]) t


multi4 _ _ [] _ 0 = [[]]
multi4 x y _ [cx] 0
  | cx<x = []
  | otherwise = [[]]
multi4 _ _ [] _ _ = []
multi4 x y v@(vx:vxs) (cx:cxs) t
  | cx<x = a
  | cx<y = a ++ b
  | otherwise = b
  where a = map (vx:) (multi4 x y v ((cx+1):cxs) (t-1))
        b = multi4 x y vxs cxs t




























