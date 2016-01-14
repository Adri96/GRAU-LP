lmult n = [ x*n | x <- [1..] ]

mcm x y = i_mcm l1 l2
  where l1 = lmult x
        l2 = lmult y
        i_mcm a@(ax:axs) b@(bx:bxs)
          | ax<bx = i_mcm axs b
          | ax>bx = i_mcm a bxs
          | otherwise = ax


data PList a = Period [a] [a]


generate :: PList a -> [a]
generate (Period x l) = x ++ cycle l


instance (Eq a) => Eq (PList a) where
  p1@(Period x1 l1) == p2@(Period x2 l2) = all (==True) k
    where n = max (length x1) (length x2)
          m = mcm (length l1) (length l2)
          k1 = take (n+m) $ generate p1
          k2 = take (n+m) $ generate p2
          k = zipWith (==) k1 k2
          
          
          


data Tree a = Node a [Tree a]

nivell :: Int -> (Tree a) -> [a]
nivell n t = i_nivell 0 [t]
  where i_nivell x ts
          | x==n = map (\(Node x l) -> x) ts
          | otherwise = i_nivell (x+1) (concat [ l | (Node a l) <- ts ])
          
          
arbre = (Node 2 [ Node 5 [ Node 3 [Node 8 []], Node 6 [ Node 2 [], Node 4 [] ], Node 1 [] ], Node 3 [ Node 8 [] ], Node 4 [ Node 5 [], Node 1 [], Node 2 [] ] ])



-- Examen 11 juny 2012
-- P1
lam_ord l = foldl (\a b -> a+b^2) 0 l

-- P2
inf l = [ n | n <- [2..], (all (==False) (map (\x -> (mod n x)==0) l)) ]




  
  
  
  
  
  
  
  
  
  
  
  
  
  
