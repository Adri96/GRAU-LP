

  
polyEquals :: (Polynomial a) -> (Polynomial a) -> Bool
polyEquals (P []) (P []) = True
polyEquals (P (x:xs)) (P []) = x==0
polyEquals (P []) (P (x:xs)) = x==0
polyEquals (P (x:xs)) (P (y:ys)) = x==y && (P xs) `polyEquals` (P ys)
    
  
  
-- 3.1
data Polynomial a = P [a]
instance Eq a => Eq (Polynomial a) where (P x)==(P y) == (P x) `polyEquals` (P y)