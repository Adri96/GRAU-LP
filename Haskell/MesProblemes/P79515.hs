-- AVL tree definition
data AVL a = E | N a Int (AVL a) (AVL a) deriving (Show)



-- get value
value :: AVL a -> a
value (N x _ _ _) = x


-- get height
height :: AVL a -> Int
height E = -1
height (N _ h _ _) = h


-- get children
left :: AVL a -> AVL a
left E = E
left (N _ _ l _) = l

right :: AVL a -> AVL a
right E = E
right (N _ _ _ r) = r



-- insert
insert :: Ord a => AVL a -> a -> AVL a
insert E a = N a 0 E E
insert t@(N x h l r) a
  | a<x = balance (N x h (insert l a) r)
  | a>x = balance (N x h l (insert r a))
  | otherwise = t
  


-- balance factor
balFac :: AVL a -> Int
balFac E = 0
balFac (N _ _ l r) = height l - height r



-- calculate height
calcHeight :: AVL a -> AVL a -> Int
calcHeight l r = 1 + max (height l) (height r)


-- join
join :: a -> AVL a -> AVL a -> AVL a
join x l r = N x (calcHeight l r) l r



-- rotations
lRot :: AVL a -> AVL a
lRot E = E
lRot (N x _ l r) = join (value l) (left l) (join x (right l) r)

rRot :: AVL a -> AVL a
rRot E = E
rRot (N x _ l r) = join (value r) (join x l (left r)) (right r)

lRRot :: AVL a -> AVL a
lRRot E = E
lRRot (N x h l r) = lRot (N x h (rRot l) r)

rLRot :: AVL a -> AVL a
rLRot E = E
rLRot (N x h l r) = rRot (N x h l (lRot r))




-- rebalance function
balance :: AVL a -> AVL a
balance E = E
balance t@(N x _ l r)
  | b>1 && bL<0    = lRRot t  -- left-right rotate
  | b>1            = lRot t   -- left
  | b<(-1) && bR>0 = rLRot t  -- right-left 
  | b<(-1)         = rRot t   -- right
  | otherwise = join x l r    -- no imbalance
  where b = balFac t
        bL = balFac l
        bR = balFac r



-- init from list
create :: Ord a => [a] -> AVL a
create l = foldl (insert) E l



-- is AVL?
check :: AVL a -> (Bool,Int)
check E = (True,-1)
check (N _ h l r)
  | lB==True && rB==True && (abs (lH-rH))<2 = (True,h)
  | otherwise = (False,-99)
  where (lB,lH) = check l
        (rB,rH) = check r



















