-- binary search tree
data BST a = E | N a (BST a) (BST a) deriving (Show)


-- insert
insert :: Ord a => BST a -> a -> BST a
insert E x = N x E E
insert t@(N x l r) v
  | v>x = N x l (insert r v)
  | v<x = N x (insert l v) r
  | otherwise = t
  
  
-- create
create :: Ord a => [a] -> BST a
create l = foldl (insert) E l





-- remove
remove :: Ord a => BST a -> a -> BST a
remove E x = E
remove t@(N x l r) v
  | v>x = N x l (remove r v)
  | v<x = N x (remove l v) r
  | otherwise = removeNode t
  


removeNode (N _ E E) = E
removeNode (N _ l E) = l
removeNode (N _ E r) = r
removeNode (N _ l r) = N x (remove l x) r where x = getmax l





-- max value
getmax :: BST a -> a
getmax (N x _ E) = x
getmax (N _ _ r) = getmax r



-- min value
getmin :: BST a -> a
getmin (N x E _) = x
getmin (N _ l _) = getmin l




-- contains?
contains :: Ord a => BST a -> a -> Bool
contains E _ = False
contains (N x l r) v
  | v>x = contains r v
  | v<x = contains l v
  | otherwise = True




-- size
size :: BST a -> Int
size E = 0
size (N _ l r) = 1 + size l + size r





-- elements list
elements :: BST a -> [a]
elements E = []
elements (N x l r) = elements l ++ [x] ++ elements r


























