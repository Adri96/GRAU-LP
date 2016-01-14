-- tree definition
data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)



-- tree node count
size :: Tree a -> Int
size Empty = 0
size (Node x l r) = 1 + size l + size r



-- tree height
height :: Tree a -> Int
height Empty = 0
height (Node x l r) = 1 + max (height l) (height r)



-- equality
equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node ax al ar) (Node bx bl br) = ax == bx && equal al bl && equal ar br



-- isomorphism
isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node ax al ar) (Node bx bl br) = ax == bx && ((isomorphic al bl && isomorphic ar br) || (isomorphic al br && isomorphic ar bl))



-- preOrder traversal
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x l r) = x : (preOrder l ++ preOrder r)



-- postOrder traversal
postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x l r) = postOrder l ++ postOrder r ++ [x]



-- inOrder traversal
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r



-- BFS traversal
bfs [] = []
bfs (Empty:xs) = bfs xs
bfs ((Node x l r):xs) = x : (bfs $ xs ++ [l,r])

breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [t]
    


-- build
build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build p@(px : pxs) i = Node px (build lp li) (build rp ri)
  where (li,_:ri) = span (/=px) i
        (lp,rp) = splitAt (length li) pxs



-- overlap
overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ a Empty = a
overlap _ Empty b = b
overlap f (Node ax al ar) (Node bx bl br) = Node (f ax bx) (overlap f al bl) (overlap f ar br)















