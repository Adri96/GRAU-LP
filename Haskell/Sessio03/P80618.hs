-- queue definition
data Queue a = Queue [a] [a]
  deriving (Show)



-- new queue
create :: Queue a
create = Queue [] []




-- insert
push :: a -> Queue a -> Queue a
push x (Queue l r) = Queue l (x:r)



-- shift
shift :: Queue a -> Queue a
shift (Queue l r) = Queue (reverse r) l


-- remove
pop :: Queue a -> Queue a
pop q@(Queue [] []) = q
pop q@(Queue [] r) = pop $ shift q
pop (Queue (lx:lxs) r) = Queue lxs r



-- query
top :: Queue a -> a
top q@(Queue [] r) = top $ shift q
top (Queue (lx:lxs) r) = lx



-- is empty?
empty :: Queue a -> Bool 
empty (Queue [] []) = True
empty (Queue _ _) = False



-- equality operator
instance (Eq a) => Eq (Queue a) where 
  (Queue al ar) == (Queue bl br) = (al ++ (reverse ar)) == (bl ++ (reverse br))
  








































