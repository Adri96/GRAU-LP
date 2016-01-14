data Operator = Add | Sub | Mul deriving (Show)
data Expression = Value Int | Operation Operator Expression Expression deriving (Show)



solve :: Int -> [Int] -> Maybe Expression
solve x l = Nothing



ops [x] = Value x
ops l = map fun [1..(len-1)]
  where fun x = let (a,b) = splitAt x l
        in []
        len = length l







mySplit x s l = 
  
  



































