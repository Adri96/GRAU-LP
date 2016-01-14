-- find closest pair of points, return their distance
closest :: [(Float,Float)] -> Float
closest [] = -1.0
closest [x] = -1.0
closest (x:xs)
  | m2==(-1.0) || m2>m1 = m1
  | otherwise = m2
  where m1 = minDis x xs
        m2 = closest xs




minDis :: (Float,Float) -> [(Float,Float)] -> Float
minDis p l = foldl keepMin (-1.0) l
  where keepMin m x = myMin m (dist p x)
        

myMin :: Float -> Float -> Float
myMin m d
  | m==(-1.0) || d<m = d
  | otherwise = m


dist :: (Float,Float) -> (Float,Float) -> Float
dist (a,b) (c,d) = sqrt (x * x + y * y)
  where x = abs(c-a)
        y = abs(d-b)