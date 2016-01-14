-- custom map
myMap :: (a -> b) -> [a] -> [b]
myMap f l = [ f x | x <- l ]



-- filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = [ x | x <- l, f x ]



-- zipWith
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = [ f x y | (x,y) <- zip a b ]



-- thingify
thingify :: [Int] -> [Int] -> [(Int,Int)]
thingify a b = [ (x,y) | x <- a, y <- b, mod x y == 0 ]



-- factors
factors :: Int -> [Int]
factors x = [ y | y <- [1..x], mod x y == 0 ]










