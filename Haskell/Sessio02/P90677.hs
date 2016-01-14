-- myFoldl
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ x [] = x
myFoldl f x (ax:axs) = myFoldl f (f x ax) axs



-- myFoldr
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x [] = x
myFoldr f x (ax:axs) = f ax $ myFoldr f x axs



-- myIterate
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)



-- myUntil
myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil c f x
  | c x = x
  | otherwise = myUntil c f $ f x



-- myMap
myMap :: (a -> b) -> [a] -> [b]
myMap f l = myFoldr (\x xs -> (f x):xs) [] l


-- myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = myFoldr (\x xs -> if f x then x:xs else xs) [] l



-- myAll
myAll :: (a -> Bool) -> [a] -> Bool
--myAll f l = 0 == (myFoldr (\x c -> if f x then c else (c+1)) 0 l)  -- WA
myAll f l = and (myMap f l)


-- myAny
myAny :: (a -> Bool) -> [a] -> Bool
--myAny f l = (myFoldr (\x c -> if f x then (c+1) else c) 0 l) > 0  -- WA
myAny f l = or (myMap f l)


-- myZip
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (ax:axs) (bx:bxs) = (ax,bx):myZip axs bxs



-- myZipWith
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = myFoldr (\x xs -> (f (fst x) (snd x)):xs) [] $ myZip a b








