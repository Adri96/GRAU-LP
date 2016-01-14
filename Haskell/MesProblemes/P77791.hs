-- serie collatz
serieCollatz :: Integer -> [Integer]
serieCollatz 1 = [1]
serieCollatz x
  | even x = x : (serieCollatz (div x 2))
  | otherwise = x : (serieCollatz (x*3+1))



collatzMesLlarga :: Integer -> Integer
collatzMesLlarga x = foldl myMax 0 $ map serieCollatz [1..x]



myMax :: Integer -> [Integer] -> Integer
myMax x l = max x (fromIntegral (length l))


--classe :: Integer -> Integer -> [Integer]
classe m x
  | l==m = x : classe m (x+1)
  | (0+(floor (logBase 2 (fromIntegral x))))>m = []
  | otherwise = classe m (x+1)
  where s = serieCollatz x
        l = toInteger $ length s


-- classe collatz
classeCollatz :: Integer -> Either Int [Integer]
classeCollatz x
  | l>35 = Left l
  | otherwise = Right $ classe (toInteger l) 1
  where s = serieCollatz x
        l = length s




representantsCollatz :: [Integer] -> [Integer]
representantsCollatz [] = []
representantsCollatz l = map (\x -> head (classe x 1)) l





















