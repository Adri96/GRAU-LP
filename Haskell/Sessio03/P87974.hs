import System.Environment
 
-- main runs the main program
main :: IO ()
main = do
  name <- getLine
  putStrLn $ hola name


femeni [] = False
femeni (x:[]) = x=='a' || x=='A'
femeni (_:xs) = femeni xs


hola s 
  | femeni s = "Hola maca!" 
  | otherwise = "Hola maco!"