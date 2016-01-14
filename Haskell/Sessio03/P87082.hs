import System.Environment
import Control.Monad



-- main runs the main program
main :: IO ()
main = do
  line <- getLine
  unless (line=="*") $ do
    process line
    main

    
message m h
  | imc<18 = "magror"
  | imc>=18 && imc<25 = "corpulencia normal"
  | imc>=25 && imc<30 = "sobrepes"
  | imc>=30 && imc<40 = "obesitat"
  | otherwise = "obesitat morbida"
  where imc = m/(h^2)

                  
process l = putStrLn $ name ++ ": " ++ message m h
  where (name:xs) = words l
        (m:h:_) = map read $ xs :: [Float]
        

  