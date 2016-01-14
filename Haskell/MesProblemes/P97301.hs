-- fizz buzz
fizzBuzz :: [Either Int String]
fizzBuzz = map fizz [0..]
  where fizz n
         | (mod n 15)==0 = Right "FizzBuzz"
         | (mod n 5)==0 = Right "Buzz"
         | (mod n 3)==0 = Right "Fizz"
         | otherwise = Left n