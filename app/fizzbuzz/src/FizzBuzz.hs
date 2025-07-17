module FizzBuzz (generate) where

generate :: Int -> String
generate number
  | number `mod` 15 == 0 = "FizzBuzz"
  | number `mod` 3 == 0 = "Fizz"
  | number `mod` 5 == 0 = "Buzz"
  | otherwise = show number