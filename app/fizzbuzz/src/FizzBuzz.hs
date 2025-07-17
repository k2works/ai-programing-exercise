module FizzBuzz (generate) where

generate :: Int -> String
generate number 
  | number `mod` 3 == 0 = "Fizz"
  | otherwise = show number
