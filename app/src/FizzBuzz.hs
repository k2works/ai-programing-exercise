module FizzBuzz (generate) where

-- 3と5両方の倍数はFizzBuzz、3の倍数はFizz、5の倍数はBuzzを返す
generate :: Int -> String
generate n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n
