module FizzBuzz (generate) where

generate :: Int -> String
generate number =
  case (number `mod` 3 == 0) of
    True -> "Fizz"
    False -> show number