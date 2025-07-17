module FizzBuzz (FizzBuzzType(..), generate) where

data FizzBuzzType = Type1 | Type2 | Type3 | TypeOther Int
  deriving (Show, Eq)

generate :: FizzBuzzType -> Int -> String
generate Type1 number
  | number `mod` 15 == 0 = "FizzBuzz"
  | number `mod` 3 == 0 = "Fizz"
  | number `mod` 5 == 0 = "Buzz"
  | otherwise = show number
generate Type2 number = show number
generate Type3 number
  | number `mod` 15 == 0 = "FizzBuzz"
  | otherwise = show number
generate (TypeOther _) _ = ""