module FizzBuzz.Domain.Type.FizzBuzzType (
  FizzBuzzType(..)
) where

data FizzBuzzType = Type1 | Type2 | Type3 | TypeOther Int
  deriving (Show, Eq)
