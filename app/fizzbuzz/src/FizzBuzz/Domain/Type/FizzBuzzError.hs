module FizzBuzz.Domain.Type.FizzBuzzError (
  FizzBuzzError(..)
) where

-- カスタムエラー型
data FizzBuzzError 
  = InvalidInput Int String
  | OutOfRange Int String
  | ListTooLarge Int String
  deriving (Show, Eq)
