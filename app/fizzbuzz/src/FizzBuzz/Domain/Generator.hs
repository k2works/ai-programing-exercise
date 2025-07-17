module FizzBuzz.Domain.Generator (
  generateValueSafe,
  generateValue,
  generateList
) where

import FizzBuzz.Domain.Type.FizzBuzzType
import FizzBuzz.Domain.Type.FizzBuzzError
import FizzBuzz.Domain.Model.FizzBuzzValue
import FizzBuzz.Domain.Model.FizzBuzzList

-- 安全な値オブジェクト生成関数
generateValueSafe :: FizzBuzzType -> Int -> Either FizzBuzzError FizzBuzzValue
generateValueSafe _ num
  | num <= 0 = Left (InvalidInput num "正の値のみ有効です")
generateValueSafe Type1 num
  | num `mod` 15 == 0 = Right (FizzBuzzValue num "FizzBuzz")
  | num `mod` 3 == 0 = Right (FizzBuzzValue num "Fizz")
  | num `mod` 5 == 0 = Right (FizzBuzzValue num "Buzz")
  | otherwise = Right (FizzBuzzValue num (show num))
generateValueSafe Type2 num = Right (FizzBuzzValue num (show num))
generateValueSafe Type3 num
  | num `mod` 15 == 0 = Right (FizzBuzzValue num "FizzBuzz")
  | otherwise = Right (FizzBuzzValue num (show num))
generateValueSafe (TypeOther _) num = Right (FizzBuzzValue num "")

-- 非安全版（互換性のため）
generateValue :: FizzBuzzType -> Int -> FizzBuzzValue
generateValue fizzbuzzType num = 
  case generateValueSafe fizzbuzzType num of
    Right val -> val
    Left _ -> FizzBuzzValue num ""  -- エラー時は空文字列

-- リストを生成する関数
generateList :: FizzBuzzType -> Int -> Either FizzBuzzError FizzBuzzList
generateList _ count
  | count > 100 = Left (OutOfRange count "上限は100件までです")
  | count < 1 = Left (InvalidInput count "1以上の値を指定してください")
generateList fizzbuzzType count = 
  case sequence [generateValueSafe fizzbuzzType n | n <- [1..count]] of
    Right values -> createFizzBuzzList values
    Left err -> Left err
