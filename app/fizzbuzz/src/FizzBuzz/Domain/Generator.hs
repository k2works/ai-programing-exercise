module FizzBuzz.Domain.Generator (
  generateValueSafe,
  generateValue,
  generateList,
  generateRange,
  fizzbuzzRule,
  applyRule
) where

import FizzBuzz.Domain.Type.FizzBuzzType
import FizzBuzz.Domain.Type.FizzBuzzError
import FizzBuzz.Domain.Model.FizzBuzzValue
import FizzBuzz.Domain.Model.FizzBuzzList
import Control.Monad (when)

-- | 関数型：FizzBuzz ルールを表現する関数型
type FizzBuzzRule = Int -> String

-- | 純粋関数：数値に対するFizzBuzzルールの適用
fizzbuzzRule :: FizzBuzzType -> FizzBuzzRule
fizzbuzzRule Type1 = \num ->
  case (num `mod` 3, num `mod` 5) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    _      -> show num
fizzbuzzRule Type2 = show
fizzbuzzRule Type3 = \num ->
  if num `mod` 15 == 0 then "FizzBuzz" else show num
fizzbuzzRule (TypeOther _) = const ""

-- | 高階関数：ルールを値オブジェクトに適用
applyRule :: FizzBuzzRule -> Int -> FizzBuzzValue
applyRule rule num = FizzBuzzValue num (rule num)

-- | 関数合成：安全な値オブジェクト生成
generateValueSafe :: FizzBuzzType -> Int -> Either FizzBuzzError FizzBuzzValue
generateValueSafe fizzbuzzType num = do
  when (num <= 0) $ Left (InvalidInput num "正の値のみ有効です")
  let rule = fizzbuzzRule fizzbuzzType
  return $ applyRule rule num

-- | 部分適用を活用した非安全版（互換性のため）
generateValue :: FizzBuzzType -> Int -> FizzBuzzValue
generateValue fizzbuzzType num = 
  either (const $ FizzBuzzValue num "") id $ generateValueSafe fizzbuzzType num

-- | モナド合成：範囲に対するリスト生成
generateRange :: FizzBuzzType -> Int -> Int -> Either FizzBuzzError [FizzBuzzValue]
generateRange fizzbuzzType start end = do
  when (start < 1) $ Left (InvalidInput start "開始値は1以上である必要があります")
  when (end < start) $ Left (InvalidInput end "終了値は開始値以上である必要があります")
  when (end - start + 1 > 100) $ Left (OutOfRange (end - start + 1) "範囲は100件までです")
  traverse (generateValueSafe fizzbuzzType) [start..end]

-- | リファクタリング：generateListをgenerateRangeで実装
generateList :: FizzBuzzType -> Int -> Either FizzBuzzError FizzBuzzList
generateList fizzbuzzType count = 
  generateRange fizzbuzzType 1 count >>= createFizzBuzzList
