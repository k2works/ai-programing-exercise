module FizzBuzz.Domain.Model.FizzBuzzList (
  FizzBuzzList,
  createFizzBuzzList,
  getValues,
  addToList,
  getLength
) where

import FizzBuzz.Domain.Type.FizzBuzzError
import FizzBuzz.Domain.Model.FizzBuzzValue

-- ファーストクラスコレクション：FizzBuzzValueのリストをカプセル化
newtype FizzBuzzList = FizzBuzzList [FizzBuzzValue]
  deriving (Show, Eq)

-- スマートコンストラクタ：最大100件の制限
createFizzBuzzList :: [FizzBuzzValue] -> Either FizzBuzzError FizzBuzzList
createFizzBuzzList values
  | length values > 100 = Left (OutOfRange (length values) "上限は100件までです")
  | null values = Left (InvalidInput 0 "空のリストは無効です")
  | otherwise = Right (FizzBuzzList values)

-- FizzBuzzListからリストを取得
getValues :: FizzBuzzList -> [FizzBuzzValue]
getValues (FizzBuzzList values) = values

-- FizzBuzzListに値を追加（新しいリストを返す）
addToList :: FizzBuzzList -> [FizzBuzzValue] -> Either FizzBuzzError FizzBuzzList
addToList (FizzBuzzList existing) new = createFizzBuzzList (existing ++ new)

-- リストの長さを取得
getLength :: FizzBuzzList -> Int
getLength (FizzBuzzList values) = length values
