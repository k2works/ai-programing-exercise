module FizzBuzz.Domain.Model.FizzBuzzList (
  FizzBuzzList,
  createFizzBuzzList,
  getValues,
  addToList,
  getLength
) where

import FizzBuzz.Domain.Type.FizzBuzzError
import FizzBuzz.Domain.Model.FizzBuzzValue

-- ファーストクラスコレクション：FizzBuzzValueの不変リスト抽象化
newtype FizzBuzzList = FizzBuzzList [FizzBuzzValue]
  deriving (Show, Eq)

-- 検証述語の関数抽象化
type ValidationPredicate a = a -> Bool
type ValidationRule a = (ValidationPredicate a, FizzBuzzError)

-- スマートコンストラクタ：モナド組み合わせによる複合検証
createFizzBuzzList :: [FizzBuzzValue] -> Either FizzBuzzError FizzBuzzList
createFizzBuzzList values = validateWith rules values >>= pure . FizzBuzzList
  where
    rules = [ (not . null, InvalidInput 0 "空のリストは無効です")
            , ((<= 100) . length, OutOfRange (length values) "範囲は100件までです")
            ]

-- 汎用検証関数：高階関数による抽象化
validateWith :: [ValidationRule a] -> a -> Either FizzBuzzError a
validateWith rules val = foldl checkRule (Right val) rules
  where
    checkRule acc (predicate, err) = acc >>= \v -> 
      if predicate v then Right v else Left err

-- 純粋射影：新型ラッパーからの安全なデータ抽出
getValues :: FizzBuzzList -> [FizzBuzzValue]
getValues (FizzBuzzList values) = values

-- モナド結合による不変リスト操作
addToList :: FizzBuzzList -> [FizzBuzzValue] -> Either FizzBuzzError FizzBuzzList
addToList (FizzBuzzList existing) new = createFizzBuzzList (existing <> new)

-- 関数合成による長さ計算
getLength :: FizzBuzzList -> Int
getLength = length . getValues
