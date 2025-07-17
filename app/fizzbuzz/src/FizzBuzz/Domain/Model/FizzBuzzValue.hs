module FizzBuzz.Domain.Model.FizzBuzzValue (
  FizzBuzzValue(..),
  createFizzBuzzValue,
  getValue,
  getNumber
) where

import FizzBuzz.Domain.Type.FizzBuzzError

-- 値オブジェクト：数値と結果文字列をカプセル化
data FizzBuzzValue = FizzBuzzValue 
  { number :: Int
  , value :: String
  } deriving (Show, Eq)

-- スマートコンストラクタ
createFizzBuzzValue :: Int -> String -> Either FizzBuzzError FizzBuzzValue
createFizzBuzzValue num val
  | num <= 0 = Left (InvalidInput num "正の値のみ有効です")
  | otherwise = Right (FizzBuzzValue num val)

-- 安全なアクセサ
getValue :: FizzBuzzValue -> String
getValue = value

getNumber :: FizzBuzzValue -> Int
getNumber = number
