module FizzBuzz.Domain.Model.FizzBuzzValue (
  FizzBuzzValue(..),
  createFizzBuzzValue,
  getValue,
  getNumber
) where

import FizzBuzz.Domain.Type.FizzBuzzError

-- 値オブジェクト：数値と結果文字列の不変データ構造
data FizzBuzzValue = FizzBuzzValue 
  { number :: Int
  , value :: String
  } deriving (Show, Eq)

-- スマートコンストラクタ：純粋な検証とデータ構築
createFizzBuzzValue :: Int -> String -> Either FizzBuzzError FizzBuzzValue
createFizzBuzzValue num val = validateInput num >>= constructValue
  where
    validateInput n
      | n <= 0    = Left (InvalidInput n "正の値のみ有効です")
      | otherwise = Right n
    constructValue n = Right (FizzBuzzValue n val)

-- レンズ風純粋アクセサ：データ射影の関数抽象化
getValue :: FizzBuzzValue -> String
getValue = value

getNumber :: FizzBuzzValue -> Int
getNumber = number
