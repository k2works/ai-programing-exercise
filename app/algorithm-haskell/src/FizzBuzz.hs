module FizzBuzz
    ( generate
    , generateList
    , maxNumber
    ) where

-- | FizzBuzzの最大値
maxNumber :: Int
maxNumber = 100

-- | 数値を受け取ってFizzBuzz文字列を返す
generate :: Int -> String
generate n
    | isFizz && isBuzz = "FizzBuzz"
    | isFizz           = "Fizz"
    | isBuzz           = "Buzz"
    | otherwise        = show n
  where
    isFizz = n `mod` 3 == 0
    isBuzz = n `mod` 5 == 0

-- | 1からmaxNumberまでのFizzBuzz配列を生成
generateList :: [String]
generateList = map generate [1..maxNumber]
