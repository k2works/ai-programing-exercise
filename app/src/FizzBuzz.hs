module FizzBuzz (generate) where

import Data.Monoid (First(..), getFirst)
import Data.Maybe (fromMaybe)

-- 関数型アプローチによるFizzBuzz実装：関数の合成とMonoidを活用
generate :: Int -> String
generate n = fromMaybe (show n) $ getFirst $ foldMap (First . ($ n)) rules

-- FizzBuzzのルールを関数のリストとして定義
rules :: [Int -> Maybe String]
rules = [ fizzbuzzRule
        , fizzRule
        , buzzRule
        ]

-- 各ルールを純粋関数として定義
fizzbuzzRule :: Int -> Maybe String
fizzbuzzRule n = if isDivisibleBy 15 n then Just "FizzBuzz" else Nothing

fizzRule :: Int -> Maybe String  
fizzRule n = if isDivisibleBy 3 n then Just "Fizz" else Nothing

buzzRule :: Int -> Maybe String
buzzRule n = if isDivisibleBy 5 n then Just "Buzz" else Nothing

-- 除算可能性をチェックする純粋関数
isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy divisor number = number `mod` divisor == 0
