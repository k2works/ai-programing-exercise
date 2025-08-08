module BasicAlgorithms
    ( max3
    , med3
    , judgeSign
    , sum1ToN
    , alternative1
    , alternative2
    , rectangle
    , multiplicationTable
    , triangleLb
    ) where

import Text.Printf (printf)

-- 3値の最大値を求める関数
max3 :: (Ord a) => a -> a -> a -> a
max3 a b c = max a (max b c)

-- 3値の中央値を求める関数
med3 :: (Ord a) => a -> a -> a -> a
med3 a b c
  | (a >= b && b >= c) || (c >= b && b >= a) = b
  | (b >= a && a >= c) || (c >= a && a >= b) = a
  | otherwise = c

-- 整数値の符号を判定する関数
judgeSign :: (Num a, Ord a) => a -> String
judgeSign n
  | n > 0     = "その値は正です。"
  | n < 0     = "その値は負です。"
  | otherwise = "その値は0です。"

-- 1からnまでの総和を求める関数
sum1ToN :: (Num a, Enum a) => a -> a
sum1ToN n = sum [1..n]

-- 記号文字の交互表示（cycle使用）
alternative1 :: Int -> String
alternative1 n = take n (cycle "+-")

-- 記号文字の交互表示（リスト内包表記使用）
alternative2 :: Int -> String
alternative2 n = [if even i then '+' else '-' | i <- [0..n-1]]

-- 長方形の辺の長さを列挙する関数
rectangle :: Int -> [String]
rectangle area =
  [ show i ++ "x" ++ show (area `div` i)
  | i <- [1..isqrt area]
  , area `mod` i == 0
  ]
  where
    isqrt :: Int -> Int
    isqrt = floor . sqrt . fromIntegral

-- 九九の表を生成する関数
multiplicationTable :: String
multiplicationTable = unlines [row i | i <- [1..9 :: Int]]
  where
    row i = unwords [printf "%2d" (i * j) | j <- [1..9 :: Int]]

-- 左下が直角の二等辺三角形を表示する関数
triangleLb :: Int -> String
triangleLb n = unlines [replicate i '*' | i <- [1..n]]
