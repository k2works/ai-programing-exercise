module ListAlgorithms
    ( maxOf
    , reverseList
    , cardConv
    , primes
    ) where

-- | リストの最大値を求める
-- 空リストに対してはエラーになります
maxOf :: (Ord a) => [a] -> a
maxOf = maximum

-- | リストの要素の並びを反転する
reverseList :: [a] -> [a]
reverseList = reverse

-- | 10進数の整数値を指定された基数に変換する
-- 2進数から36進数までサポート
cardConv :: Int -> Int -> String
cardConv x r
  | r < 2 || r > 36 = error "Base must be between 2 and 36"
  | x < 0 = error "Number must be non-negative"
  | x == 0 = "0"
  | otherwise = reverse $ go x
  where
    dchar = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    go 0 = ""
    go n = (dchar !! (n `mod` r)) : go (n `div` r)

-- | 指定された上限以下の素数をすべて列挙する
-- エラトステネスの篩を使用
primes :: Int -> [Int]
primes n
  | n < 2 = []
  | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
