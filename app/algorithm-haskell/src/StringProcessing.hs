-- | 第7章: 文字列処理
-- String processing algorithms including brute force search and text operations
module StringProcessing
  ( -- * 基本的な文字列検索
    bruteForceSearch
  , bruteForceSearchManual
  , findAllOccurrences
  , findFirst
    -- * 文字列操作
  , capitalize
  , removeSpaces
  , reverseWords
  , isPalindrome
    -- * パターンマッチング
  , startsWith
  , endsWith
  , countOccurrences
    -- * 文字列変換
  , caesarCipher
  , rot13
  , charFrequency
  ) where

import Data.Char (toUpper, toLower, isSpace, ord, chr)
import Data.List (isPrefixOf, isSuffixOf, group, sort)

-- | 力まかせ法による文字列検索
-- Data.List.isInfixOf の実装
bruteForceSearch :: (Eq a) => [a] -> [a] -> Bool
bruteForceSearch needle haystack = needle `elem` (substrings haystack)
  where
    substrings [] = [[]]
    substrings xs@(_:ys) = take (length needle) xs : substrings ys

-- | 力まかせ法による文字列検索（手動実装）
-- パターンをテキストの各位置から順次比較
bruteForceSearchManual :: (Eq a) => [a] -> [a] -> Bool
bruteForceSearchManual [] _ = True  -- 空のパターンは常に見つかる
bruteForceSearchManual _ [] = False -- 空のテキストには何も見つからない
bruteForceSearchManual needle haystack@(_:hs)
  | needle `isPrefixOf` haystack = True
  | otherwise = bruteForceSearchManual needle hs

-- | 指定されたパターンのすべての出現位置を検索
findAllOccurrences :: (Eq a) => [a] -> [a] -> [Int]
findAllOccurrences needle haystack = findOccurrences needle haystack 0
  where
    findOccurrences [] _ pos = [pos]  -- 空のパターン
    findOccurrences _ [] _ = []        -- テキストが尽きた
    findOccurrences pattern text@(_:ts) pos
      | pattern `isPrefixOf` text = pos : findOccurrences pattern ts (pos + 1)
      | otherwise = findOccurrences pattern ts (pos + 1)

-- | 最初の出現位置を検索
findFirst :: (Eq a) => [a] -> [a] -> Maybe Int
findFirst needle haystack = case findAllOccurrences needle haystack of
  [] -> Nothing
  (x:_) -> Just x

-- | 文字列の最初の文字を大文字にする
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

-- | スペースを除去する
removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

-- | 単語を逆順にする
reverseWords :: String -> String
reverseWords = unwords . reverse . words

-- | 回文判定
isPalindrome :: String -> Bool
isPalindrome s = cleanStr == reverse cleanStr
  where
    cleanStr = map toLower $ filter (not . isSpace) s

-- | 指定された文字列で始まるかチェック
startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith = isPrefixOf

-- | 指定された文字列で終わるかチェック
endsWith :: (Eq a) => [a] -> [a] -> Bool
endsWith = isSuffixOf

-- | パターンの出現回数を数える
countOccurrences :: (Eq a) => [a] -> [a] -> Int
countOccurrences needle haystack = length $ findAllOccurrences needle haystack

-- | シーザー暗号
caesarCipher :: Int -> String -> String
caesarCipher shift = map (shiftChar shift)
  where
    shiftChar n c
      | c >= 'A' && c <= 'Z' = chr $ ((ord c - ord 'A' + n) `mod` 26) + ord 'A'
      | c >= 'a' && c <= 'z' = chr $ ((ord c - ord 'a' + n) `mod` 26) + ord 'a'
      | otherwise = c

-- | ROT13暗号（シーザー暗号のshift=13版）
rot13 :: String -> String
rot13 = caesarCipher 13

-- | 文字の出現頻度を計算
charFrequency :: String -> [(Char, Int)]
charFrequency s = map (\g -> (head g, length g)) $ group $ sort s
