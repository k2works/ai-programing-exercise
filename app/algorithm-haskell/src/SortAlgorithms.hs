-- |
-- Module: SortAlgorithms
-- Description: 第6章 ソートアルゴリズム
-- 
-- このモジュールでは、様々なソートアルゴリズムを関数型スタイルで実装しています。
-- バブルソート、選択ソート、挿入ソート、クイックソート、マージソートを扱います。
module SortAlgorithms
  ( -- * 基本的なソート
    bubbleSort
  , selectionSort
  , insertionSort
  
  -- * 効率的なソート
  , quickSort
  , mergeSort
  
  -- * ヘルパー関数
  , merge
  , bubble
  , insert'
  ) where

import Data.List (delete)

-- | バブルソート: 隣接する要素を比較し、順序が逆であれば交換することを繰り返す
-- 時間計算量: O(n²)
-- 空間計算量: O(1)
-- 安定性: 安定
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = go (length xs) xs
  where
    go 0 ys = ys
    go n ys = go (n - 1) (bubble ys)

-- | 1回の走査で最大の要素を末尾に移動
bubble :: (Ord a) => [a] -> [a]
bubble [] = []
bubble [y] = [y]
bubble (y1:y2:ys)
  | y1 > y2   = y2 : bubble (y1:ys)
  | otherwise = y1 : bubble (y2:ys)

-- | 選択ソート: 未ソート部分から最小の要素を見つけ、結果リストの先頭に追加
-- 時間計算量: O(n²)
-- 空間計算量: O(n)
-- 安定性: 不安定
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs =
  let minVal = minimum xs
  in minVal : selectionSort (delete minVal xs)

-- | 挿入ソート: 未ソート部分の先頭要素をソート済み結果リストの適切な位置に挿入
-- 時間計算量: O(n²) （最良の場合: O(n)）
-- 空間計算量: O(n)
-- 安定性: 安定
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert' []

-- | 要素をソート済みリストの適切な位置に挿入
insert' :: (Ord a) => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert' x ys

-- | クイックソート: 分割統治法に基づく効率的なソート
-- ピボットを基準にリストを3つに分割し、再帰的にソート
-- 時間計算量: O(n log n) 平均、O(n²) 最悪
-- 空間計算量: O(log n) 平均、O(n) 最悪
-- 安定性: 不安定
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []               -- 基底ケース
quickSort (p:xs) =              -- pをピボットとする
  let smaller = quickSort [x | x <- xs, x <= p]  -- ピボット以下の要素
      larger  = quickSort [x | x <- xs, x > p]   -- ピボットより大きい要素
  in smaller ++ [p] ++ larger

-- | マージソート: 分割統治法に基づく安定なソート
-- リストを半分に分割し、それぞれをソートしてからマージ
-- 時間計算量: O(n log n)
-- 空間計算量: O(n)
-- 安定性: 安定
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let (left, right) = splitAt (length xs `div` 2) xs
  in merge (mergeSort left) (mergeSort right)

-- | 2つのソート済みリストをマージして1つのソート済みリストを作成
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
