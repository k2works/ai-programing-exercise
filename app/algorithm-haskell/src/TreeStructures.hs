-- | 第9章: 木構造
-- Binary trees and binary search trees implementation
module TreeStructures
  ( -- * 二分木の基本型
    Tree(..)
    -- * 二分探索木の操作
  , treeInsert
  , treeElem
  , treeDelete
  , treeSize
  , treeHeight
  , treeMin
  , treeMax
    -- * 木の走査
  , inOrderTraversal
  , preOrderTraversal
  , postOrderTraversal
  , treeToList
    -- * 木の構築
  , fromList
  , singleton
    -- * 平衡性チェック
  , isBalanced
  , balanceFactor
    -- * 標準ライブラリとの連携
  , toSet
  , fromSet
  , toMap
  , fromMap
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (foldl')

-- | 二分木のデータ型
-- EmptyTree: 空の木
-- Node: 値と左右の子木を持つノード
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
  deriving (Show, Eq, Ord)

-- | 二分探索木への要素挿入
-- 二分探索木の性質を保ちながら新しい要素を挿入
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node val left right)
  | x == val  = Node x left right          -- 同じ値なら何もしない
  | x < val   = Node val (treeInsert x left) right   -- 小さければ左の子に挿入
  | otherwise = Node val left (treeInsert x right)   -- 大きければ右の子に挿入

-- | 二分探索木での要素検索
-- 二分探索木の性質を利用した効率的な検索
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node val left right)
  | x == val  = True                       -- 見つかった
  | x < val   = treeElem x left           -- 小さければ左を探す
  | otherwise = treeElem x right          -- 大きければ右を探す

-- | 二分探索木からの要素削除
-- 3つのケースに対応：葉ノード、片子ノード、両子ノード
treeDelete :: (Ord a) => a -> Tree a -> Tree a
treeDelete _ EmptyTree = EmptyTree
treeDelete x (Node val left right)
  | x < val   = Node val (treeDelete x left) right
  | x > val   = Node val left (treeDelete x right)
  | otherwise = deleteNode (Node val left right)
  where
    deleteNode (Node _ EmptyTree EmptyTree) = EmptyTree        -- 葉ノード
    deleteNode (Node _ left EmptyTree) = left                  -- 右の子が空
    deleteNode (Node _ EmptyTree right) = right                -- 左の子が空
    deleteNode (Node _ left right) =                           -- 両方の子がある
      let minVal = treeMin right
      in Node minVal left (treeDelete minVal right)

-- | 木のサイズ（ノード数）を計算
treeSize :: Tree a -> Int
treeSize EmptyTree = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

-- | 木の高さを計算
treeHeight :: Tree a -> Int
treeHeight EmptyTree = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

-- | 木の最小値を取得
treeMin :: Tree a -> a
treeMin EmptyTree = error "Empty tree has no minimum"
treeMin (Node val EmptyTree _) = val
treeMin (Node _ left _) = treeMin left

-- | 木の最大値を取得
treeMax :: Tree a -> a
treeMax EmptyTree = error "Empty tree has no maximum"
treeMax (Node val _ EmptyTree) = val
treeMax (Node _ _ right) = treeMax right

-- | 中間順走査（in-order traversal）
-- 左の子 → ルート → 右の子の順で訪問
-- 二分探索木では昇順にソートされた結果が得られる
inOrderTraversal :: Tree a -> [a]
inOrderTraversal EmptyTree = []
inOrderTraversal (Node val left right) =
  inOrderTraversal left ++ [val] ++ inOrderTraversal right

-- | 前順走査（pre-order traversal）
-- ルート → 左の子 → 右の子の順で訪問
preOrderTraversal :: Tree a -> [a]
preOrderTraversal EmptyTree = []
preOrderTraversal (Node val left right) =
  [val] ++ preOrderTraversal left ++ preOrderTraversal right

-- | 後順走査（post-order traversal）
-- 左の子 → 右の子 → ルートの順で訪問
postOrderTraversal :: Tree a -> [a]
postOrderTraversal EmptyTree = []
postOrderTraversal (Node val left right) =
  postOrderTraversal left ++ postOrderTraversal right ++ [val]

-- | 木をリストに変換（中間順走査）
treeToList :: Tree a -> [a]
treeToList = inOrderTraversal

-- | リストから二分探索木を構築
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl' (flip treeInsert) EmptyTree

-- | 単一要素の木を作成
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- | 木が平衡しているかチェック
-- 各ノードで左右の子木の高さの差が1以下かを確認
isBalanced :: Tree a -> Bool
isBalanced EmptyTree = True
isBalanced (Node _ left right) =
  abs (treeHeight left - treeHeight right) <= 1 &&
  isBalanced left && isBalanced right

-- | ノードの平衡係数を計算
-- 右の子木の高さ - 左の子木の高さ
balanceFactor :: Tree a -> Int
balanceFactor EmptyTree = 0
balanceFactor (Node _ left right) = treeHeight right - treeHeight left

-- | 木をData.Setに変換
toSet :: (Ord a) => Tree a -> Set.Set a
toSet = Set.fromList . treeToList

-- | Data.Setから木を構築
fromSet :: (Ord a) => Set.Set a -> Tree a
fromSet = fromList . Set.toList

-- | 木をData.Mapに変換（値をキーとして使用）
toMap :: (Ord a) => Tree a -> Map.Map a ()
toMap tree = Map.fromList [(x, ()) | x <- treeToList tree]

-- | Data.Mapから木を構築（キーのみを使用）
fromMap :: (Ord k) => Map.Map k v -> Tree k
fromMap = fromList . Map.keys
