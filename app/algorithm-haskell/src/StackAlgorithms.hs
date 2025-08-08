module StackAlgorithms
  ( Stack
  , isEmpty
  , push
  , pop
  , peek
  , popSafe
  , peekSafe
  ) where

-- | 型エイリアスでStackを定義
type Stack a = [a]

-- | スタックが空かどうか
isEmpty :: Stack a -> Bool
isEmpty = null

-- | プッシュ - スタックの先頭に要素を追加
push :: a -> Stack a -> Stack a
push = (:)

-- | ポップ - スタックの先頭から要素を取り出し、残りのスタックを返す（安全でないバージョン）
pop :: Stack a -> (a, Stack a)
pop [] = error "pop from empty stack"
pop (x:xs) = (x, xs)

-- | ピーク - スタックの先頭の要素を参照する（取り出さない、安全でないバージョン）
peek :: Stack a -> a
peek [] = error "peek from empty stack"
peek (x:_) = x

-- | 安全なポップ - Maybeを使って空スタックでのエラーを防ぐ
popSafe :: Stack a -> Maybe (a, Stack a)
popSafe [] = Nothing
popSafe (x:xs) = Just (x, xs)

-- | 安全なピーク - Maybeを使って空スタックでのエラーを防ぐ
peekSafe :: Stack a -> Maybe a
peekSafe [] = Nothing
peekSafe (x:_) = Just x
