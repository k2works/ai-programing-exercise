-- | 第8章: リスト構造
-- Advanced list structures including circular lists and zipper pattern
module AdvancedLists
  ( -- * 循環リストと遅延評価
    cyclicListManual
  , cyclicFromList
  , takeFromCyclic
    -- * Zipperパターン（双方向リスト）
  , ListZipper
  , fromList
  , toList
  , getFocus
  , goForward
  , goBackward
  , goToPosition
  , modify
  , insertHere
  , deleteHere
    -- * リスト操作の分析
  , ListOperation(..)
  , listOperationComplexity
  , measureListOperation
    -- * 高度なリスト生成
  , fibonacciList
  , primesList
  , powerList
    -- * リスト変換とユーティリティ
  , chunksOf
  , interleave
  , slidingWindow
  , rotateLeft
  , rotateRight
  ) where

-- | リストZipperの定義
-- 構造: (注目点より前の要素(逆順), 注目点以降の要素)
type ListZipper a = ([a], [a])

-- | 手動で定義した循環リスト（1,2,3の無限繰り返し）
cyclicListManual :: [Integer]
cyclicListManual = 1 : 2 : 3 : cyclicListManual

-- | 有限リストから循環リストを生成
cyclicFromList :: [a] -> [a]
cyclicFromList [] = error "Cannot create cyclic list from empty list"
cyclicFromList xs = cycle xs

-- | 循環リストから指定数の要素を取得
takeFromCyclic :: Int -> [a] -> [a]
takeFromCyclic n xs = take n (cycle xs)

-- | リストからZipperを作成（注目点は先頭）
fromList :: [a] -> ListZipper a
fromList xs = ([], xs)

-- | ZipperからリストEUを復元
toList :: ListZipper a -> [a]
toList (ls, rs) = reverse ls ++ rs

-- | 現在の注目点の要素を取得
-- | Zipperの現在のフォーカス要素を取得
getFocus :: ListZipper a -> Maybe a
getFocus (_, []) = Nothing
getFocus (_, x:_) = Just x

-- | 注目点を一つ前方（右）に移動
goForward :: ListZipper a -> ListZipper a
goForward (ls, []) = (ls, [])  -- 末尾では動かない
goForward (ls, r:rs) = (r:ls, rs)

-- | 注目点を一つ後方（左）に移動
goBackward :: ListZipper a -> ListZipper a
goBackward ([], rs) = ([], rs)  -- 先頭では動かない
goBackward (l:ls, rs) = (ls, l:rs)

-- | 指定位置に注目点を移動
goToPosition :: Int -> ListZipper a -> ListZipper a
goToPosition pos zipper = goToPos pos (fromList (toList zipper))
  where
    goToPos 0 z = z
    goToPos n z@(_, []) = z  -- 範囲外
    goToPos n z | n < 0 = z  -- 負の位置
    goToPos n z = goToPos (n-1) (goForward z)

-- | 注目点の要素を変更
modify :: (a -> a) -> ListZipper a -> ListZipper a
modify _ (ls, []) = (ls, [])
modify f (ls, r:rs) = (ls, f r : rs)

-- | 注目点に新しい要素を挿入
insertHere :: a -> ListZipper a -> ListZipper a
insertHere x (ls, rs) = (ls, x:rs)

-- | 注目点の要素を削除
deleteHere :: ListZipper a -> ListZipper a
deleteHere (ls, []) = (ls, [])
deleteHere (ls, _:rs) = (ls, rs)

-- | リスト操作の計算量を説明する型
data ListOperation = PrependOp | AppendOp | HeadOp | TailOp | IndexOp | LengthOp
  deriving (Show, Eq)

-- | リスト操作の計算量情報
listOperationComplexity :: ListOperation -> String
listOperationComplexity PrependOp = "O(1) - 先頭への追加"
listOperationComplexity AppendOp = "O(n) - 末尾への追加"
listOperationComplexity HeadOp = "O(1) - 先頭要素の取得"
listOperationComplexity TailOp = "O(1) - 先頭要素の除去"
listOperationComplexity IndexOp = "O(n) - インデックスアクセス"
listOperationComplexity LengthOp = "O(n) - 長さの取得"

-- | リスト操作の実行時間を測定（デモ用）
measureListOperation :: ListOperation -> [Int] -> String
measureListOperation PrependOp xs = "Prepending element: " ++ show (0 : xs)
measureListOperation AppendOp xs = "Appending element: " ++ show (xs ++ [length xs])
measureListOperation HeadOp [] = "Head of empty list: error"
measureListOperation HeadOp (x:_) = "Head: " ++ show x
measureListOperation TailOp [] = "Tail of empty list: error"
measureListOperation TailOp (_:xs) = "Tail: " ++ show xs
measureListOperation IndexOp xs = "Element at index 2: " ++ show (if length xs > 2 then xs !! 2 else error "Index out of bounds")
measureListOperation LengthOp xs = "Length: " ++ show (length xs)

-- | フィボナッチ数列の無限リスト
fibonacciList :: [Integer]
fibonacciList = 0 : 1 : zipWith (+) fibonacciList (tail fibonacciList)

-- | 素数の無限リスト（エラトステネスの篩）
primesList :: [Integer]
primesList = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []

-- | 指定数の冪乗列
powerList :: Integer -> [Integer]
powerList base = iterate (*base) 1

-- | リストを指定サイズのチャンクに分割
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | 2つのリストを交互に結合
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- | スライディングウィンドウ
slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs
  | length xs < n = []
  | otherwise = take n xs : slidingWindow n (tail xs)

-- | リストを左にn個ローテート
rotateLeft :: Int -> [a] -> [a]
rotateLeft _ [] = []
rotateLeft n xs = drop (n `mod` length xs) xs ++ take (n `mod` length xs) xs

-- | リストを右にn個ローテート
rotateRight :: Int -> [a] -> [a]
rotateRight n xs = rotateLeft (length xs - (n `mod` length xs)) xs
