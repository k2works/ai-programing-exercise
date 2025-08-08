-- |
-- Module: RecursionAlgorithms
-- Description: 第5章 再帰アルゴリズム
-- 
-- このモジュールでは、再帰を使った基本的なアルゴリズムから
-- より高度なバックトラッキングまで実装しています。
module RecursionAlgorithms
  ( -- * 基本的な再帰
    factorial
  , factorial'
  , gcd'
  
  -- * ハノイの塔
  , hanoi
  , Peg
  , Move
  
  -- * 8王妃問題
  , queens
  , Queen
  , Solution
  , isSafe
  ) where

-- | 階乗値を計算する（通常版）
-- 基底ケース: 0! = 1
-- 再帰ケース: n! = n × (n-1)!
factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1                    -- 基底ケース
factorial n = n * factorial (n - 1) -- 再帰ケース

-- | ユークリッドの互除法で最大公約数を求める
-- 基底ケース: gcd(x, 0) = x
-- 再帰ケース: gcd(x, y) = gcd(y, x `mod` y) (y > 0)
gcd' :: Integral a => a -> a -> a
gcd' x y
  | y == 0    = x                 -- 基底ケース
  | otherwise = gcd' y (x `mod` y) -- 再帰ケース

-- | 末尾再帰最適化版の階乗値計算
-- アキュムレータを使って末尾再帰にしています
factorial' :: (Eq p, Num p) => p -> p
factorial' n = go n 1
  where
    go 0 acc = acc             -- 基底ケース
    go m acc = go (m - 1) (m * acc) -- 末尾再帰

-- | ハノイの塔の型定義
type Peg = Int    -- 杭の番号
type Move = (Int, Peg, Peg) -- (円盤番号, 移動元杭, 移動先杭)

-- | ハノイの塔を解く
-- n枚の円盤を杭startから杭endに移動する手順を返す
hanoi :: Int -> Peg -> Peg -> [Move]
hanoi 0 _ _ = []               -- 基底ケース：0枚なら何もしない
hanoi n start end = 
  let other = 6 - start - end -- 中間地点の杭（1+2+3=6から計算）
  in hanoi (n - 1) start other ++  -- n-1枚を始点から中間へ
     [(n, start, end)] ++          -- n枚目を始点から終点へ
     hanoi (n - 1) other end       -- n-1枚を中間から終点へ

-- | 8王妃問題の型定義
type Queen = (Int, Int)  -- (行, 列)
type Solution = [Queen]  -- 1つの解（8個の女王の位置のリスト）

-- | n王妃問題を解く
-- すべての可能な解のリストを返す
queens :: Int -> [Solution]
queens n = solve n
  where
    solve :: Int -> [Solution]
    solve 0 = [[]]            -- 基底ケース：0個の女王の解は空の盤面1通り
    solve k = do
      -- k-1個の女王の解を再帰的に求める
      prevSolution <- solve (k - 1)
      -- 新しい女王の列（k列目）を1からnまで試す
      col <- [1..n]
      -- 新しい女王の位置
      let newQueen = (k, col)
      -- もし安全な位置なら、解に追加する
      if isSafe newQueen prevSolution
        then return (newQueen : prevSolution)
        else [] -- 安全でなければ、この探索パスは捨てる

-- | 新しい女王が既存の解に対して安全かどうかをチェック
-- 同じ列に配置されていないか、対角線上に配置されていないかを確認
isSafe :: Queen -> Solution -> Bool
isSafe _ [] = True               -- 空の盤面では常に安全
isSafe (r1, c1) (q:qs) =
  let (r2, c2) = q
  in c1 /= c2 &&                -- 同じ列でない
     abs (r1 - r2) /= abs (c1 - c2) && -- 対角線上でない
     isSafe (r1, c1) qs         -- 残りの女王ともチェック
