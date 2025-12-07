-- ============================================
-- 線形探索と2分探索の比較回数を確認するHaskellコード
-- ============================================

import Data.Array (Array, (!), listArray, bounds)

-- | 探索結果: (見つかった位置, 比較回数)
type SearchResult = (Maybe Int, Int)

-- ============================================
-- 線形探索（前から順番に探す）
-- ============================================
--   [1] [2] [3] [4] [5]...
--    ↑   ↑   ↑
--   1回 2回 3回...と順番に見ていく

linearSearch :: [Int] -> Int -> SearchResult
linearSearch xs target = go xs 0
  where
    go [] count = (Nothing, count)  -- 見つからなかった
    go (y:ys) count
      | y == target = (Just count, count + 1)  -- 見つかった！
      | otherwise   = go ys (count + 1)        -- 次を見る

-- ============================================
-- 2分探索（半分に分けながら探す）
-- ============================================
--   真ん中を見て、大きいか小さいかで
--   半分だけ探す → どんどん半分になるから速い！

binarySearch :: Array Int Int -> Int -> SearchResult
binarySearch arr target = go lo hi 0
  where
    (lo, hi) = bounds arr

    go left right count
      | left > right = (Nothing, count)  -- 見つからなかった
      | arr ! mid == target = (Just mid, count + 1)  -- 見つかった！
      | arr ! mid < target  = go (mid + 1) right (count + 1)  -- 右を探す
      | otherwise           = go left (mid - 1) (count + 1)   -- 左を探す
      where
        mid = (left + right) `div` 2

-- ============================================
-- 理論値の計算
-- ============================================

-- 線形探索: 最良1回、最悪n回、平均(n+1)/2回
theoreticalLinear :: Int -> (Int, Double, Int)
theoreticalLinear n = (1, fromIntegral (n + 1) / 2.0, n)

-- 2分探索: 平均[log₂n]回、最大[log₂n]+1回
theoreticalBinary :: Int -> (Int, Int, Int)
theoreticalBinary n = (1, floor logN, floor logN + 1)
  where
    logN = logBase 2 (fromIntegral n)

-- ============================================
-- 実行例
-- ============================================

main :: IO ()
main = do
    let dataList = [1..100]
    let n = length dataList
    let dataArray = listArray (0, n-1) dataList

    -- 50を探す
    let (_, linCount) = linearSearch dataList 50
    let (_, binCount) = binarySearch dataArray 50

    putStrLn $ "50を探す: 線形=" ++ show linCount ++ "回, 2分=" ++ show binCount ++ "回"

    -- 理論値
    let (_, linAvg, _) = theoreticalLinear n
    let (_, binAvg, binWorst) = theoreticalBinary n

    putStrLn $ "理論値: 線形平均=" ++ show linAvg ++ "回, 2分平均=" ++ show binAvg ++ "回, 2分最悪=" ++ show binWorst ++ "回"
