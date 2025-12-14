-- ロット合格確率の計算

-- 基本データ
total :: Int
total = 100      -- 全部の個数

defective :: Int
defective = 10   -- 不良品の個数

good :: Int
good = total - defective  -- 良品 = 90

samples :: Int
samples = 3      -- 抜き取る個数

-- 方法1: 直接計算
probability1 :: Double
probability1 = p1 * p2 * p3
  where
    p1 = fromIntegral good / fromIntegral total           -- 90/100
    p2 = fromIntegral (good - 1) / fromIntegral (total - 1)  -- 89/99
    p3 = fromIntegral (good - 2) / fromIntegral (total - 2)  -- 88/98

-- 方法2: 再帰関数
calcProb :: Int -> Int -> Int -> Double
calcProb _ _ 0 = 1.0  -- 全部引き終わった
calcProb g t s = p * calcProb (g - 1) (t - 1) (s - 1)
  where
    p = fromIntegral g / fromIntegral t

probability2 :: Double
probability2 = calcProb good total samples

-- 方法3: foldr を使う
probability3 :: Double
probability3 = foldr (*) 1.0 probs
  where
    probs = [ fromIntegral (good - i) / fromIntegral (total - i)
            | i <- [0 .. samples - 1] ]

-- 方法4: リスト内包表記で各確率を見せる
eachProbability :: [Double]
eachProbability =
    [ fromIntegral (good - i) / fromIntegral (total - i)
    | i <- [0 .. samples - 1] ]

-- メイン
main :: IO ()
main = do
    putStrLn $ "方法1（直接計算）: " ++ show probability1
    putStrLn $ "方法2（再帰関数）: " ++ show probability2
    putStrLn $ "方法3（foldr使用）: " ++ show probability3
    putStrLn ""
    putStrLn "各ステップの確率:"
    putStrLn $ "  1個目: " ++ show (eachProbability !! 0)
    putStrLn $ "  2個目: " ++ show (eachProbability !! 1)
    putStrLn $ "  3個目: " ++ show (eachProbability !! 2)
    putStrLn ""
    putStrLn $ "合格確率: " ++ show (probability3 * 100) ++ "%"
