-- NANDゲート
nand :: Bool -> Bool -> Bool
nand a b = not (a && b)

-- 回路の構造
circuit :: Bool -> Bool -> Bool
circuit x y = nand top bottom
  where
    top    = nand x x   -- 上のNAND（Xの反対）
    bottom = nand y y   -- 下のNAND（Yの反対）

-- 真理値表を作る
truthTable :: [(Bool, Bool, Bool, Bool)]
truthTable =
    [ (x, y, circuit x y, x || y)
    | x <- [False, True]
    , y <- [False, True]
    ]

-- 結果を表示
main :: IO ()
main = mapM_ printRow truthTable
  where
    printRow (x, y, z, orResult) =
        putStrLn $ "X=" ++ show x ++ " Y=" ++ show y ++
                   " → Z=" ++ show z ++ " (OR=" ++ show orResult ++ ")"
