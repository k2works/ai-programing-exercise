module Main (main) where

import BasicAlgorithms
import FizzBuzz

main :: IO ()
main = do
  putStrLn "=== アルゴリズムから始めるHaskell入門 ==="
  putStrLn ""

  putStrLn "=== 第1章 基本的なアルゴリズム ==="
  putStrLn ""

  -- 3値の最大値
  putStrLn "1. 3値の最大値："
  putStrLn $ "max3 3 2 1 = " ++ show (max3 3 2 1)
  putStrLn $ "max3 1 3 2 = " ++ show (max3 1 3 2)
  putStrLn ""

  -- 3値の中央値
  putStrLn "2. 3値の中央値："
  putStrLn $ "med3 3 2 1 = " ++ show (med3 3 2 1)
  putStrLn $ "med3 1 3 2 = " ++ show (med3 1 3 2)
  putStrLn ""

  -- 符号判定
  putStrLn "3. 符号判定："
  putStrLn $ "judgeSign 17: " ++ judgeSign 17
  putStrLn $ "judgeSign (-5): " ++ judgeSign (-5)
  putStrLn $ "judgeSign 0: " ++ judgeSign 0
  putStrLn ""

  -- 1からnまでの総和
  putStrLn "4. 1からnまでの総和："
  putStrLn $ "sum1ToN 5 = " ++ show (sum1ToN 5)
  putStrLn $ "sum1ToN 10 = " ++ show (sum1ToN 10)
  putStrLn ""

  -- 記号文字の交互表示
  putStrLn "5. 記号文字の交互表示："
  putStrLn $ "alternative1 12: " ++ alternative1 12
  putStrLn $ "alternative2 12: " ++ alternative2 12
  putStrLn ""

  -- 長方形の辺の長さ
  putStrLn "6. 長方形の辺の長さ（面積32）："
  mapM_ putStrLn $ rectangle 32
  putStrLn ""

  -- 九九の表
  putStrLn "7. 九九の表："
  putStr multiplicationTable
  putStrLn ""

  -- 直角三角形
  putStrLn "8. 左下直角三角形（高さ5）："
  putStr $ triangleLb 5
  putStrLn ""

  putStrLn "=== FizzBuzz（既存実装） ==="
  putStrLn "FizzBuzz (1-20):"
  mapM_ putStrLn $ take 20 generateList
