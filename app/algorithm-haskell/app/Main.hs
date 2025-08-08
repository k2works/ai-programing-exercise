module Main (main) where

import BasicAlgorithms
import FizzBuzz
import ListAlgorithms
import SearchAlgorithms

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

  putStrLn "=== 第2章 配列（リスト） ==="
  putStrLn ""

  -- リストの最大値
  putStrLn "1. リストの最大値："
  putStrLn $ "maxOf [172, 153, 192, 140, 165] = " ++ show (maxOf [172, 153, 192, 140, 165])
  putStrLn $ "maxOf [-1, -5, -2] = " ++ show (maxOf [-1, -5, -2])
  putStrLn ""

  -- リストの反転
  putStrLn "2. リストの反転："
  let original = [2, 5, 1, 3, 9, 6, 7]
  putStrLn $ "original: " ++ show original
  putStrLn $ "reversed: " ++ show (reverseList original)
  putStrLn ""

  -- 基数変換
  putStrLn "3. 基数変換："
  putStrLn $ "cardConv 29 2  = " ++ cardConv 29 2 ++ " (2進数)"
  putStrLn $ "cardConv 29 16 = " ++ cardConv 29 16 ++ " (16進数)"
  putStrLn $ "cardConv 255 16 = " ++ cardConv 255 16 ++ " (16進数)"
  putStrLn ""

  -- 素数の列挙
  putStrLn "4. 素数の列挙："
  putStrLn $ "primes 20: " ++ show (primes 20)
  putStrLn $ "1000以下の素数の個数: " ++ show (length (primes 1000)) ++ "個"
  putStrLn ""

  putStrLn "=== FizzBuzz（既存実装） ==="
  putStrLn "FizzBuzz (1-20):"
  mapM_ putStrLn $ take 20 generateList

  putStrLn ""
  putStrLn "=== 第3章: 探索アルゴリズム ==="
  putStrLn ""
  
  -- 線形探索
  putStrLn "1. 線形探索（LinearSearch）："
  let testList = [6, 4, 3, 2, 1, 2, 8]
  putStrLn $ "リスト: " ++ show testList
  putStrLn $ "linearSearch 2 => " ++ show (linearSearch 2 testList)
  putStrLn $ "linearSearch 5 => " ++ show (linearSearch 5 testList)
  putStrLn $ "linearSearch 8 => " ++ show (linearSearch 8 testList)
  putStrLn ""
  
  -- 二分探索
  putStrLn "2. 二分探索（BinarySearch）："
  let sortedList = [1, 2, 3, 5, 7, 8, 9]
  putStrLn $ "ソート済みリスト: " ++ show sortedList
  putStrLn $ "binarySearch 5 => " ++ show (binarySearch 5 sortedList)
  putStrLn $ "binarySearch 6 => " ++ show (binarySearch 6 sortedList)
  putStrLn $ "binarySearch 1 => " ++ show (binarySearch 1 sortedList)
  putStrLn $ "binarySearch 9 => " ++ show (binarySearch 9 sortedList)
  putStrLn ""
  
  -- ハッシュテーブル（Data.Map）
  putStrLn "3. ハッシュテーブル（Data.Map）："
  let studentMap = createMap [(1, "赤尾"), (5, "武田"), (10, "小野")]
  putStrLn $ "学生マップ: " ++ show studentMap
  putStrLn $ "searchInMap 1  => " ++ show (searchInMap 1 studentMap)
  putStrLn $ "searchInMap 5  => " ++ show (searchInMap 5 studentMap)
  putStrLn $ "searchInMap 99 => " ++ show (searchInMap 99 studentMap)
  putStrLn ""
  
  let updatedMap = addToMap 12 "佐藤" studentMap
  putStrLn $ "addToMap 12 \"佐藤\" => " ++ show updatedMap
  putStrLn $ "searchInMap 12 => " ++ show (searchInMap 12 updatedMap)
  putStrLn ""
  
  let removedMap = removeFromMap 5 updatedMap
  putStrLn $ "removeFromMap 5 => " ++ show removedMap
  putStrLn $ "searchInMap 5 => " ++ show (searchInMap 5 removedMap)
