module Main (main) where

import AdvancedLists
import BasicAlgorithms
import FizzBuzz
import ListAlgorithms
import SearchAlgorithms
import StackAlgorithms
import QueueAlgorithms
import RecursionAlgorithms
import SortAlgorithms
import StringProcessing
import TreeStructures (Tree(..), treeInsert, treeElem, treeDelete, treeSize, treeHeight, 
                      treeMin, treeMax, inOrderTraversal, preOrderTraversal, 
                      postOrderTraversal, treeToList, singleton, isBalanced, 
                      balanceFactor, toSet, fromSet, toMap, fromMap)
import qualified TreeStructures as Tree
import qualified Data.Set as Set
import qualified Data.Map as Map

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

  putStrLn ""
  putStrLn "=== 第4章: スタックとキュー ==="
  putStrLn ""
  
  -- スタック
  putStrLn "1. スタック（Stack - LIFO）："
  let stack0 = [] :: Stack Int
  putStrLn $ "空のスタック: " ++ show stack0
  putStrLn $ "isEmpty stack0 => " ++ show (isEmpty stack0)
  
  let stack1 = push 1 stack0
  let stack2 = push 2 stack1
  let stack3 = push 3 stack2
  putStrLn $ "push 1, 2, 3 => " ++ show stack3
  putStrLn $ "peek stack3 => " ++ show (peek stack3)
  
  let (val1, stack4) = pop stack3
  let (val2, stack5) = pop stack4
  let (val3, stack6) = pop stack5
  putStrLn $ "pop operations => " ++ show (val1, val2, val3)
  putStrLn $ "final stack => " ++ show stack6
  putStrLn $ "isEmpty final => " ++ show (isEmpty stack6)
  putStrLn ""
  
  -- キュー
  putStrLn "2. キュー（Queue - FIFO）："
  let queue0 = createEmpty :: Queue Int
  putStrLn $ "空のキュー: " ++ show queue0
  putStrLn $ "qIsEmpty queue0 => " ++ show (qIsEmpty queue0)
  
  let queue1 = enqueue 1 queue0
  let queue2 = enqueue 2 queue1
  let queue3 = enqueue 3 queue2
  putStrLn $ "enqueue 1, 2, 3 => " ++ show queue3
  putStrLn $ "qPeek queue3 => " ++ show (qPeek queue3)
  
  let (qval1, queue4) = dequeue queue3
  let (qval2, queue5) = dequeue queue4
  let (qval3, queue6) = dequeue queue5
  putStrLn $ "dequeue operations => " ++ show (qval1, qval2, qval3)
  putStrLn $ "final queue => " ++ show queue6
  putStrLn $ "qIsEmpty final => " ++ show (qIsEmpty queue6)
  putStrLn ""
  
  -- 安全な操作のデモ
  putStrLn "3. 安全な操作（Safe Operations）："
  let emptyStack = [] :: Stack String
  let emptyQueue = createEmpty :: Queue String
  
  putStrLn $ "popSafe emptyStack => " ++ show (popSafe emptyStack)
  putStrLn $ "peekSafe emptyStack => " ++ show (peekSafe emptyStack)
  putStrLn $ "dequeueSafe emptyQueue => " ++ show (dequeueSafe emptyQueue)
  putStrLn $ "qPeekSafe emptyQueue => " ++ show (qPeekSafe emptyQueue)
  
  let safeStack = push "hello" emptyStack
  let safeQueue = enqueue "world" emptyQueue
  putStrLn $ "popSafe [\"hello\"] => " ++ show (popSafe safeStack)
  putStrLn $ "dequeueSafe (enqueue \"world\") => " ++ show (dequeueSafe safeQueue)
  
  putStrLn ""
  putStrLn "=== 第5章: 再帰アルゴリズム ==="
  putStrLn ""
  
  -- 階乗値のデモ
  putStrLn "1. 階乗値（Factorial）："
  putStrLn $ "factorial 0 => " ++ show (factorial 0)
  putStrLn $ "factorial 3 => " ++ show (factorial 3)
  putStrLn $ "factorial 5 => " ++ show (factorial 5)
  putStrLn $ "factorial' 5 (tail recursive) => " ++ show (factorial' 5)
  putStrLn $ "factorial 10 == factorial' 10 => " ++ show (factorial 10 == factorial' 10)
  putStrLn ""
  
  -- ユークリッドの互除法のデモ
  putStrLn "2. ユークリッドの互除法（GCD）："
  putStrLn $ "gcd' 22 8 => " ++ show (gcd' 22 8)
  putStrLn $ "gcd' 48 18 => " ++ show (gcd' 48 18)
  putStrLn $ "gcd' 17 13 => " ++ show (gcd' 17 13)
  putStrLn ""
  
  -- ハノイの塔のデモ
  putStrLn "3. ハノイの塔（Hanoi Tower）："
  putStrLn $ "hanoi 1 1 3 => " ++ show (hanoi 1 1 3)
  putStrLn $ "hanoi 2 1 3 => " ++ show (hanoi 2 1 3)
  putStrLn $ "hanoi 3 1 3 (7 moves) => " ++ show (hanoi 3 1 3)
  putStrLn $ "hanoi 4のステップ数 => " ++ show (length (hanoi 4 1 3)) ++ " moves"
  putStrLn ""
  
  -- N王妃問題のデモ
  putStrLn "4. N王妃問題（N-Queens Problem）："
  putStrLn $ "queens 1の解の数 => " ++ show (length (queens 1))
  putStrLn $ "queens 4の解の数 => " ++ show (length (queens 4))
  putStrLn $ "queens 8の解の数 => " ++ show (length (queens 8))
  putStrLn ""
  
  -- 4王妃問題の1つの解を表示
  let queen4solutions = queens 4
  if not (null queen4solutions)
    then do
      putStrLn "4王妃問題の1つの解:"
      putStrLn $ show (head queen4solutions)
    else putStrLn "4王妃問題に解はありません"
  
  putStrLn ""
  putStrLn "=== 第6章: ソートアルゴリズム ==="
  putStrLn ""
  
  let testData = [6, 4, 3, 7, 1, 9, 8] :: [Int]
  putStrLn $ "テストデータ: " ++ show testData
  putStrLn ""
  
  -- バブルソートのデモ
  putStrLn "1. バブルソート（Bubble Sort）："
  putStrLn $ "bubbleSort " ++ show testData ++ " => " ++ show (bubbleSort testData)
  putStrLn "特徴: 隣接要素を比較・交換、O(n²)、安定"
  putStrLn ""
  
  -- 選択ソートのデモ
  putStrLn "2. 選択ソート（Selection Sort）："
  putStrLn $ "selectionSort " ++ show testData ++ " => " ++ show (selectionSort testData)
  putStrLn "特徴: 最小値を選択して配置、O(n²)、不安定"
  putStrLn ""
  
  -- 挿入ソートのデモ
  putStrLn "3. 挿入ソート（Insertion Sort）："
  putStrLn $ "insertionSort " ++ show testData ++ " => " ++ show (insertionSort testData)
  putStrLn "特徴: 適切な位置に挿入、O(n²)、安定、ほぼソート済みに高速"
  putStrLn ""
  
  -- クイックソートのデモ
  putStrLn "4. クイックソート（Quick Sort）："
  putStrLn $ "quickSort " ++ show testData ++ " => " ++ show (quickSort testData)
  putStrLn "特徴: 分割統治法、O(n log n)平均、不安定、Haskellで簡潔"
  putStrLn ""
  
  -- マージソートのデモ
  putStrLn "5. マージソート（Merge Sort）："
  let testData2 = [5, 8, 4, 2, 6, 1, 3, 9, 7] :: [Int]
  putStrLn $ "mergeSort " ++ show testData2 ++ " => " ++ show (mergeSort testData2)
  putStrLn "特徴: 分割統治法、O(n log n)常に、安定、標準ライブラリのベース"
  putStrLn ""
  
  -- 文字列ソートのデモ
  putStrLn "6. 文字列のソート："
  let fruits = ["banana", "apple", "cherry", "date"]
  putStrLn $ "文字列データ: " ++ show fruits
  putStrLn $ "quickSort => " ++ show (quickSort fruits)
  putStrLn $ "mergeSort => " ++ show (mergeSort fruits)
  putStrLn ""
  
  -- 重複データのテスト
  putStrLn "7. 重複要素を含むデータ："
  let duplicateData = [3, 1, 4, 1, 5, 9, 2, 6, 5] :: [Int]
  putStrLn $ "テストデータ: " ++ show duplicateData
  putStrLn $ "quickSort => " ++ show (quickSort duplicateData)
  putStrLn $ "mergeSort => " ++ show (mergeSort duplicateData)
  putStrLn ""
  
  -- パフォーマンス比較の準備
  putStrLn "8. アルゴリズムの特徴比較："
  putStrLn "| アルゴリズム    | 平均計算量   | 最悪計算量   | 安定性 |"
  putStrLn "|----------------|-------------|-------------|--------|"
  putStrLn "| バブルソート    | O(n²)       | O(n²)       | 安定   |"
  putStrLn "| 選択ソート      | O(n²)       | O(n²)       | 不安定 |"
  putStrLn "| 挿入ソート      | O(n²)       | O(n²)       | 安定   |"
  putStrLn "| クイックソート  | O(n log n)  | O(n²)       | 不安定 |"
  putStrLn "| マージソート    | O(n log n)  | O(n log n)  | 安定   |"
  putStrLn ""

  putStrLn "=== 第7章: 文字列処理 ==="
  putStrLn ""

  -- 基本的な文字列検索
  putStrLn "1. 基本的な文字列検索："
  let text = "Hello, Haskell World!"
  let pattern1 = "Haskell"
  let pattern2 = "Python"
  putStrLn $ "テキスト: \"" ++ text ++ "\""
  putStrLn $ "bruteForceSearch \"" ++ pattern1 ++ "\" => " ++ show (bruteForceSearch pattern1 text)
  putStrLn $ "bruteForceSearch \"" ++ pattern2 ++ "\" => " ++ show (bruteForceSearch pattern2 text)
  putStrLn $ "bruteForceSearchManual \"" ++ pattern1 ++ "\" => " ++ show (bruteForceSearchManual pattern1 text)
  putStrLn ""

  -- パターンの出現位置検索
  putStrLn "2. パターンの出現位置検索："
  let searchText = "ABABCABABAB"
  let searchPattern = "AB"
  putStrLn $ "テキスト: \"" ++ searchText ++ "\""
  putStrLn $ "パターン: \"" ++ searchPattern ++ "\""
  putStrLn $ "findAllOccurrences => " ++ show (findAllOccurrences searchPattern searchText)
  putStrLn $ "findFirst => " ++ show (findFirst searchPattern searchText)
  putStrLn $ "countOccurrences => " ++ show (countOccurrences searchPattern searchText)
  putStrLn ""

  -- 文字列操作
  putStrLn "3. 文字列操作："
  let sampleText = "hello world"
  putStrLn $ "元の文字列: \"" ++ sampleText ++ "\""
  putStrLn $ "capitalize => \"" ++ capitalize sampleText ++ "\""
  putStrLn $ "removeSpaces => \"" ++ removeSpaces sampleText ++ "\""
  putStrLn $ "reverseWords => \"" ++ reverseWords sampleText ++ "\""
  putStrLn ""

  -- 回文判定
  putStrLn "4. 回文判定："
  let palindromes = ["racecar", "A man a plan a canal Panama", "hello", "Madam"]
  mapM_ (\s -> putStrLn $ "isPalindrome \"" ++ s ++ "\" => " ++ show (isPalindrome s)) palindromes
  putStrLn ""

  -- パターンマッチング
  putStrLn "5. パターンマッチング："
  let testString = "Hello, World!"
  putStrLn $ "テキスト: \"" ++ testString ++ "\""
  putStrLn $ "startsWith \"Hello\" => " ++ show (startsWith "Hello" testString)
  putStrLn $ "endsWith \"World!\" => " ++ show (endsWith "World!" testString)
  putStrLn $ "startsWith \"Hi\" => " ++ show (startsWith "Hi" testString)
  putStrLn ""

  -- シーザー暗号
  putStrLn "6. シーザー暗号："
  let message = "Hello World"
  putStrLn $ "元のメッセージ: \"" ++ message ++ "\""
  putStrLn $ "caesarCipher 3 => \"" ++ caesarCipher 3 message ++ "\""
  putStrLn $ "caesarCipher (-3) => \"" ++ caesarCipher (-3) (caesarCipher 3 message) ++ "\""
  putStrLn $ "rot13 => \"" ++ rot13 message ++ "\""
  putStrLn $ "rot13 (rot13) => \"" ++ rot13 (rot13 message) ++ "\" (元に戻る)"
  putStrLn ""

  -- 文字頻度解析
  putStrLn "7. 文字頻度解析："
  let analyzeText = "programming"
  putStrLn $ "テキスト: \"" ++ analyzeText ++ "\""
  putStrLn $ "charFrequency => " ++ show (charFrequency analyzeText)
  putStrLn ""

  -- アルゴリズムの比較
  putStrLn "8. 文字列検索アルゴリズムの特徴："
  putStrLn "| アルゴリズム        | 時間計算量  | 特徴                     |"
  putStrLn "|-------------------|------------|--------------------------|"
  putStrLn "| 力まかせ法          | O(n*m)     | 単純、理解しやすい        |"
  putStrLn "| KMP法              | O(n+m)     | 前処理でスキップテーブル作成|"
  putStrLn "| Boyer-Moore法       | O(n/m)平均  | 右から左比較、実用的      |"
  putStrLn "| Data.Text.isInfixOf | 最適化済み  | Haskellの実用的選択       |"
  putStrLn ""

  putStrLn "=== 第8章: リスト構造 ==="
  putStrLn ""

  -- 循環リストと遅延評価
  putStrLn "1. 循環リストと遅延評価："
  putStrLn $ "cyclicListManual (最初の10要素): " ++ show (take 10 cyclicListManual)
  putStrLn $ "cyclicFromList [10,20,30] (最初の8要素): " ++ show (take 8 (cyclicFromList [10,20,30]))
  putStrLn $ "takeFromCyclic 7 [1,2,3]: " ++ show (takeFromCyclic 7 [1,2,3])
  putStrLn ""

  -- Zipperパターン（双方向リスト）
  putStrLn "2. Zipperパターン（双方向リスト）："
  let originalList = [10, 20, 30, 40, 50]
  let zipper1 = AdvancedLists.fromList originalList
  putStrLn $ "元のリスト: " ++ show originalList
  putStrLn $ "Zipperから復元: " ++ show (AdvancedLists.toList zipper1)
  putStrLn $ "現在の注目点: " ++ show (getFocus zipper1)
  
  let zipper2 = goForward zipper1
  putStrLn $ "前方移動後の注目点: " ++ show (getFocus zipper2)
  
  let zipper3 = goForward zipper2
  let zipper4 = modify (*100) zipper3
  putStrLn $ "2回前方移動してから要素を100倍: " ++ show (AdvancedLists.toList zipper4)
  putStrLn $ "現在の注目点: " ++ show (getFocus zipper4)
  
  let zipper5 = insertHere 25 zipper4
  putStrLn $ "注目点に25を挿入: " ++ show (AdvancedLists.toList zipper5)
  putStrLn ""

  -- リスト操作の計算量分析
  putStrLn "3. リスト操作の計算量分析："
  let sampleList = [1, 2, 3, 4, 5]
  putStrLn $ "サンプルリスト: " ++ show sampleList
  putStrLn $ listOperationComplexity PrependOp
  putStrLn $ listOperationComplexity AppendOp
  putStrLn $ listOperationComplexity HeadOp
  putStrLn $ listOperationComplexity IndexOp
  putStrLn ""

  -- 高度なリスト生成
  putStrLn "4. 高度なリスト生成（遅延評価）："
  putStrLn $ "フィボナッチ数列の最初の15項: " ++ show (take 15 fibonacciList)
  putStrLn $ "最初の12個の素数: " ++ show (take 12 primesList)
  putStrLn $ "2の冪乗（最初の8項）: " ++ show (take 8 (powerList 2))
  putStrLn $ "3の冪乗（最初の6項）: " ++ show (take 6 (powerList 3))
  putStrLn ""

  -- リスト変換ユーティリティ
  putStrLn "5. リスト変換とユーティリティ："
  let testList = [1..12]
  putStrLn $ "元のリスト: " ++ show testList
  putStrLn $ "chunksOf 3: " ++ show (chunksOf 3 testList)
  putStrLn $ "interleave [1,3,5] [2,4,6]: " ++ show (interleave [1,3,5] [2,4,6])
  putStrLn $ "slidingWindow 3 [1..6]: " ++ show (slidingWindow 3 [1..6])
  putStrLn $ "rotateLeft 2 [1..5]: " ++ show (rotateLeft 2 [1..5])
  putStrLn $ "rotateRight 2 [1..5]: " ++ show (rotateRight 2 [1..5])
  putStrLn ""

  -- 遅延評価の威力
  putStrLn "6. 遅延評価の威力："
  putStrLn $ "無限リスト repeat 'A' の最初の10要素: " ++ show (take 10 (repeat 'A'))
  putStrLn $ "無限の自然数列 [1..] の最初の10要素: " ++ show (take 10 [1..])
  putStrLn $ "無限の平方数列の最初の8要素: " ++ show (take 8 (map (^2) [1..]))
  putStrLn ""

  -- データ構造の特徴比較
  putStrLn "7. リストデータ構造の特徴比較："
  putStrLn "| 操作                | Haskellリスト | 配列   | 双方向リスト |"
  putStrLn "|-------------------|-------------|--------|------------|"
  putStrLn "| 先頭への挿入        | O(1)        | O(n)   | O(1)       |"
  putStrLn "| 末尾への挿入        | O(n)        | O(1)*  | O(1)       |"
  putStrLn "| インデックスアクセス | O(n)        | O(1)   | O(n)       |"
  putStrLn "| 前方/後方移動       | -           | O(1)   | O(1)       |"
  putStrLn "| メモリ効率          | 低          | 高     | 中         |"

  putStrLn ""
  putStrLn "=== 第9章: 木構造 ==="
  putStrLn ""
  
  putStrLn "1. 二分探索木の基本操作："
  let emptyBst = EmptyTree :: Tree Int
  let sampleBst = Tree.fromList [8, 6, 10, 5, 7, 9, 11]
  putStrLn $ "空の木のサイズ: " ++ show (treeSize emptyBst)
  putStrLn $ "サンプル木構築: Tree.fromList [8,6,10,5,7,9,11]"
  putStrLn $ "木のサイズ: " ++ show (treeSize sampleBst)
  putStrLn $ "木の高さ: " ++ show (treeHeight sampleBst)
  putStrLn $ "要素7の検索: " ++ show (treeElem 7 sampleBst)
  putStrLn $ "要素12の検索: " ++ show (treeElem 12 sampleBst)
  putStrLn $ "最小値: " ++ show (treeMin sampleBst)
  putStrLn $ "最大値: " ++ show (treeMax sampleBst)
  
  putStrLn ""
  putStrLn "2. 木の走査アルゴリズム："
  let traversalTree = Tree.fromList [4, 2, 6, 1, 3, 5, 7]
  putStrLn $ "元の挿入順序: [4,2,6,1,3,5,7]"
  putStrLn $ "中間順走査（in-order）: " ++ show (inOrderTraversal traversalTree)
  putStrLn $ "前順走査（pre-order）: " ++ show (preOrderTraversal traversalTree)
  putStrLn $ "後順走査（post-order）: " ++ show (postOrderTraversal traversalTree)
  putStrLn $ "ソートされたリスト: " ++ show (treeToList traversalTree)
  
  putStrLn ""
  putStrLn "3. 木の削除操作："
  let deleteTestTree = Tree.fromList [5, 3, 7, 2, 4, 6, 8]
  putStrLn $ "削除前: " ++ show (treeToList deleteTestTree)
  let afterDelete3 = treeDelete 3 deleteTestTree
  putStrLn $ "要素3削除後: " ++ show (treeToList afterDelete3)
  let afterDelete5 = treeDelete 5 deleteTestTree
  putStrLn $ "要素5削除後: " ++ show (treeToList afterDelete5)
  
  putStrLn ""
  putStrLn "4. 平衡性の分析："
  let balancedTree = Tree.fromList [4, 2, 6, 1, 3, 5, 7]
  let unbalancedTree = Tree.fromList [1, 2, 3, 4, 5]
  putStrLn $ "平衡木 [4,2,6,1,3,5,7]:"
  putStrLn $ "  高さ: " ++ show (treeHeight balancedTree)
  putStrLn $ "  平衡?: " ++ show (isBalanced balancedTree)
  putStrLn $ "  平衡係数: " ++ show (balanceFactor balancedTree)
  putStrLn $ "不平衡木 [1,2,3,4,5]:"
  putStrLn $ "  高さ: " ++ show (treeHeight unbalancedTree)
  putStrLn $ "  平衡?: " ++ show (isBalanced unbalancedTree)
  
  putStrLn ""
  putStrLn "5. 標準ライブラリとの連携："
  let originalList = [5, 3, 7, 2, 4, 6, 8]
  let bstFromList = Tree.fromList originalList
  putStrLn $ "元のリスト: " ++ show originalList
  putStrLn $ "BST構築: " ++ show (treeToList bstFromList)
  
  -- Data.Setとの連携
  let setFromTree = toSet bstFromList
  let treeFromSet = fromSet setFromTree
  putStrLn $ "Data.Set変換: " ++ show (Set.toList setFromTree)
  putStrLn $ "Set→BST変換: " ++ show (treeToList treeFromSet)
  
  -- Data.Mapとの連携
  let mapFromTree = toMap bstFromList
  let treeFromMap = fromMap mapFromTree
  putStrLn $ "Data.Map変換: " ++ show (Map.keys mapFromTree)
  putStrLn $ "Map→BST変換: " ++ show (treeToList treeFromMap)
  
  putStrLn ""
  putStrLn "6. 実用的な使用例："
  -- 重複除去
  let duplicateList = [5, 3, 7, 3, 5, 8, 2, 7]
  let deduplicatedTree = Tree.fromList duplicateList
  putStrLn $ "重複あり: " ++ show duplicateList
  putStrLn $ "重複除去: " ++ show (treeToList deduplicatedTree)
  
  -- 大きなデータでの性能
  let largeData = [100, 50, 150, 25, 75, 125, 175, 12, 37, 62, 87, 112, 137, 162, 187]
  let largeBst = Tree.fromList largeData
  putStrLn $ "大きなBST（15要素）での検索："
  putStrLn $ "  要素87の検索: " ++ show (treeElem 87 largeBst)
  putStrLn $ "  要素200の検索: " ++ show (treeElem 200 largeBst)
  putStrLn $ "  最小値: " ++ show (treeMin largeBst)
  putStrLn $ "  最大値: " ++ show (treeMax largeBst)
  
  putStrLn ""
  putStrLn "7. 木構造の特徴比較："
  putStrLn "| 操作           | 平衡BST    | 不平衡BST | 配列     | リスト   |"
  putStrLn "|---------------|-----------|----------|---------|---------|"
  putStrLn "| 検索          | O(log n)  | O(n)     | O(log n)| O(n)    |"
  putStrLn "| 挿入          | O(log n)  | O(n)     | O(n)    | O(1)    |"
  putStrLn "| 削除          | O(log n)  | O(n)     | O(n)    | O(1)    |"
  putStrLn "| ソート済み列挙 | O(n)      | O(n)     | O(n ln n)| O(n ln n)|"
  putStrLn "| メモリ効率     | 中        | 中       | 高      | 中      |"
