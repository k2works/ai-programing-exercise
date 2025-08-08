(ns algorithm-clj.basic-algorithms.recursion-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.basic-algorithms.recursion :refer :all]))

;; ============================
;; 基本的な再帰アルゴリズムのテスト
;; ============================

(deftest test-factorial
  (testing "階乗関数のテスト"
    (is (= (factorial 0) 1))
    (is (= (factorial 1) 1))
    (is (= (factorial 3) 6))
    (is (= (factorial 5) 120))
    (is (= (factorial 10) 3628800))))

(deftest test-gcd
  (testing "最大公約数関数のテスト"
    (is (= (gcd 22 8) 2))
    (is (= (gcd 10 0) 10))
    (is (= (gcd 48 18) 6))
    (is (= (gcd 17 13) 1))
    (is (= (gcd 100 50) 50))))

;; ============================
;; 真に再帰的な関数のテスト
;; ============================

(deftest test-recure
  (testing "真に再帰的な関数のテスト"
    (let [result-atom (atom [])]
      (is (= (recure 4 result-atom) [1 2 3 1 4 1 2])))
    
    (let [result-atom (atom [])]
      (is (= (recure 3 result-atom) [1 2 1 3 1])))
    
    (let [result-atom (atom [])]
      (is (= (recure 2 result-atom) [1 2])))
    
    (let [result-atom (atom [])]
      (is (= (recure 1 result-atom) [1])))))

(deftest test-recure-iterative
  (testing "非再帰版の真に再帰的な関数のテスト"
    (is (= (recure-iterative 4) [1 2 3 1 4 1 2]))
    (is (= (recure-iterative 3) [1 2 1 3 1]))
    (is (= (recure-iterative 2) [1 2]))
    (is (= (recure-iterative 1) [1]))
    (is (= (recure-iterative 0) []))))

(deftest test-recure-vs-iterative
  (testing "再帰版と非再帰版の結果が一致することを確認"
    (doseq [n (range 6)]
      (let [result-atom (atom [])]
        (is (= (recure n result-atom) (recure-iterative n)))))))

;; ============================
;; ハノイの塔のテスト
;; ============================

(deftest test-move-hanoi
  (testing "ハノイの塔 3枚の円盤"
    (let [expected ["円盤[1]を1軸から3軸へ移動"
                    "円盤[2]を1軸から2軸へ移動"
                    "円盤[1]を3軸から2軸へ移動"
                    "円盤[3]を1軸から3軸へ移動"
                    "円盤[1]を2軸から1軸へ移動"
                    "円盤[2]を2軸から3軸へ移動"
                    "円盤[1]を1軸から3軸へ移動"]
          result-atom (atom [])]
      (move-hanoi 3 1 3 result-atom)
      (is (= @result-atom expected))))
  
  (testing "ハノイの塔 2枚の円盤"
    (let [expected ["円盤[1]を1軸から2軸へ移動"
                    "円盤[2]を1軸から3軸へ移動"
                    "円盤[1]を2軸から3軸へ移動"]
          result-atom (atom [])]
      (move-hanoi 2 1 3 result-atom)
      (is (= @result-atom expected))))
  
  (testing "ハノイの塔 1枚の円盤"
    (let [expected ["円盤[1]を1軸から3軸へ移動"]
          result-atom (atom [])]
      (move-hanoi 1 1 3 result-atom)
      (is (= @result-atom expected)))))

(deftest test-count-hanoi-moves
  (testing "ハノイの塔の手数計算"
    (is (= (count-hanoi-moves 1) 1))
    (is (= (count-hanoi-moves 2) 3))
    (is (= (count-hanoi-moves 3) 7))
    (is (= (count-hanoi-moves 4) 15))
    (is (= (count-hanoi-moves 5) 31))))

;; ============================
;; 8王妃問題のテスト
;; ============================

(deftest test-put-queens-all-combinations
  (testing "各列に1個の王妃を配置する組み合わせ数（4×4）"
    (let [combinations (put-queens-all-combinations 4)]
      (is (= (count combinations) 256)) ; 4^4 = 256通り
      (is (= (first combinations) [0 0 0 0]))
      (is (= (last combinations) [3 3 3 3])))))

(deftest test-put-queens-row-col-constraint
  (testing "各行・各列に1個の王妃を配置する組み合わせ数（4×4）"
    (let [combinations (put-queens-row-col-constraint 4)]
      (is (= (count combinations) 24)) ; 4! = 24通り
      ;; 各組み合わせで行と列に重複がないことを確認
      (doseq [combo combinations]
        (is (= (count (set combo)) 4)) ; 各行に1個の王妃
        (is (= (count (set (map-indexed vector combo))) 4)))))) ; 各列に1個の王妃

(deftest test-solve-4-queens
  (testing "4王妃問題の解"
    (let [result (solve-n-queens 4)]
      (is (= (:count result) 2)) ; 4王妃問題には2つの解がある
      (is (= (count (:solutions result)) 2))
      ;; 解の正当性を確認
      (doseq [solution (:solutions result)]
        (is (= (count solution) 4))
        (is (= (count (set solution)) 4)) ; 各行に1個の王妃
        ;; 対角線の制約を確認
        (let [n 4]
          (is (= (count (set (map-indexed + solution))) n)) ; 右上がり対角線
          (is (= (count (set (map-indexed #(- %2 %1) solution))) n))))))) ; 右下がり対角線

(deftest test-solve-8-queens
  (testing "8王妃問題の解の数"
    (let [result (solve-8-queens)]
      (is (= (:count result) 92)) ; 8王妃問題には92個の解がある
      (is (= (count (:solutions result)) 92))
      ;; 最初の解の正当性を確認
      (let [first-solution (first (:solutions result))]
        (is (= (count first-solution) 8))
        (is (= (count (set first-solution)) 8)) ; 各行に1個の王妃
        ;; 対角線の制約を確認
        (let [n 8]
          (is (= (count (set (map-indexed + first-solution))) n)) ; 右上がり対角線
          (is (= (count (set (map-indexed #(- %2 %1) first-solution))) n))))))) ; 右下がり対角線

;; ============================
;; ユーティリティ関数のテスト
;; ============================

(deftest test-print-queen-board
  (testing "王妃の配置表示（目視確認用）"
    ;; このテストは実際の出力を確認するためのもの
    ;; 実際のテストでは出力をキャプチャして検証することもできるが、
    ;; ここでは例外が発生しないことを確認するだけ
    (is (nil? (print-queen-board [1 3 0 2]))) ; 4王妃問題の解の一つ
    (is (nil? (print-queen-board [0 4 7 5 2 6 1 3]))))) ; 8王妃問題の解の一つ

;; ============================
;; 性能テスト（小規模）
;; ============================

(deftest test-recursion-performance
  (testing "再帰アルゴリズムの基本性能"
    ;; 階乗の性能テスト
    (time (is (= (factorial 12) 479001600)))
    
    ;; ユークリッドの互除法の性能テスト
    (time (is (= (gcd 1071 462) 21)))
    
    ;; ハノイの塔の性能テスト（小規模）
    (let [result-atom (atom [])]
      (time (move-hanoi 5 1 3 result-atom))
      (is (= (count @result-atom) 31))) ; 2^5 - 1 = 31手
    
    ;; 6王妃問題の性能テスト
    (let [result (time (solve-n-queens 6))]
      (is (= (:count result) 4))))) ; 6王妃問題には4個の解がある
