(ns algorithm-clj.basic-algorithms.array-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.basic-algorithms.array :refer :all]))

;; 5人の点数から合計と平均を計算（個別引数版）
(deftest test-calculate-scores-individual
  (testing "calculate-scores-individual function"
    (is (= (calculate-scores-individual 32 68 72 54 92) "318,63.6"))
    (is (= (calculate-scores-individual 80 85 90 95 100) "450,90.0"))))

;; 配列の要素の最大値を求める
(deftest test-max-of
  (testing "max-of function"
    (is (= (max-of [172 153 192 140 165]) 192))
    (is (= (max-of [1 5 3 9 2]) 9))
    (is (= (max-of [42]) 42))))

;; 配列の要素の並びを反転する
(deftest test-reverse-vector
  (testing "reverse-vector function"
    (is (= (reverse-vector [2 5 1 3 9 6 7]) [7 6 9 3 1 5 2]))
    (is (= (reverse-vector [1 2 3 4 5]) [5 4 3 2 1]))
    (is (= (reverse-vector [42]) [42]))))

;; 基数変換
(deftest test-card-conv
  (testing "card-conv function"
    (is (= (card-conv 29 2) "11101"))
    (is (= (card-conv 255 16) "FF"))
    (is (= (card-conv 36 36) "10"))
    (is (= (card-conv 0 2) "0"))
    (is (= (card-conv 100 10) "100"))))

;; 素数の列挙
(deftest test-prime
  (testing "prime1 function"
    (is (= (prime1 1000) 78022))
    (is (= (prime1 100) 1133)))
  (testing "prime2 function"
    (is (= (prime2 1000) 15121))
    (is (= (prime2 100) 362)))
  (testing "prime3 function"
    (is (= (prime3 1000) 3774))
    (is (= (prime3 100) 191))))
