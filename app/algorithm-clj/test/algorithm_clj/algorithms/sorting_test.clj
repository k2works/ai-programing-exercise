(ns algorithm-clj.algorithms.sorting-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.algorithms.sorting :refer :all]))

;; ============================
;; 第6章: ソートアルゴリズム テスト
;; ============================

;; テスト用データ
(def test-data
  {:empty []
   :single [42]
   :sorted [1 2 3 4 5]
   :reverse [5 4 3 2 1]
   :random [3 1 4 1 5 9 2 6]
   :duplicates [3 1 4 1 5 9 2 6 5 3]})

;; ============================
;; バブルソートのテスト
;; ============================

(deftest test-bubble-sort
  (testing "バブルソートのテスト"
    (is (= (bubble-sort (:empty test-data)) []))
    (is (= (bubble-sort (:single test-data)) [42]))
    (is (= (bubble-sort (:sorted test-data)) [1 2 3 4 5]))
    (is (= (bubble-sort (:reverse test-data)) [1 2 3 4 5]))
    (is (= (bubble-sort (:random test-data)) [1 1 2 3 4 5 6 9]))
    (is (= (bubble-sort (:duplicates test-data)) [1 1 2 3 3 4 5 5 6 9]))))

;; ============================
;; 選択ソートのテスト
;; ============================

(deftest test-selection-sort
  (testing "選択ソートのテスト"
    (is (= (selection-sort (:empty test-data)) []))
    (is (= (selection-sort (:single test-data)) [42]))
    (is (= (selection-sort (:sorted test-data)) [1 2 3 4 5]))
    (is (= (selection-sort (:reverse test-data)) [1 2 3 4 5]))
    (is (= (selection-sort (:random test-data)) [1 1 2 3 4 5 6 9]))
    (is (= (selection-sort (:duplicates test-data)) [1 1 2 3 3 4 5 5 6 9]))))

;; ============================
;; 挿入ソートのテスト
;; ============================

(deftest test-insertion-sort
  (testing "挿入ソートのテスト"
    (is (= (insertion-sort (:empty test-data)) []))
    (is (= (insertion-sort (:single test-data)) [42]))
    (is (= (insertion-sort (:sorted test-data)) [1 2 3 4 5]))
    (is (= (insertion-sort (:reverse test-data)) [1 2 3 4 5]))
    (is (= (insertion-sort (:random test-data)) [1 1 2 3 4 5 6 9]))
    (is (= (insertion-sort (:duplicates test-data)) [1 1 2 3 3 4 5 5 6 9]))))

;; ============================
;; シェルソートのテスト
;; ============================

(deftest test-shell-sort
  (testing "シェルソートのテスト"
    (is (= (shell-sort (:empty test-data)) []))
    (is (= (shell-sort (:single test-data)) [42]))
    (is (= (shell-sort (:sorted test-data)) [1 2 3 4 5]))
    (is (= (shell-sort (:reverse test-data)) [1 2 3 4 5]))
    (is (= (shell-sort (:random test-data)) [1 1 2 3 4 5 6 9]))
    (is (= (shell-sort (:duplicates test-data)) [1 1 2 3 3 4 5 5 6 9]))))

;; ============================
;; クイックソートのテスト
;; ============================

(deftest test-quick-sort
  (testing "クイックソートのテスト"
    (is (= (quick-sort (:empty test-data)) []))
    (is (= (quick-sort (:single test-data)) [42]))
    (is (= (quick-sort (:sorted test-data)) [1 2 3 4 5]))
    (is (= (quick-sort (:reverse test-data)) [1 2 3 4 5]))
    (is (= (quick-sort (:random test-data)) [1 1 2 3 4 5 6 9]))
    (is (= (quick-sort (:duplicates test-data)) [1 1 2 3 3 4 5 5 6 9]))))

;; ============================
;; マージソートのテスト
;; ============================

(deftest test-merge-sort
  (testing "マージソートのテスト"
    (is (= (merge-sort (:empty test-data)) []))
    (is (= (merge-sort (:single test-data)) [42]))
    (is (= (merge-sort (:sorted test-data)) [1 2 3 4 5]))
    (is (= (merge-sort (:reverse test-data)) [1 2 3 4 5]))
    (is (= (merge-sort (:random test-data)) [1 1 2 3 4 5 6 9]))
    (is (= (merge-sort (:duplicates test-data)) [1 1 2 3 3 4 5 5 6 9]))))

;; ============================
;; ヒープソートのテスト
;; ============================

(deftest test-heap-sort
  (testing "ヒープソートのテスト"
    (is (= (heap-sort (:empty test-data)) []))
    (is (= (heap-sort (:single test-data)) [42]))
    (is (= (heap-sort (:sorted test-data)) [1 2 3 4 5]))
    (is (= (heap-sort (:reverse test-data)) [1 2 3 4 5]))
    (is (= (heap-sort (:random test-data)) [1 1 2 3 4 5 6 9]))
    (is (= (heap-sort (:duplicates test-data)) [1 1 2 3 3 4 5 5 6 9]))))

;; ============================
;; ソート性能比較テスト
;; ============================

(deftest test-sorting-performance
  (testing "ソートアルゴリズムの性能比較"
    (let [large-data (shuffle (range 1000))]
      ;; 全アルゴリズムが同じ結果を返すことを確認
      (is (= (bubble-sort large-data)
             (selection-sort large-data)
             (insertion-sort large-data)
             (shell-sort large-data)
             (quick-sort large-data)
             (merge-sort large-data)
             (heap-sort large-data)
             (sort large-data))))))
