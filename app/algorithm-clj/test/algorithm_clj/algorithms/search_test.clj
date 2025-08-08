(ns algorithm-clj.algorithms.search-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.algorithms.search :refer :all]))

;; 線形探索のテスト
(deftest test-ssearch
  (testing "ssearch-while function"
    (is (= (ssearch-while [6 4 3 2 1 2 8] 2) 3))
    (is (= (ssearch-while [12.7 3.14 6.4 7.2 "End"] 6.4) 2))
    (is (= (ssearch-while [4 7 5.6 2 3.14 1] 5.6) 2))
    (is (= (ssearch-while ["DTS" "AAC" "FLAC"] "DTS") 0))
    (is (= (ssearch-while [1 2 3 4 5] 9) -1)))

  (testing "ssearch-for function"
    (is (= (ssearch-for [6 4 3 2 1 2 8] 2) 3))
    (is (= (ssearch-for [12.7 3.14 6.4 7.2 "End"] 6.4) 2))
    (is (= (ssearch-for [4 7 5.6 2 3.14 1] 5.6) 2))
    (is (= (ssearch-for ["DTS" "AAC" "FLAC"] "DTS") 0))
    (is (= (ssearch-for [1 2 3 4 5] 9) -1))))

;; 番兵法による線形探索のテスト
(deftest test-ssearch-sentinel
  (testing "ssearch-sentinel function"
    (is (= (ssearch-sentinel [6 4 3 2 1 2 8] 2) 3))
    (is (= (ssearch-sentinel [1 2 3 4 5] 9) -1))
    (is (= (ssearch-sentinel [4 7 5.6 2 3.14 1] 5.6) 2))))

;; 二分探索のテスト
(deftest test-bsearch
  (testing "bsearch function"
    (is (= (bsearch [1 2 3 5 7 8 9] 5) 3))
    (is (= (bsearch [1 2 3 5 7 8 9] 1) 0))
    (is (= (bsearch [1 2 3 5 7 8 9] 9) 6))
    (is (= (bsearch [1 2 3 5 7 8 9] 4) -1))
    (is (= (bsearch [1 2 3 5 7 8 9] 10) -1))))

;; チェイン法によるハッシュテーブルのテスト
(deftest test-chained-hash
  (let [ch (make-chained-hash 13)]
    (add-chained-hash ch 1 "赤尾")
    (add-chained-hash ch 5 "武田")
    (add-chained-hash ch 10 "小野")
    (add-chained-hash ch 12 "鈴木")
    (add-chained-hash ch 14 "神崎")

    (testing "search function"
      (is (= (search-chained-hash ch 1) "赤尾"))
      (is (= (search-chained-hash ch 14) "神崎"))
      (is (nil? (search-chained-hash ch 100))))

    (testing "add function"
      (is (true? (add-chained-hash ch 100 "山田")))
      (is (= (search-chained-hash ch 100) "山田"))
      (is (false? (add-chained-hash ch 100 "田中")))) ; 既存キーの追加は失敗

    (testing "remove function"
      (is (true? (remove-chained-hash ch 100)))
      (is (nil? (search-chained-hash ch 100)))
      (is (false? (remove-chained-hash ch 100)))))) ; 存在しないキーの削除は失敗

;; オープンアドレス法によるハッシュテーブルのテスト
(deftest test-open-hash
  (let [oh (make-open-hash 13)]
    (add-open-hash oh 1 "赤尾")
    (add-open-hash oh 5 "武田")
    (add-open-hash oh 10 "小野")
    (add-open-hash oh 12 "鈴木")
    (add-open-hash oh 14 "神崎")

    (testing "search function"
      (is (= (search-open-hash oh 1) "赤尾"))
      (is (= (search-open-hash oh 14) "神崎"))
      (is (nil? (search-open-hash oh 100))))

    (testing "add function"
      (is (true? (add-open-hash oh 100 "山田")))
      (is (= (search-open-hash oh 100) "山田"))
      (is (false? (add-open-hash oh 100 "田中")))) ; 既存キーの追加は失敗

    (testing "remove function"
      (is (true? (remove-open-hash oh 100)))
      (is (nil? (search-open-hash oh 100)))
      (is (false? (remove-open-hash oh 100)))))) ; 存在しないキーの削除は失敗
