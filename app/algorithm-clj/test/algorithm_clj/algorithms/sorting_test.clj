(ns algorithm-clj.algorithms.sorting-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.algorithms.sorting :refer :all]))

;; ソートアルゴリズムのテスト（今後実装予定）

(deftest bubble-sort-test
  (testing "バブルソートのテスト（今後実装予定）"
    ;; TODO: 実装予定
    (is (= [] (bubble-sort [])))
    (is (= [1] (bubble-sort [1])))))
