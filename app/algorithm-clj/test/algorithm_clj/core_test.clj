(ns algorithm-clj.core-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.core :refer :all]))

(deftest hello-world-test
  (testing "Hello Worldメッセージが正しく返される"
    (is (= "Hello, Algorithm World!" (hello-world)))))

(deftest sum-test
  (testing "2つの数値の和が正しく計算される"
    (is (= 5 (sum 2 3)))
    (is (= 0 (sum -1 1)))
    (is (= -5 (sum -2 -3)))))
