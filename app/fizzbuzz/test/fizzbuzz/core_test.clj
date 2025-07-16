(ns fizzbuzz.core-test
  (:require [clojure.test :refer :all]
            [fizzbuzz.core :refer :all]))

(deftest fizzbuzz-test
  (testing "1を渡したら文字列1を返す"
    (is (= "1" (generate 1))))
  
  (testing "2を渡したら文字列2を返す"
    (is (= "2" (generate 2))))
  
  (testing "3を渡したら文字列Fizzを返す"
    (is (= "Fizz" (generate 3)))))
