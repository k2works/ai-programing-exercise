(ns fizzbuzz.core-test
  (:require [clojure.test :refer :all]
            [fizzbuzz.core :refer :all]))

(deftest fizzbuzz-test
  (testing "1を渡したら文字列1を返す"
    (is (= "1" (generate 1))))

  (testing "2を渡したら文字列2を返す"
    (is (= "2" (generate 2))))

  (testing "3を渡したら文字列Fizzを返す"
    (is (= "Fizz" (generate 3))))

  (testing "5を渡したら文字列Buzzを返す"
    (is (= "Buzz" (generate 5))))

  (testing "15を渡したら文字列FizzBuzzを返す"
    (is (= "FizzBuzz" (generate 15)))))

(deftest fizzbuzz-sequence-test
  (testing "fizzbuzz-sequenceで1から15までの数列を生成"
    (is (= ["1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"]
           (fizzbuzz-sequence 15))))

  (testing "fizzbuzz-sequenceで開始と終了を指定"
    (is (= ["13" "14" "FizzBuzz" "16" "17"]
           (fizzbuzz-sequence 13 17)))))

(deftest alternative-implementation-test
  (testing "generate-v2でも同じ結果が得られる"
    (is (= "1" (generate-v2 1)))
    (is (= "Fizz" (generate-v2 3)))
    (is (= "Buzz" (generate-v2 5)))
    (is (= "FizzBuzz" (generate-v2 15)))))

(deftest lazy-sequence-test
  (testing "fizzbuzz-lazy-seqで最初の15要素を取得"
    (is (= ["1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"]
           (take 15 (fizzbuzz-lazy-seq))))))

(deftest print-fizzbuzz-test
  (testing "1から100までの数をプリントする"
    (is (= 100 (count (print-fizzbuzz))))))
