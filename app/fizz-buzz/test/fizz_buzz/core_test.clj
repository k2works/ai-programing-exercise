(ns fizz-buzz.core-test
  (:require [clojure.test :refer :all]
            [fizz-buzz.core :refer :all]
            [fizz-buzz.model :as model]
            [fizz-buzz.application :as app]))

(deftest fizz-buzz-test
  (testing "基本的なFizzBuzz"
    (is (= "1" (generate 1)))
    (is (= "2" (generate 2)))
    (is (= "Fizz" (generate 3)))
    (is (= "4" (generate 4)))
    (is (= "Buzz" (generate 5)))
    (is (= "Fizz" (generate 6)))
    (is (= "7" (generate 7)))
    (is (= "8" (generate 8)))
    (is (= "Fizz" (generate 9)))
    (is (= "Buzz" (generate 10)))
    (is (= "11" (generate 11)))
    (is (= "Fizz" (generate 12)))
    (is (= "13" (generate 13)))
    (is (= "14" (generate 14)))
    (is (= "FizzBuzz" (generate 15)))))

(deftest fizz-buzz-type-test
  (testing "タイプごとに出力を切り替えることができる"
    (testing "タイプ1の場合"
      (is (= "1" (generate 1 1)))
      (is (= "Fizz" (generate 3 1)))
      (is (= "Buzz" (generate 5 1)))
      (is (= "FizzBuzz" (generate 15 1))))
    (testing "タイプ2の場合"
      (is (= "1" (generate 1 2)))
      (is (= "3" (generate 3 2)))
      (is (= "5" (generate 5 2)))
      (is (= "15" (generate 15 2))))
    (testing "タイプ3の場合"
      (is (= "1" (generate 1 3)))
      (is (= "3" (generate 3 3)))
      (is (= "5" (generate 5 3)))
      (is (= "FizzBuzz" (generate 15 3))))
    (testing "それ以外のタイプの場合"
      (is (thrown? Exception (generate 1 4))))))

(deftest functional-programming-test
  (testing "関数型プログラミング機能のテスト"
    
    (testing "純粋関数のテスト"
      (is (= "1" (number->fizz-buzz-string 1)))
      (is (= "Fizz" (number->fizz-buzz-string 3)))
      (is (= "3" (number->fizz-buzz-string 3 2))))
    
    (testing "シーケンス生成のテスト"
      (is (= ["1" "2" "Fizz" "4" "Buzz"] (take 5 (numbers->fizz-buzz-seq 100))))
      (is (= ["1" "2" "3" "4" "5"] (take 5 (numbers->fizz-buzz-seq 100 2)))))))

(deftest model-test
  (testing "Model層のテスト"
    (let [fbv (model/create-fizz-buzz-value 15)]
      (is (model/fizz? fbv))
      (is (model/buzz? fbv))
      (is (model/fizz-buzz? fbv)))))

(deftest application-test
  (testing "Application層のテスト"
    (let [processor (app/create-fizz-buzz-processor 1)]
      (is (= "1" (processor 1)))
      (is (= "Fizz" (processor 3)))
      (is (= "Buzz" (processor 5)))
      (is (= "FizzBuzz" (processor 15))))))

(deftest integration-test
  (testing "統合テスト"
    (let [results (app/process-numbers [1 2 3 4 5] 1)]
      (is (= ["1" "2" "Fizz" "4" "Buzz"] results)))))