(ns fizz-buzz.core-test
  (:require [clojure.test :refer :all]
            [fizz-buzz.core :refer :all]))

(deftest fizz-buzz-test
  (testing "三の倍数の場合"
    (testing "3を渡したら文字列Fizzを返す"
      (is (= "Fizz" (generate 3))))
    (testing "6を渡したら文字列Fizzを返す"
      (is (= "Fizz" (generate 6)))))
  
  (testing "五の倍数の場合"
    (testing "5を渡したら文字列Buzzを返す"
      (is (= "Buzz" (generate 5))))
    (testing "10を渡したら文字列Buzzを返す"
      (is (= "Buzz" (generate 10)))))
      
  (testing "三と五の倍数の場合"
    (testing "15を渡したら文字列FizzBuzzを返す"
      (is (= "FizzBuzz" (generate 15))))
    (testing "30を渡したら文字列FizzBuzzを返す"
      (is (= "FizzBuzz" (generate 30)))))
      
  (testing "その他の数値の場合"
    (testing "1を渡したら文字列1を返す"
      (is (= "1" (generate 1))))
    (testing "2を渡したら文字列2を返す"
      (is (= "2" (generate 2))))))

(deftest fizz-buzz-type-test
  (testing "タイプごとに出力を切り替えることができる"
    (testing "タイプ1の場合"
      (testing "1を渡したら文字列1を返す"
        (is (= "1" (generate 1 1))))
      (testing "3を渡したら文字列Fizzを返す"
        (is (= "Fizz" (generate 3 1))))
      (testing "5を渡したら文字列Buzzを返す"
        (is (= "Buzz" (generate 5 1))))
      (testing "15を渡したら文字列FizzBuzzを返す"
        (is (= "FizzBuzz" (generate 15 1)))))
    (testing "タイプ2の場合"
      (testing "1を渡したら文字列1を返す"
        (is (= "1" (generate 1 2))))
      (testing "3を渡したら文字列3を返す"
        (is (= "3" (generate 3 2))))
      (testing "5を渡したら文字列5を返す"
        (is (= "5" (generate 5 2))))
      (testing "15を渡したら文字列15を返す"
        (is (= "15" (generate 15 2)))))
    (testing "タイプ3の場合"
      (testing "1を渡したら文字列1を返す"
        (is (= "1" (generate 1 3))))
      (testing "3を渡したら文字列3を返す"
        (is (= "3" (generate 3 3))))
      (testing "5を渡したら文字列5を返す"
        (is (= "5" (generate 5 3))))
      (testing "15を渡したら文字列FizzBuzzを返す"
        (is (= "FizzBuzz" (generate 15 3)))))))