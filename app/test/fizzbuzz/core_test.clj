(ns fizzbuzz.core-test
  (:require [clojure.test :refer :all]
            [fizzbuzz.core :refer :all]))

(deftest fizzbuzz-test
  (testing "数を文字列にして返す"
    (testing "1を渡したら文字列\"1\"を返す"
      (is (= "1" (fizzbuzz 1))))
    (testing "2を渡したら文字列\"2\"を返す"
      (is (= "2" (fizzbuzz 2)))))
  
  (testing "3の倍数のときは数の代わりに「Fizz」と返す"
    (testing "3を渡したら文字列\"Fizz\"を返す"
      (is (= "Fizz" (fizzbuzz 3))))
    (testing "6を渡したら文字列\"Fizz\"を返す"
      (is (= "Fizz" (fizzbuzz 6))))
    (testing "9を渡したら文字列\"Fizz\"を返す"
      (is (= "Fizz" (fizzbuzz 9)))))
  
  (testing "5の倍数のときは「Buzz」と返す"
    (testing "5を渡したら文字列\"Buzz\"を返す"
      (is (= "Buzz" (fizzbuzz 5))))
    (testing "10を渡したら文字列\"Buzz\"を返す"
      (is (= "Buzz" (fizzbuzz 10))))
    (testing "20を渡したら文字列\"Buzz\"を返す"
      (is (= "Buzz" (fizzbuzz 20)))))
  
  (testing "3と5両方の倍数の場合には「FizzBuzz」と返す"
    (testing "15を渡したら文字列\"FizzBuzz\"を返す"
      (is (= "FizzBuzz" (fizzbuzz 15))))
    (testing "30を渡したら文字列\"FizzBuzz\"を返す"
      (is (= "FizzBuzz" (fizzbuzz 30))))
    (testing "45を渡したら文字列\"FizzBuzz\"を返す"
      (is (= "FizzBuzz" (fizzbuzz 45))))))

(deftest fizzbuzz-list-test
  (testing "1から100までのFizzBuzzリストを生成する"
    (let [result (fizzbuzz-list 1 100)]
      (is (= 100 (count result)))
      (is (= "1" (first result)))
      (is (= "Buzz" (nth result 99))) ; 100番目の要素は"Buzz"
      (is (= "Fizz" (nth result 2)))  ; 3番目の要素は"Fizz"
      (is (= "FizzBuzz" (nth result 14)))))) ; 15番目の要素は"FizzBuzz"

(deftest fizzbuzz-edge-cases-test
  (testing "エッジケース"
    (testing "0を渡したら文字列\"FizzBuzz\"を返す"
      (is (= "FizzBuzz" (fizzbuzz 0))))
    (testing "負の数 -15を渡したら文字列\"FizzBuzz\"を返す"
      (is (= "FizzBuzz" (fizzbuzz -15))))))
