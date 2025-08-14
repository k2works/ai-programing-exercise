(ns fizzbuzz.core-test
  "テスト駆動開発から始めるClojureScript入門 - FizzBuzzテストスイート
   
   このテストスイートは以下のTDDサイクルに従って作成されています:
   1. Red: 失敗するテストを書く
   2. Green: テストをパスする最小限のコードを書く  
   3. Refactor: コードをリファクタリングして品質を向上させる"
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [fizzbuzz.core :as fizzbuzz]))

(deftest fizzbuzz-test
  (testing "FizzBuzz基本機能のテスト"
    (testing "数を文字列にして返す"
      (testing "1を渡したら文字列\"1\"を返す"
        (is (= "1" (fizzbuzz/fizzbuzz 1))))
      (testing "2を渡したら文字列\"2\"を返す"
        (is (= "2" (fizzbuzz/fizzbuzz 2)))))
    
    (testing "3の倍数のときは数の代わりに「Fizz」と返す"
      (testing "3を渡したら文字列\"Fizz\"を返す"
        (is (= "Fizz" (fizzbuzz/fizzbuzz 3))))
      (testing "6を渡したら文字列\"Fizz\"を返す"
        (is (= "Fizz" (fizzbuzz/fizzbuzz 6))))
      (testing "9を渡したら文字列\"Fizz\"を返す"
        (is (= "Fizz" (fizzbuzz/fizzbuzz 9)))))
    
    (testing "5の倍数のときは「Buzz」と返す"
      (testing "5を渡したら文字列\"Buzz\"を返す"
        (is (= "Buzz" (fizzbuzz/fizzbuzz 5))))
      (testing "10を渡したら文字列\"Buzz\"を返す"
        (is (= "Buzz" (fizzbuzz/fizzbuzz 10))))
      (testing "20を渡したら文字列\"Buzz\"を返す"
        (is (= "Buzz" (fizzbuzz/fizzbuzz 20)))))
    
    (testing "3と5両方の倍数の場合には「FizzBuzz」と返す"
      (testing "15を渡したら文字列\"FizzBuzz\"を返す"
        (is (= "FizzBuzz" (fizzbuzz/fizzbuzz 15))))
      (testing "30を渡したら文字列\"FizzBuzz\"を返す"
        (is (= "FizzBuzz" (fizzbuzz/fizzbuzz 30))))
      (testing "45を渡したら文字列\"FizzBuzz\"を返す"
        (is (= "FizzBuzz" (fizzbuzz/fizzbuzz 45)))))))

(deftest fizzbuzz-list-test
  (testing "FizzBuzzリスト生成機能のテスト"
    (testing "1から100までのFizzBuzzリストを生成する"
      (let [result (fizzbuzz/fizzbuzz-list 1 100)]
        (is (= 100 (count result)))
        (is (= "1" (first result)))
        (is (= "Buzz" (nth result 99)))))
    
    (testing "小さな範囲のFizzBuzzリストを生成する"
      (let [result (fizzbuzz/fizzbuzz-list 1 15)]
        (is (= 15 (count result)))
        (is (= ["1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"] result))))))

;; テストを実行
;; このファイルがコンパイルされると自動的にテストが実行されます
(run-tests)
