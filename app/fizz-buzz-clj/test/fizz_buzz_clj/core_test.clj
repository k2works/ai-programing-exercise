(ns fizz-buzz-clj.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [fizz-buzz-clj.core :refer [generate generate-list]]))

(deftest fizz-buzz-test
  (testing "FizzBuzz"
    (testing "三の倍数の場合"
      (is (= "Fizz" (generate 3))))

    (testing "五の倍数の場合"
      (is (= "Buzz" (generate 5))))

    (testing "三と五の倍数の場合"
      (is (= "FizzBuzz" (generate 15))))

    (testing "その他の場合"
      (is (= "1" (generate 1)))
      (is (= "2" (generate 2)))
      (is (= "4" (generate 4)))
      (is (= "7" (generate 7))))

    (testing "配列の操作（学習用テスト）"
      (testing "mapによる新しい要素の配列を返す"
        (is (= [5 6 9 10] (map count ["apple" "orange" "pineapple" "strawberry"]))))

      (testing "filterによる特定の条件を満たす要素だけを配列に入れて返す"
        (is (= [2 4] (filter #(= 0 (mod % 2)) [1 2 3 4 5]))))

      (testing "remove（rejectの代替）で特定の条件を満たさない要素だけを配列に入れて返す"
        (is (= [1 3 5] (remove #(= 0 (mod % 2)) [1 2 3 4 5]))))

      (testing "配列の中から条件に一致する要素を取得する"
        (is (= "apple" (first (filter #(> (count %) 4) ["apple" "orange" "pineapple" "strawberry"])))))

      (testing "sortによる指定した評価式で並び変えた配列を返す"
        (let [numbers ["2" "4" "13" "3" "1" "10"]]
          (is (= ["1" "10" "13" "2" "3" "4"] (sort numbers)))
          (is (= ["1" "2" "3" "4" "10" "13"] (sort-by #(Integer/parseInt %) numbers)))
          (is (= ["13" "10" "4" "3" "2" "1"] (sort-by #(Integer/parseInt %) > numbers)))))

      (testing "takeとdropによる要素の取得"
        (is (= [1 2 3 4 5] (take-while #(< % 6) [1 2 3 4 5 6 7 8 9])))
        (is (= [6 7 8 9 10] (drop-while #(< % 6) [1 2 3 4 5 6 7 8 9 10]))))

      (testing "reduceによる畳み込み演算を行う"
        (is (= 15 (reduce + [1 2 3 4 5])))
        (is (= 15 (reduce + 0 [1 2 3 4 5])))))

    (testing "1から指定した数値までのFizzBuzz配列を返す"
      (let [result (take 15 (generate-list))]
        (is (= ["1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"] result))))

    (testing "1から100までのFizzBuzz配列を返す"
      (let [result (generate-list)]
        (is (= 100 (count result)))
        (is (= "1" (first result)))
        (is (= "Buzz" (last result)))))))
