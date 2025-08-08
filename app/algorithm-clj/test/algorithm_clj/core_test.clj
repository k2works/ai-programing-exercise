(ns algorithm-clj.core-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.core :refer :all]))

(deftest test-hello-world
  (testing "Hello World function"
    (is (= "Hello, Algorithm World!" (hello-world)))))

(deftest test-sum
  (testing "Sum function"
    (is (= 5 (sum 2 3)))
    (is (= 0 (sum -1 1)))
    (is (= -5 (sum -2 -3)))))

(deftest test-greet
  (testing "Greet function"
    (is (= "Hello, World!" (greet "World")))
    (is (= "Hello, Alice!" (greet "Alice")))))

(deftest test-factorial
  (testing "Factorial function"
    (is (= 1 (factorial 0)))
    (is (= 1 (factorial 1)))
    (is (= 2 (factorial 2)))
    (is (= 6 (factorial 3)))
    (is (= 24 (factorial 4)))
    (is (= 120 (factorial 5)))))

(deftest test-fibonacci
  (testing "Fibonacci function"
    (is (= 0 (fibonacci 0)))
    (is (= 1 (fibonacci 1)))
    (is (= 1 (fibonacci 2)))
    (is (= 2 (fibonacci 3)))
    (is (= 3 (fibonacci 4)))
    (is (= 5 (fibonacci 5)))
    (is (= 8 (fibonacci 6)))
    (is (= 55 (fibonacci 10)))))
