(ns fizzbuzz.core-test
  (:require [clojure.test :refer :all]
            [fizzbuzz.core :refer :all]))

(deftest hello-test
  (testing "greeting"
    (is (= "hello world" (greeting)))))
