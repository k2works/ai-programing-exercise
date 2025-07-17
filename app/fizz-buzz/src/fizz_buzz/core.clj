(ns fizz-buzz.core
  (:gen-class))

(defrecord FizzBuzz [number type])

; ファクトリーメソッド：setterを削除してイミュータブルにする
(defn create-fizz-buzz [number type]
  (->FizzBuzz number type))

(defn generate 
  ([number] (generate number 1))
  ([number type]
   (let [fizz? (zero? (mod number 3))
         buzz? (zero? (mod number 5))]
     (cond
       (= type 1) (cond
                    (and fizz? buzz?) "FizzBuzz"
                    fizz? "Fizz"
                    buzz? "Buzz"
                    :else (str number))
       (= type 2) (str number)
       (= type 3) (if (and fizz? buzz?)
                    "FizzBuzz"
                    (str number))
       :else (throw (Exception. "不正なタイプです"))))))

(defn fizz-buzz-generate [fb]
  (let [{:keys [number type]} fb
        fizz? (zero? (mod number 3))
        buzz? (zero? (mod number 5))]
    (cond
      (= type 1) (cond
                   (and fizz? buzz?) "FizzBuzz"
                   fizz? "Fizz"
                   buzz? "Buzz"
                   :else (str number))
      (= type 2) (str number)
      (= type 3) (if (and fizz? buzz?)
                   "FizzBuzz"
                   (str number))
      :else (throw (Exception. "不正なタイプです")))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
