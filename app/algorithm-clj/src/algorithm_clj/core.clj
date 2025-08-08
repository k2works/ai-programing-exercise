(ns algorithm-clj.core
  (:gen-class))

(defn hello-world
  "Hello Worldメッセージを返す"
  []
  "Hello, Algorithm World!")

(defn sum
  "2つの数値の和を計算する"
  [a b]
  (+ a b))

(defn greet
  "Simple greeting function for testing purposes"
  [name]
  (str "Hello, " name "!"))

(defn factorial
  "Compute factorial of n"
  [n]
  (if (<= n 1)
    1
    (* n (factorial (dec n)))))

(defn fibonacci
  "Compute the nth Fibonacci number"
  [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(defn -main
  "アプリケーションのエントリーポイント"
  [& args]
  (println "Algorithm Clojure Project")
  (println (hello-world))
  (println (greet "World"))
  (println "2 + 3 =" (sum 2 3))
  (println "Factorial of 5:" (factorial 5))
  (println "Fibonacci of 10:" (fibonacci 10)))
