(ns algorithm-clj.core
  (:require [algorithm-clj.demos.array-demo :as array-demo]
            [algorithm-clj.demos.search-demo :as search-demo]
            [algorithm-clj.demos.sorting-demo :as sorting-demo]
            [algorithm-clj.demos.tree-demo :as tree-demo]
            [algorithm-clj.demos.string-demo :as string-demo]
            [algorithm-clj.demos.recursion-demo :as recursion-demo]
            [algorithm-clj.demos.stack-queue-demo :as stack-queue-demo]
            [algorithm-clj.demos.list-demo :as list-demo]
            [algorithm-clj.demos.core-demo :as core-demo])
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
  [user-name]
  (str "Hello, " user-name "!"))

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
  (println "Fibonacci of 10:" (fibonacci 10))

  ;; デモの実行
  (when (or (empty? args) (some #{"demo"} args))
    (println "\n=== Running Algorithm Demos ===")
    (try
      (array-demo/-main)
      (search-demo/-main)
      (sorting-demo/-main)
      (sorting-demo/demo)  ; sorting-demo内のdemo関数も呼び出し
      (tree-demo/-main)
      (string-demo/-main)
      (recursion-demo/-main)
      (stack-queue-demo/-main)
      (list-demo/run-all-demos)
      (core-demo/-main)

      ;; デバッグ用素数生成デモの統合
      (when (some #{"all" "debug"} args)
        (println "\n=== Prime Generation Analysis ===")
        (try
          (let [debug-prime (requiring-resolve 'algorithm-clj.debug-prime/-main)]
            (when debug-prime (debug-prime)))
          (catch Exception e
            (println "Debug prime analysis not available"))))

      (catch Exception e
        (println "Demo execution error:" (.getMessage e))))))
