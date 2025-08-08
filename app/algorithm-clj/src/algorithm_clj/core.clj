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

(defn -main
  "アプリケーションのエントリーポイント"
  [& args]
  (println (hello-world))
  (println "2 + 3 =" (sum 2 3)))
