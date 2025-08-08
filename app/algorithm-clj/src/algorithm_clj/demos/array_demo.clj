(ns algorithm-clj.demos.array-demo
  (:require [algorithm-clj.algorithms.array :as array]))

(defn -main []
  (println "=== アルゴリズムから始めるClojure入門 第2章 配列 デモ ===\n")
  
  ;; 5人の点数から合計と平均
  (println "【5人の点数から合計と平均】")
  (println "calculate-scores-individual(32, 68, 72, 54, 92) =" 
           (array/calculate-scores-individual 32 68 72 54 92))
  (println)
  
  ;; 配列の要素の最大値
  (println "【配列の要素の最大値】")
  (println "max-of([172 153 192 140 165]) =" (array/max-of [172 153 192 140 165]))
  (println "max-of([1 5 3 9 2]) =" (array/max-of [1 5 3 9 2]))
  (println)
  
  ;; 配列の要素の並びを反転
  (println "【配列の要素の並びを反転】")
  (println "reverse-vector([2 5 1 3 9 6 7]) =" (array/reverse-vector [2 5 1 3 9 6 7]))
  (println "reverse-vector([1 2 3 4 5]) =" (array/reverse-vector [1 2 3 4 5]))
  (println)
  
  ;; 基数変換
  (println "【基数変換】")
  (println "card-conv(29, 2) =" (array/card-conv 29 2))
  (println "card-conv(255, 16) =" (array/card-conv 255 16))
  (println "card-conv(36, 36) =" (array/card-conv 36 36))
  (println "card-conv(100, 10) =" (array/card-conv 100 10))
  (println)
  
  ;; 素数の列挙（効率比較）
  (println "【素数の列挙 - 除算回数の比較】")
  (let [limit 1000]
    (println (str "1000以下の素数列挙での除算回数:"))
    (println "  第1版 (prime1):" (array/prime1 limit) "回")
    (println "  第2版 (prime2):" (array/prime2 limit) "回")
    (println "  第3版 (prime3):" (array/prime3 limit) "回"))
  (println)
  
  (let [limit 100]
    (println (str "100以下の素数列挙での除算回数:"))
    (println "  第1版 (prime1):" (array/prime1 limit) "回")
    (println "  第2版 (prime2):" (array/prime2 limit) "回")
    (println "  第3版 (prime3):" (array/prime3 limit) "回"))
  (println)
  
  ;; Clojureのベクターとリストの基本的な操作例
  (println "【Clojureのベクターとリストの基本操作】")
  (let [v [11 22 33 44 55 66 77]
        l '(4 7 5.6 2 3.14 1)]
    (println "ベクター v:" v)
    (println "  nth v 2:" (nth v 2))
    (println "  count v:" (count v))
    (println "  conj v 88:" (conj v 88))
    (println)
    
    (println "リスト l:" l)
    (println "  first l:" (first l))
    (println "  rest l:" (rest l))
    (println "  conj l 0:" (conj l 0))
    (println)))
