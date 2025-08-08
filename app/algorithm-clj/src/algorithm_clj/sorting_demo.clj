(ns algorithm-clj.sorting-demo
  "第6章 ソートアルゴリズムのデモ"
  (:require [algorithm-clj.algorithms.sorting :as sort]))

(defn -main []
  (println "=== 第6章: ソートアルゴリズムのデモ ===")
  (println)
  
  (let [test-data [3 1 4 1 5 9 2 6]]
    (println "テストデータ:" test-data)
    (println)
    
    (println "バブルソート:" (sort/bubble-sort test-data))
    (println "選択ソート:" (sort/selection-sort test-data))
    (println "挿入ソート:" (sort/insertion-sort test-data))
    (println "シェルソート:" (sort/shell-sort test-data))
    (println "クイックソート:" (sort/quick-sort test-data))
    (println "マージソート:" (sort/merge-sort test-data))
    (println "ヒープソート:" (sort/heap-sort test-data))
    (println)
    
    (println "標準ソート:" (sort test-data))
    (println)
    
    ;; 性能測定用のデータ
    (let [large-data (shuffle (range 1000))]
      (println "1000要素のランダムデータでの動作確認:")
      (println "バブルソート完了:" (= (sort/bubble-sort large-data) (sort large-data)))
      (println "選択ソート完了:" (= (sort/selection-sort large-data) (sort large-data)))
      (println "挿入ソート完了:" (= (sort/insertion-sort large-data) (sort large-data)))
      (println "シェルソート完了:" (= (sort/shell-sort large-data) (sort large-data)))
      (println "クイックソート完了:" (= (sort/quick-sort large-data) (sort large-data)))
      (println "マージソート完了:" (= (sort/merge-sort large-data) (sort large-data)))
      (println "ヒープソート完了:" (= (sort/heap-sort large-data) (sort large-data))))))

;; REPLで直接呼び出すための関数
(defn demo []
  (-main))
