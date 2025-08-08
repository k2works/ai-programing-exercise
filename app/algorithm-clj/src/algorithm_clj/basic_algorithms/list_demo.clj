(ns algorithm-clj.basic-algorithms.list-demo
  (:require [algorithm-clj.basic-algorithms.list :as list-ops]))

(defn demo-clojure-list
  "Clojureの組み込みリストのデモ"
  []
  (println "=== Clojureリストのデモ ===")
  (let [my-list '(1 2 3 4 5)]
    (println "元のリスト:" my-list)
    (println "先頭に0を追加:" (cons 0 my-list))
    (println "先頭の要素:" (first my-list))
    (println "先頭以外の要素:" (rest my-list))
    (println "要素数:" (count my-list))
    
    (println "\n要素の走査:")
    (doseq [x my-list]
      (println "  " x))))

(defn demo-java-linked-list
  "JavaのLinkedListのデモ"
  []
  (println "\n=== Java LinkedListのデモ ===")
  (let [ll (list-ops/make-java-linked-list)]
    (println "初期状態 - サイズ:" (.size ll))
    
    ;; 要素の追加
    (list-ops/add-first-java-linked-list ll 2)
    (list-ops/add-first-java-linked-list ll 1)
    (list-ops/add-last-java-linked-list ll 3)
    (list-ops/add-last-java-linked-list ll 4)
    
    (println "要素追加後 - サイズ:" (.size ll))
    (println "リストの内容:" (seq ll))
    
    ;; 要素の検索
    (println "要素3のインデックス:" (list-ops/search-java-linked-list ll 3))
    (println "要素5のインデックス:" (list-ops/search-java-linked-list ll 5))
    
    ;; 要素の削除
    (println "先頭要素を削除:" (list-ops/remove-first-java-linked-list ll))
    (println "末尾要素を削除:" (list-ops/remove-last-java-linked-list ll))
    (println "削除後のリスト:" (seq ll))))

(defn demo-bidirectional-access
  "双方向アクセスのデモ"
  []
  (println "\n=== 双方向アクセスのデモ ===")
  (let [ll (list-ops/make-java-linked-list)]
    (list-ops/add-last-java-linked-list ll "A")
    (list-ops/add-last-java-linked-list ll "B")
    (list-ops/add-last-java-linked-list ll "C")
    
    (println "リスト:" (seq ll))
    
    ;; 前方向の走査
    (println "前方向の走査:")
    (dotimes [i (.size ll)]
      (println "  インデックス" i ":" (.get ll i)))
    
    ;; 後方向の走査
    (println "後方向の走査:")
    (let [it (.listIterator ll (.size ll))]
      (while (.hasPrevious it)
        (println "  " (.previous it))))))

(defn run-all-demos
  "すべてのデモを実行"
  []
  (demo-clojure-list)
  (demo-java-linked-list)
  (demo-bidirectional-access)
  (println "\n=== デモ完了 ==="))

;; REPLで実行する場合
;; (run-all-demos)
