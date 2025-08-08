(ns algorithm-clj.basic-algorithms.tree-demo
  (:require [algorithm-clj.basic-algorithms.tree :as tree]))

(defn demo-basic-tree
  "基本的な二分木のデモ"
  []
  (println "=== 基本的な二分木のデモ ===")
  (let [leaf1 (tree/make-leaf 1)
        leaf3 (tree/make-leaf 3)
        root (tree/make-node 2 leaf1 leaf3)]
    (println "作成した木:")
    (println "  ルート:" (:value root))
    (println "  左の子:" (:value (:left root)))
    (println "  右の子:" (:value (:right root)))
    (println "  木のサイズ:" (tree/tree-size root))
    (println "  木の高さ:" (tree/tree-height root))
    
    (println "\n木の走査:")
    (println "  先行順:" (tree/preorder-traversal root))
    (println "  中順:" (tree/inorder-traversal root))
    (println "  後行順:" (tree/postorder-traversal root))))

(defn demo-binary-search-tree
  "二分探索木のデモ"
  []
  (println "\n=== 二分探索木のデモ ===")
  (let [bst (tree/make-binary-search-tree)]
    (println "空の二分探索木を作成")
    
    ;; 要素の追加
    (println "\n要素の追加:")
    (doseq [item [[5 "five"] [3 "three"] [7 "seven"] [1 "one"] [9 "nine"] [4 "four"] [6 "six"]]]
      (let [key (first item)
            value (second item)]
        (tree/add-bst bst key value)
        (println "  " key "->" value "を追加")))
    
    ;; 探索
    (println "\n要素の探索:")
    (doseq [key [1 3 5 7 9 10]]
      (let [result (tree/search-bst bst key)]
        (if result
          (println "  キー" key ":" result)
          (println "  キー" key ": 見つかりません"))))
    
    ;; 最小値・最大値
    (println "\n最小値・最大値:")
    (println "  最小キー:" (tree/bst-min bst))
    (println "  最大キー:" (tree/bst-max bst))
    
    ;; 二分探索木の内容を表示
    (println "\n二分探索木の内容（昇順）:")
    (tree/dump-bst bst)
    
    ;; 削除操作
    (println "\n削除操作:")
    (println "  キー3を削除")
    (tree/remove-bst bst 3)
    (println "  削除後の内容:")
    (tree/dump-bst bst)))

(defn demo-heap
  "ヒープのデモ"
  []
  (println "\n=== ヒープのデモ ===")
  (let [heap (tree/make-heap)]
    (println "空のヒープを作成")
    (println "  ヒープが空？" (tree/heap-empty? heap))
    (println "  ヒープのサイズ:" (tree/heap-size heap))
    
    ;; 要素の追加
    (println "\n要素の追加:")
    (doseq [value [3 1 4 1 5 9 2 6]]
      (tree/heap-add heap value)
      (println "  " value "を追加、現在の最小値:" (tree/heap-peek heap)))
    
    (println "\nヒープの状態:")
    (println "  ヒープが空？" (tree/heap-empty? heap))
    (println "  ヒープのサイズ:" (tree/heap-size heap))
    
    ;; 要素の取り出し
    (println "\n要素の取り出し（昇順）:")
    (while (not (tree/heap-empty? heap))
      (println "  " (tree/heap-poll heap)))))

(defn demo-complex-tree
  "より複雑な木のデモ"
  []
  (println "\n=== より複雑な木のデモ ===")
  (let [;; より深い木を構築
        tree (tree/make-node 4
                            (tree/make-node 2
                                           (tree/make-leaf 1)
                                           (tree/make-leaf 3))
                            (tree/make-node 6
                                           (tree/make-leaf 5)
                                           (tree/make-leaf 7)))]
    (println "複雑な木の構造:")
    (println "  木のサイズ:" (tree/tree-size tree))
    (println "  木の高さ:" (tree/tree-height tree))
    
    (println "\n各種走査の結果:")
    (println "  先行順:" (tree/preorder-traversal tree))
    (println "  中順:" (tree/inorder-traversal tree))
    (println "  後行順:" (tree/postorder-traversal tree))))

(defn run-all-demos
  "すべてのデモを実行"
  []
  (demo-basic-tree)
  (demo-binary-search-tree)
  (demo-heap)
  (demo-complex-tree)
  (println "\n=== デモ完了 ==="))

;; REPLで実行する場合
;; (run-all-demos)
