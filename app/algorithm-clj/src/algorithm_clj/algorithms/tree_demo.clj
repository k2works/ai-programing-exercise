(ns algorithm-clj.algorithms.tree-demo
  (:require [algorithm-clj.algorithms.tree :as tree]))

(defn demo-basic-tree
  "基本的な木のデモ"
  []
  (println "=== 基本的な木のデモ ===")
  
  ;; 単純な木の作成
  (let [root (tree/make-node 2 
                            (tree/make-leaf 1) 
                            (tree/make-leaf 3))]
    (println "作成した木: " root)
    (println "  サイズ:" (tree/tree-size root))
    (println "  高さ:" (tree/tree-height root))
    (println "  先行順:" (tree/preorder-traversal root))
    (println "  中順:" (tree/inorder-traversal root))
    (println "  後行順:" (tree/postorder-traversal root))))

(defn demo-binary-search-tree
  "二分探索木のデモ"
  []
  (println "\n=== 二分探索木のデモ ===")
  
  ;; BSTの作成と操作
  (let [bst (-> nil
                (tree/insert-bst 5)
                (tree/insert-bst 3)
                (tree/insert-bst 7)
                (tree/insert-bst 2)
                (tree/insert-bst 4)
                (tree/insert-bst 6)
                (tree/insert-bst 8))]
    (println "BST作成（5,3,7,2,4,6,8を挿入）")
    (println "  中順走査（ソート順）:" (tree/inorder-traversal bst))
    (println "  最小値:" (tree/find-min bst))
    
    ;; 検索
    (println "  5を検索:" (tree/search-bst bst 5))
    (println "  9を検索:" (tree/search-bst bst 9))
    
    ;; 削除
    (let [bst-after-removal (tree/remove-bst bst 3)]
      (println "  3を削除後の中順走査:" (tree/inorder-traversal bst-after-removal)))))

(defn demo-heap
  "ヒープのデモ"
  []
  (println "\n=== ヒープのデモ ===")
  
  ;; 最小ヒープ
  (let [min-heap (tree/make-heap)]
    (println "最小ヒープを作成")
    (tree/heap-add min-heap 5)
    (tree/heap-add min-heap 3)
    (tree/heap-add min-heap 7)
    (tree/heap-add min-heap 1)
    (tree/heap-add min-heap 9)
    
    (println "  要素を追加（5,3,7,1,9）")
    (println "  サイズ:" (tree/heap-size min-heap))
    (println "  最小要素:" (tree/heap-peek min-heap))
    
    (println "  取り出し順序:")
    (while (not (tree/heap-empty? min-heap))
      (println "    " (tree/heap-poll min-heap))))
  
  ;; 最大ヒープ
  (let [max-heap (tree/make-heap #(compare %2 %1))]
    (println "\n最大ヒープを作成")
    (tree/heap-add max-heap 5)
    (tree/heap-add max-heap 3)
    (tree/heap-add max-heap 7)
    (tree/heap-add max-heap 1)
    (tree/heap-add max-heap 9)
    
    (println "  要素を追加（5,3,7,1,9）")
    (println "  サイズ:" (tree/heap-size max-heap))
    (println "  最大要素:" (tree/heap-peek max-heap))
    
    (println "  取り出し順序:")
    (while (not (tree/heap-empty? max-heap))
      (println "    " (tree/heap-poll max-heap)))))

(defn demo-complex-tree
  "複雑な木のデモ"
  []
  (println "\n=== 複雑な木のデモ ===")
  
  ;; より複雑な木構造
  (let [complex-tree (tree/make-node 1
                                    (tree/make-node 2
                                                   (tree/make-leaf 4)
                                                   (tree/make-leaf 5))
                                    (tree/make-node 3
                                                   (tree/make-leaf 6)
                                                   (tree/make-leaf 7)))]
    (println "複雑な木構造:")
    (println "       1")
    (println "      / \\")
    (println "     2   3")
    (println "    / \\ / \\")
    (println "   4  5 6  7")
    (println)
    (println "  サイズ:" (tree/tree-size complex-tree))
    (println "  高さ:" (tree/tree-height complex-tree))
    (println "  先行順:" (tree/preorder-traversal complex-tree))
    (println "  中順:" (tree/inorder-traversal complex-tree))
    (println "  後行順:" (tree/postorder-traversal complex-tree))))

(defn -main
  "すべてのデモを実行"
  [& args]
  (demo-basic-tree)
  (demo-binary-search-tree)
  (demo-heap)
  (demo-complex-tree))
