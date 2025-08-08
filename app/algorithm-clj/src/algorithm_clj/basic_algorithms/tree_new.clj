(ns algorithm-clj.basic-algorithms.tree
  (:import [java.util PriorityQueue]))

;; 基本的な木のノード（不変）
(defrecord TreeNode [value left right])

;; 二分探索木のノード（不変）
(defrecord BSTNode [value left right])

;; 基本的な木の操作

(defn make-leaf
  "葉ノードを作成"
  [value]
  (->TreeNode value nil nil))

(defn make-node
  "内部ノードを作成"
  [value left right]
  (->TreeNode value left right))

(defn leaf?
  "ノードが葉かどうか判定"
  [node]
  (and (nil? (:left node)) (nil? (:right node))))

(defn tree-size
  "木のノード数を計算"
  [node]
  (if (nil? node)
    0
    (+ 1 (tree-size (:left node)) (tree-size (:right node)))))

(defn tree-height
  "木の高さを計算"
  [node]
  (if (nil? node)
    0
    (+ 1 (max (tree-height (:left node)) (tree-height (:right node))))))

;; 木の走査

(defn preorder-traversal
  "先行順走査"
  [node]
  (if (nil? node)
    []
    (concat [(:value node)]
            (preorder-traversal (:left node))
            (preorder-traversal (:right node)))))

(defn inorder-traversal
  "中順走査"
  [node]
  (if (nil? node)
    []
    (concat (inorder-traversal (:left node))
            [(:value node)]
            (inorder-traversal (:right node)))))

(defn postorder-traversal
  "後行順走査"
  [node]
  (if (nil? node)
    []
    (concat (postorder-traversal (:left node))
            (postorder-traversal (:right node))
            [(:value node)])))

;; 二分探索木の操作（シンプル版）

(defn insert-bst
  "BST に値を挿入（不変）"
  [root value]
  (if (nil? root)
    (->BSTNode value nil nil)
    (let [{:keys [value current-value left right]} root]
      (cond
        (< value current-value) 
        (->BSTNode current-value (insert-bst left value) right)
        
        (> value current-value) 
        (->BSTNode current-value left (insert-bst right value))
        
        :else root)))) ; 既に存在する値は変更なし

(defn search-bst
  "BST で値を検索"
  [root value]
  (cond
    (nil? root) false
    (= value (:value root)) true
    (< value (:value root)) (search-bst (:left root) value)
    :else (search-bst (:right root) value)))

(defn find-min
  "BST の最小値を検索"
  [root]
  (if (nil? (:left root))
    (:value root)
    (recur (:left root))))

(defn remove-bst
  "BST から値を削除（不変）"
  [root value]
  (if (nil? root)
    nil
    (let [{:keys [value current-value left right]} root]
      (cond
        (< value current-value)
        (->BSTNode current-value (remove-bst left value) right)
        
        (> value current-value)
        (->BSTNode current-value left (remove-bst right value))
        
        :else ; 削除対象ノード
        (cond
          (and (nil? left) (nil? right)) nil ; 葉ノード
          (nil? left) right ; 右の子のみ
          (nil? right) left ; 左の子のみ
          :else ; 両方の子がある
          (let [min-value (find-min right)
                new-right (remove-bst right min-value)]
            (->BSTNode min-value left new-right)))))))

;; ヒープ操作（Java PriorityQueue のラッパー）

(defn make-heap
  "ヒープを作成"
  ([]
   (PriorityQueue.))
  ([comparator]
   (PriorityQueue. comparator)))

(defn heap-add
  "ヒープに要素を追加"
  [heap element]
  (.add heap element)
  heap)

(defn heap-peek
  "ヒープの最小要素を参照"
  [heap]
  (.peek heap))

(defn heap-poll
  "ヒープの最小要素を取り出し"
  [heap]
  (.poll heap))

(defn heap-size
  "ヒープのサイズを取得"
  [heap]
  (.size heap))

(defn heap-empty?
  "ヒープが空かどうか判定"
  [heap]
  (.isEmpty heap))
