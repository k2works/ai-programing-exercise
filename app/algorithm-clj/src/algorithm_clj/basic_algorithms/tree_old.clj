(ns algorithm-clj.basic-algorithms.tree
  (:import [java.util PriorityQueue]))

;; 二分木のノードを表すレコード
(defrecord TreeNode [value left right])

;; 二分探索木のノード（可変）
(defrecord BSTNode [value left right])

;; 二分探索木を表すレコード
(defrecord BinarySearchTree [^:volatile-mutable root])

;; 基本的な二分木操作

(defn make-leaf
  "葉ノードを作成"
  [value]
  (TreeNode. value nil nil))

(defn make-node
  "内部ノードを作成"
  [value left right]
  (TreeNode. value left right))

(defn leaf?
  "ノードが葉かどうかを判定"
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

;; 二分探索木の操作

(defn make-binary-search-tree
  "空の二分探索木を作成"
  []
  (BinarySearchTree. (atom nil)))

(defn get-root
  "二分探索木のルートノードを取得"
  [^BinarySearchTree bst]
  @(:root bst))

(defn add-bst
  "二分探索木にノードを追加"
  [^BinarySearchTree bst key value]
  (let [new-node (BSTNode. key value (atom nil) (atom nil))]
    (if (nil? (get-root bst))
      (do (reset! (:root bst) new-node) true)
      (loop [current-node (get-root bst)]
        (cond
          (= key (:key current-node)) false ; キーが既に存在
          (< key (:key current-node))
          (if (nil? @(:left current-node))
            (do (reset! (:left current-node) new-node) true)
            (recur @(:left current-node)))
          :else ; (> key (:key current-node))
          (if (nil? @(:right current-node))
            (do (reset! (:right current-node) new-node) true)
            (recur @(:right current-node))))))))

(defn search-bst
  "二分探索木でキーを検索"
  [^BinarySearchTree bst key]
  (loop [current-node (get-root bst)]
    (cond
      (nil? current-node) nil ; 探索失敗
      (= key (:key current-node)) (:value current-node) ; 探索成功
      (< key (:key current-node)) (recur @(:left current-node))
      :else (recur @(:right current-node))))) ; (> key (:key current-node))

(defn bst-min
  "二分探索木の最小キーを取得"
  [^BinarySearchTree bst]
  (loop [current-node (get-root bst)]
    (if (or (nil? current-node) (nil? @(:left current-node)))
      (when current-node (:key current-node))
      (recur @(:left current-node)))))

(defn bst-max
  "二分探索木の最大キーを取得"
  [^BinarySearchTree bst]
  (loop [current-node (get-root bst)]
    (if (or (nil? current-node) (nil? @(:right current-node)))
      (when current-node (:key current-node))
      (recur @(:right current-node)))))

(defn dump-bst
  "二分探索木を中順走査で表示"
  [^BinarySearchTree bst]
  (letfn [(print-subtree [node]
            (when node
              (print-subtree @(:left node)) ; 左部分木をキーの昇順に表示
              (println (str (:key node) "  " (:value node))) ; nodeを表示
              (print-subtree @(:right node))))] ; 右部分木をキーの昇順に表示
    (print-subtree (get-root bst))))

(defn remove-bst
  "二分探索木からノードを削除"
  [^BinarySearchTree bst key]
  (letfn [(find-min [node]
            (loop [current node]
              (if (nil? @(:left current))
                current
                (recur @(:left current)))))
          
          (remove-node [node key]
            (cond
              (nil? node) nil
              (< key (:key node)) 
              (do (reset! (:left node) (remove-node @(:left node) key)) node)
              (> key (:key node))
              (do (reset! (:right node) (remove-node @(:right node) key)) node)
              :else ; key == (:key node)
              (cond
                (nil? @(:left node)) @(:right node)
                (nil? @(:right node)) @(:left node)
                :else ; 両方の子が存在
                (let [successor (find-min @(:right node))]
                  (BSTNode. (:key successor) (:value successor)
                           (:left node) 
                           (atom (remove-node @(:right node) (:key successor))))))))]
    
    (let [original-root (get-root bst)]
      (if (nil? original-root)
        false
        (do
          (reset! (:root bst) (remove-node original-root key))
          true)))))

;; ヒープ操作（JavaのPriorityQueueを使用）

(defn make-heap
  "空のヒープを作成"
  []
  (PriorityQueue.))

(defn heap-add
  "ヒープに要素を追加"
  [^PriorityQueue heap value]
  (.add heap value))

(defn heap-peek
  "ヒープの最小値を取得（削除しない）"
  [^PriorityQueue heap]
  (.peek heap))

(defn heap-poll
  "ヒープの最小値を取得して削除"
  [^PriorityQueue heap]
  (.poll heap))

(defn heap-size
  "ヒープのサイズを取得"
  [^PriorityQueue heap]
  (.size heap))

(defn heap-empty?
  "ヒープが空かどうかを判定"
  [^PriorityQueue heap]
  (.isEmpty heap))
