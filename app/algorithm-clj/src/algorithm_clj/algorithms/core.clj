(ns algorithm-clj.algorithms.core)

;; 3値の最大値を求める
(defn max3
  "三つの整数値を読み込んで最大値を求めて表示"
  [a b c]
  (max a b c))

(defn max3-imperative
  "三つの整数値を読み込んで最大値を求めて表示（命令型スタイル）"
  [a b c]
  (let [maximum (atom a)] ; atomを使って可変状態を表現
    (when (> b @maximum)  ; @でatomの現在の値を取得
      (reset! maximum b)) ; reset!でatomの値を更新
    (when (> c @maximum)
      (reset! maximum c))
    @maximum))

;; 3値の中央値を求める
(defn med3
  "a,b,cの中央値を求めて返却"
  [a b c]
  (cond
    (>= a b) (cond
               (>= b c) b
               (<= a c) a
               :else c)
    (> a c) a
    (> b c) c
    :else b))

;; 符号判定
(defn judge-sign
  "読み込んだ整数値の符号を表示"
  [n]
  (cond
    (pos? n) "その値は正です。"    ; (> n 0) から変更
    (neg? n) "その値は負です。"    ; (< n 0) から変更
    :else "その値は0です。"))

;; 1からnまでの総和
(defn sum-1-to-n-while
  "loop/recurによる繰り返し"
  [n]
  (loop [sum 0
         i 1]
    (if (<= i n)
      (recur (+ sum i) (inc i))
      sum)))

(defn sum-1-to-n-for
  "reduceによる繰り返し"
  [n]
  (reduce + (range 1 (inc n))))

;; 記号文字の交互表示
(defn alternative-1
  "記号文字+と-を交互に表示（その１）"
  [n]
  (apply str
         (for [i (range n)]
           (if (odd? i) "-" "+"))))

(defn alternative-2
  "記号文字+と-を交互に表示（その２）"
  [n]
  (str (apply str (repeat (quot n 2) "+-"))
       (when (odd? n) "+")))

;; 長方形の辺の長さを列挙
(defn rectangle
  "縦横が整数で面積がareaの長方形の辺の長さを列挙"
  [area]
  (apply str
         (for [i (range 1 (inc area))
               :when (<= (* i i) area) ; i*i > area ならループを抜ける（breakに相当）
               :when (zero? (mod area i))] ; area % i != 0 ならスキップ（continueに相当）
           (str i "x" (quot area i) " "))))

;; 九九の表
(defn multiplication-table
  "九九の表を表示"
  []
  (str (apply str (repeat 27 "-")) "\n"
       (apply str
              (for [i (range 1 10)]
                (str (apply str
                            (for [j (range 1 10)]
                              (format "%3d" (* i j))))
                     "\n")))
       (apply str (repeat 27 "-"))))

;; 直角三角形の表示
(defn triangle-lb
  "左下側が直角の二等辺三角形を表示"
  [n]
  (apply str
         (for [i (range n)]
           (str (apply str (repeat (inc i) "*")) "\n"))))

;; リスト関連の基本関数

(defn make-java-linked-list
  "新しいJava LinkedListを作成"
  []
  (java.util.LinkedList.))

(defn add-first-java-linked-list
  "LinkedListの先頭に要素を追加"
  [^java.util.LinkedList ll data]
  (.addFirst ll data))

(defn add-last-java-linked-list
  "LinkedListの末尾に要素を追加"
  [^java.util.LinkedList ll data]
  (.addLast ll data))

(defn remove-first-java-linked-list
  "LinkedListの先頭要素を削除して返す"
  [^java.util.LinkedList ll]
  (.removeFirst ll))

(defn remove-last-java-linked-list
  "LinkedListの末尾要素を削除して返す"
  [^java.util.LinkedList ll]
  (.removeLast ll))

(defn search-java-linked-list
  "LinkedList内で指定したデータのインデックスを検索"
  [^java.util.LinkedList ll data]
  (.indexOf ll data))

;; 木構造関連の基本関数

(defrecord TreeNode [value left right])

(defn make-tree-leaf
  "葉ノードを作成"
  [value]
  (TreeNode. value nil nil))

(defn make-tree-node
  "内部ノードを作成"
  [value left right]
  (TreeNode. value left right))

(defn tree-leaf?
  "ノードが葉かどうかを判定"
  [node]
  (and (nil? (:left node)) (nil? (:right node))))

(defn simple-tree-size
  "木のノード数を計算"
  [node]
  (if (nil? node)
    0
    (+ 1 (simple-tree-size (:left node)) (simple-tree-size (:right node)))))
