(ns algorithm-clj.algorithms.search)

;; 線形探索（loop/recur版）
(defn ssearch-while
  "シーケンスaからsearch-keyと等価な要素を線形探索（loop/recur）"
  [a search-key]
  (loop [i 0]
    (cond
      (= i (count a)) -1 ; 探索失敗
      (= (nth a i) search-key) i ; 探索成功
      :else (recur (inc i))))) ; 次の要素へ

;; 線形探索（for版）
(defn ssearch-for
  "シーケンスaからsearch-keyと等価な要素を線形探索（for）"
  [a search-key]
  (if-let [result (first (for [[idx val] (map-indexed vector a)
                                :when (= val search-key)]
                            idx))]
    result
    -1))

;; 番兵法による線形探索
(defn ssearch-sentinel
  "シーケンスaからsearch-keyと一致する要素を線形探索（番兵法）"
  [a search-key]
  (let [a-with-sentinel (conj (vec a) search-key) ; ベクターに番兵を追加
        n (count a)
        result (loop [i 0]
                 (if (= (nth a-with-sentinel i) search-key)
                   i
                   (recur (inc i))))]
    (if (= result n) ; 番兵に一致したかどうかを判定
      -1
      result)))

;; 二分探索
(defn bsearch
  "シーケンスaからsearch-keyと一致する要素を二分探索"
  [a search-key]
  (loop [pl 0 ; 探索範囲の先頭要素のインデックス
         pr (- (count a) 1)] ; 探索範囲の末尾要素のインデックス
    (if (> pl pr)
      -1 ; 探索失敗
      (let [pc (quot (+ pl pr) 2) ; 中央要素のインデックス
            val (nth a pc)]
        (cond
          (= val search-key) pc ; 探索成功
          (< val search-key) (recur (inc pc) pr) ; 探索範囲を後半に絞り込む
          :else (recur pl (dec pc))))))) ; 探索範囲を前半に絞り込む

;; ===== チェイン法によるハッシュテーブル =====

(defrecord Node [hash-key value node-next])

(defrecord ChainedHash [capacity table])

(defn make-chained-hash [capacity]
  (->ChainedHash capacity (atom (vec (repeat capacity nil)))))

(defn hash-value-chained [^ChainedHash ch hash-key]
  (mod (hash hash-key) (:capacity ch)))

(defn search-chained-hash [^ChainedHash ch hash-key]
  (let [h (hash-value-chained ch hash-key)
        bucket (get @(:table ch) h)]
    (loop [node bucket]
      (when node
        (if (= (:hash-key node) hash-key)
          (:value node)
          (recur (:node-next node)))))))

(defn add-chained-hash [^ChainedHash ch hash-key value]
  (let [h (hash-value-chained ch hash-key)
        current-bucket (get @(:table ch) h)]
    (if (search-chained-hash ch hash-key) ; 既に存在するかチェック
      false
      (do
        (swap! (:table ch) assoc h (->Node hash-key value current-bucket))
        true))))

(defn remove-chained-hash [^ChainedHash ch hash-key]
  (let [h (hash-value-chained ch hash-key)]
    (letfn [(remove-from-list [node]
              (when node
                (if (= (:hash-key node) hash-key)
                  (:node-next node) ; このノードを削除（次のノードを返す）
                  (assoc node :node-next (remove-from-list (:node-next node))))))]
      (let [current-bucket (get @(:table ch) h)
            new-bucket (remove-from-list current-bucket)]
        (if (not= current-bucket new-bucket)
          (do
            (swap! (:table ch) assoc h new-bucket)
            true)
          false)))))

;; ===== オープンアドレス法によるハッシュテーブル =====

(defrecord Bucket [hash-key value status])

(def ^:const OCCUPIED :occupied)
(def ^:const EMPTY :empty)
(def ^:const DELETED :deleted)

(defrecord OpenHash [capacity table])

(defn make-open-hash [capacity]
  (->OpenHash capacity (atom (vec (repeat capacity (->Bucket nil nil EMPTY))))))

(defn hash-value-open [^OpenHash oh hash-key]
  (mod (hash hash-key) (:capacity oh)))

(defn rehash-value-open [^OpenHash oh h]
  (mod (inc h) (:capacity oh)))

(defn search-open-hash [^OpenHash oh hash-key]
  (let [capacity (:capacity oh)]
    (loop [h (hash-value-open oh hash-key)
           i 0]
      (when (< i capacity)
        (let [^Bucket bucket (get @(:table oh) h)
              status (:status bucket)]
          (cond
            (= status EMPTY) nil ; 探索失敗
            (and (= status OCCUPIED) (= (:hash-key bucket) hash-key)) (:value bucket) ; 探索成功
            :else (recur (rehash-value-open oh h) (inc i))))))))

(defn add-open-hash [^OpenHash oh hash-key value]
  (let [capacity (:capacity oh)]
    (if (search-open-hash oh hash-key) ; 既に存在するかチェック
      false
      (loop [h (hash-value-open oh hash-key)
             i 0]
        (cond
          (>= i capacity) false ; 表が満杯
          (or (= (:status (get @(:table oh) h)) EMPTY)
              (= (:status (get @(:table oh) h)) DELETED))
          (do
            (swap! (:table oh) assoc h (->Bucket hash-key value OCCUPIED))
            true)
          :else (recur (rehash-value-open oh h) (inc i)))))))

(defn remove-open-hash [^OpenHash oh hash-key]
  (let [capacity (:capacity oh)]
    (loop [h (hash-value-open oh hash-key)
           i 0]
      (when (< i capacity)
        (let [^Bucket bucket (get @(:table oh) h)
              status (:status bucket)]
          (cond
            (= status EMPTY) false ; 削除失敗（見つからなかった）
            (and (= status OCCUPIED) (= (:hash-key bucket) hash-key))
            (do
              (swap! (:table oh) assoc h (assoc bucket :status DELETED))
              true)
            :else (recur (rehash-value-open oh h) (inc i))))))))

;; Mapの作成
(defn create-map
  "キーと値のペアのリストからマップを作成"
  [kvs]
  (into {} kvs))

;; 探索
(defn search-in-map
  "マップからキーに対応する値を取得（なければnil）"
  [k m]
  (get m k))

;; 追加・更新
(defn add-to-map
  "マップにキーと値を追加または更新"
  [k v m]
  (assoc m k v))

;; 削除
(defn remove-from-map
  "マップから指定キーを削除"
  [k m]
  (dissoc m k))