(ns algorithm-clj.algorithms.sorting
  "第6章: ソートアルゴリズムの実装")

;; ============================
;; バブルソート
;; ============================

(defn bubble-sort
  "バブルソート: 隣接する要素を比較して交換を繰り返す"
 [coll]
 (let [arr (vec coll)
       n (count arr)]
   (if (< n 2)
     arr
     (reduce (fn [acc _]
               (reduce (fn [inner-arr i]
                         (if (and (< i (dec n))
                                  (> (get inner-arr i) (get inner-arr (inc i))))
                           (assoc inner-arr
                                  i (get inner-arr (inc i))
                                  (inc i) (get inner-arr i))
                           inner-arr))
                       acc
                       (range (dec n))))
             arr
             (range n)))))

;; ============================
;; 選択ソート
;; ============================

(defn selection-sort
  "選択ソート: 最小値を選択して先頭から配置"
  [coll]
  (let [arr (vec coll)
        n (count arr)]
    (if (< n 2)
      arr
      (reduce (fn [acc i]
                (let [min-idx (reduce (fn [min-i j]
                                        (if (< (get acc j) (get acc min-i))
                                          j
                                          min-i))
                                      i
                                      (range (inc i) n))]
                  (if (not= i min-idx)
                    (assoc acc
                           i (get acc min-idx)
                           min-idx (get acc i))
                    acc)))
              arr
              (range n)))))

;; ============================
;; 挿入ソート
;; ============================

(defn insertion-sort
  "挿入ソート: 要素を適切な位置に挿入"
  [coll]
  (let [arr (vec coll)]
    (if (< (count arr) 2)
      arr
      (reduce (fn [acc i]
                (let [key (get acc i)]
                  (loop [j (dec i)
                         current-arr acc]
                    (if (or (< j 0) (<= (get current-arr j) key))
                      (assoc current-arr (inc j) key)
                      (recur (dec j)
                             (assoc current-arr (inc j) (get current-arr j)))))))
              arr
              (range 1 (count arr))))))

;; ============================
;; シェルソート
;; ============================

(defn shell-sort
  "シェルソート: ギャップを使った挿入ソートの改良版"
  [coll]
  (let [arr (vec coll)]
    (if (< (count arr) 2)
      arr
      (loop [gap (quot (count arr) 2)
             current-arr arr]
        (if (zero? gap)
          current-arr
          (let [sorted-arr (reduce (fn [acc i]
                                     (let [temp (get acc i)]
                                       (loop [j i
                                              inner-arr acc]
                                         (if (or (< j gap)
                                                 (<= (get inner-arr (- j gap)) temp))
                                           (assoc inner-arr j temp)
                                           (recur (- j gap)
                                                  (assoc inner-arr j (get inner-arr (- j gap))))))))
                                   current-arr
                                   (range gap (count current-arr)))]
            (recur (quot gap 2) sorted-arr)))))))

;; ============================
;; クイックソート
;; ============================

(defn quick-sort
  "クイックソート: 分割統治法による高速ソート"
  [coll]
  (if (< (count coll) 2)
    (vec coll)
    (let [pivot (first coll)
          rest-coll (rest coll)
          lesser (filter #(<= % pivot) rest-coll)
          greater (filter #(> % pivot) rest-coll)]
      (vec (concat (quick-sort lesser) [pivot] (quick-sort greater))))))

;; ============================
;; マージソート
;; ============================

(defn merge-sorted
  "2つのソート済み配列をマージ"
  [left right]
  (loop [l left
         r right
         result []]
    (cond
      (empty? l) (vec (concat result r))
      (empty? r) (vec (concat result l))
      (<= (first l) (first r)) (recur (rest l) r (conj result (first l)))
      :else (recur l (rest r) (conj result (first r))))))

(defn merge-sort
  "マージソート: 分割統治法による安定ソート"
  [coll]
  (if (< (count coll) 2)
    (vec coll)
    (let [mid (quot (count coll) 2)
          left (subvec (vec coll) 0 mid)
          right (subvec (vec coll) mid)]
      (merge-sorted (merge-sort left) (merge-sort right)))))

;; ============================
;; ヒープソート
;; ============================

(defn heapify
  "ヒープ条件を満たすように調整"
  [arr n i]
  (let [left (inc (* 2 i))
        right (+ 2 (* 2 i))
        largest (cond
                  (and (< left n) (> (get arr left) (get arr i))) left
                  :else i)
        largest (cond
                  (and (< right n) (> (get arr right) (get arr largest))) right
                  :else largest)]
    (if (not= largest i)
      (heapify (assoc arr
                      i (get arr largest)
                      largest (get arr i))
               n largest)
      arr)))

(defn build-max-heap
  "最大ヒープを構築"
  [arr]
  (let [n (count arr)]
    (reduce (fn [acc i]
              (heapify acc n i))
            arr
            (range (quot (dec n) 2) -1 -1))))

(defn heap-sort
  "ヒープソート: ヒープ構造を利用したソート"
  [coll]
  (if (< (count coll) 2)
   (vec coll)
   (let [arr (vec coll)
         heap (build-max-heap arr)]
     (loop [current-heap heap
            n (count heap)
            result []]
       (if (<= n 1)
         (let [final-result (if (= n 1)
                     (conj result (first current-heap))
                              result)]
 (vec (reverse final-result)))
         (let [max-elem (first current-heap)
               new-heap (assoc current-heap 0 (get current-heap (dec n)))
               heapified (heapify new-heap (dec n) 0)]
           (recur heapified (dec n) (conj result max-elem))))))))
