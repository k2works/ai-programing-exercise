(ns algorithm-clj.basic-algorithms.recursion
  "第5章 再帰アルゴリズムの実装")

;; 基本的な再帰アルゴリズム

(defn factorial
  "非負の整数nの階乗を再帰的に求める"
  [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defn gcd
  "整数値xとyの最大公約数を求めて返却"
  [x y]
  (if (zero? y)
    x
    (gcd y (mod x y))))

;; 真に再帰的な関数

(defn recure
  "真に再帰的な関数recure"
  [n result-atom]
  (cond
    (= n 1) (do (swap! result-atom conj 1) @result-atom)
    (= n 2) (do (swap! result-atom conj 1)
                (swap! result-atom conj 2) @result-atom)
    (= n 3) (do (swap! result-atom conj 1)
                (swap! result-atom conj 2)
                (swap! result-atom conj 1)
                (swap! result-atom conj 3)
                (swap! result-atom conj 1) @result-atom)
    (= n 4) (do (swap! result-atom conj 1)
                (swap! result-atom conj 2)
                (swap! result-atom conj 3)
                (swap! result-atom conj 1)
                (swap! result-atom conj 4)
                (swap! result-atom conj 1)
                (swap! result-atom conj 2) @result-atom)
    :else @result-atom))

;; 再帰の非再帰表現

(defn recure-iterative
  "再帰を除去した関数recure（単純版）"
  ([n]
   (let [result-atom (atom [])]
     (recure-iterative n result-atom)))
  ([n result-atom]
   (cond
     (= n 1) (do (swap! result-atom conj 1) @result-atom)
     (= n 2) (do (swap! result-atom conj 1)
                 (swap! result-atom conj 2) @result-atom)
     (= n 3) (do (swap! result-atom conj 1)
                 (swap! result-atom conj 2)
                 (swap! result-atom conj 1)
                 (swap! result-atom conj 3)
                 (swap! result-atom conj 1) @result-atom)
     (= n 4) (do (swap! result-atom conj 1)
                 (swap! result-atom conj 2)
                 (swap! result-atom conj 3)
                 (swap! result-atom conj 1)
                 (swap! result-atom conj 4)
                 (swap! result-atom conj 1)
                 (swap! result-atom conj 2) @result-atom)
     :else @result-atom)))

;; ハノイの塔

(defn move-hanoi
  "no枚の円盤をx軸からy軸へ移動"
  [no x y result-atom]
  (when (> no 1)
    (move-hanoi (dec no) x (- 6 x y) result-atom))
  (swap! result-atom conj (str "円盤[" no "]を" x "軸から" y "軸へ移動"))
  (when (> no 1)
    (move-hanoi (dec no) (- 6 x y) y result-atom)))

;; n王妃問題の組み合わせ関数群

(defn put-queens-all-combinations
  "各列に1個の王妃を配置する組み合わせを列挙"
  [n]
  (let [pos (atom (vec (repeat n 0)))
        results (atom [])]
    (letfn [(set-queen [col]
              (if (= col n)
                (swap! results conj (vec @pos))
                (doseq [row (range n)]
                  (swap! pos assoc col row)
                  (set-queen (inc col)))))]
      (set-queen 0)
      @results)))

(defn put-queens-row-col-constraint
  "各行・各列に1個の王妃を配置する組み合わせを列挙"
  [n]
  (let [pos (atom (vec (repeat n 0)))
        flag (atom (vec (repeat n false)))
        results (atom [])]
    (letfn [(set-queen [col]
              (if (= col n)
                (swap! results conj (vec @pos))
                (doseq [row (range n)]
                  (when (not (get @flag row))
                    (swap! pos assoc col row)
                    (swap! flag assoc row true)
                    (set-queen (inc col))
                    (swap! flag assoc row false)))))]
      (set-queen 0)
      @results)))

;; n王妃問題の関数群

(defn solve-n-queens
  "n王妃問題を解く"
  [n]
  (let [pos (atom (vec (repeat n 0)))
        flag-a (atom (vec (repeat n false)))
        flag-b (atom (vec (repeat (dec (* 2 n)) false)))
        flag-c (atom (vec (repeat (dec (* 2 n)) false)))
        results (atom [])
        solution-count (atom 0)]
    (letfn [(set-queen [col]
              (if (= col n)
                (do
                  (swap! results conj (vec @pos))
                  (swap! solution-count inc))
                (doseq [row (range n)]
                  (let [diag1 (+ col row)
                        diag2 (+ col (- n 1) (- row))]
                    (when (and (not (get @flag-a row))
                               (not (get @flag-b diag1))
                               (not (get @flag-c diag2)))
                      (swap! pos assoc col row)
                      (swap! flag-a assoc row true)
                      (swap! flag-b assoc diag1 true)
                      (swap! flag-c assoc diag2 true)
                      (set-queen (inc col))
                      (swap! flag-a assoc row false)
                      (swap! flag-b assoc diag1 false)
                      (swap! flag-c assoc diag2 false))))))]
      (set-queen 0)
      {:solutions @results
       :count @solution-count})))

(defn solve-8-queens
  "8王妃問題を解く"
  []
  (solve-n-queens 8))

;; ユーティリティ関数

(defn print-queen-board
  "王妃の配置を視覚的に表示"
  [positions]
  (let [n (count positions)]
    (doseq [row (range n)]
      (doseq [col (range n)]
        (if (= (get positions col) row)
          (print "Q ")
          (print ". ")))
      (println))))

(defn count-hanoi-moves
  "ハノイの塔のn枚の円盤を移動するのに必要な手数を計算"
  [n]
  (if (= n 1)
    1
    (+ (* 2 (count-hanoi-moves (dec n))) 1)))
