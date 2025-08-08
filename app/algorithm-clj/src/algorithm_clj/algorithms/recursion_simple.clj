(ns algorithm-clj.algorithms.recursion-simple
  "第5章 再帰アルゴリズムの実装（シンプル版）"
  (:require [algorithm-clj.algorithms.recursion :as rec])
  (:import [java.util ArrayDeque]))

;; 基本的な再帰アルゴリズムはrecursion.cljから利用
;; (factorial, gcdはrecursion.cljを参照)

;; 真に再帰的な関数（簡略版）

(defn recure
  "真に再帰的な関数recure（シンプル版）"
  [n result-atom]
  (when (> n 0)
    (recure (dec n) result-atom)
    (swap! result-atom conj n)
    (recure (- n 2) result-atom))
  @result-atom)

;; 再帰の非再帰表現

(defn recure-iterative
  "再帰を除去した関数recure"
  [n result-atom]
  (let [call-stack (ArrayDeque.)]
    (.push call-stack [n "start"])
    (while (not (.isEmpty call-stack))
      (let [[current-n state] (.pop call-stack)]
        (cond
          (<= current-n 0) 
          nil
          
          (= state "start")
          (do
            (.push call-stack [current-n "second"])
            (.push call-stack [current-n "middle"])
            (.push call-stack [(dec current-n) "start"]))
          
          (= state "middle")
          (swap! result-atom conj current-n)
          
          (= state "second")
          (when (> (- current-n 2) 0)
            (.push call-stack [(- current-n 2) "start"])))))
    @result-atom))

;; ハノイの塔

(defn move-hanoi
  "no枚の円盤をx軸からy軸へ移動"
  [no x y result-atom]
  (when (> no 1)
    (move-hanoi (dec no) x (- 6 x y) result-atom))
  (swap! result-atom conj (str "円盤[" no "]を" x "軸から" y "軸へ移動"))
  (when (> no 1)
    (move-hanoi (dec no) (- 6 x y) y result-atom)))

;; n王妃問題

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
