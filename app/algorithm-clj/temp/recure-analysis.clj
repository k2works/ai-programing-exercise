;; recure関数の動作分析
;; 期待値: recure(3) = [1 2 1 3 1]
;; 
;; 分析:
;; recure(3) -> recure(2) -> recure(1) -> add(1) -> recure(-1) [無効]
;;                        -> add(2) -> recure(0) [無効]
;;           -> add(3) -> recure(1) -> add(1)
;;
;; 実行順序: [1, 2, 3, 1] ← 現在の実装
;; 期待順序: [1, 2, 1, 3, 1]
;;
;; 修正案: 2番目の再帰呼び出しを先に実行する

(ns algorithm-clj.algorithms.recure-analysis)
