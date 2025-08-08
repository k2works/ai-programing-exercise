;; 正しいrecure実装の推定
;; 期待値分析:
;; recure(1) = [1] ✓
;; recure(2) = [1, 2] ✓  
;; recure(3) = [1, 2, 1, 3, 1] <- 期待値
;; recure(4) = [1, 2, 3, 1, 4, 1, 2] <- 期待値
;;
;; パターン分析: 
;; n=3の場合 [1, 2, 1, 3, 1] は以下の組み合わせ:
;; - recure(2) の結果 [1, 2] 
;; - 1 (n=3時点でのn-2=1)
;; - 3 (n自体)
;; - 1 (n=3時点でのn-2=1)

(defn recure-correct [n result-atom]
  (when (> n 0)
    (recure-correct (dec n) result-atom)
    (when (> (- n 2) 0)
      (swap! result-atom conj (- n 2)))
    (swap! result-atom conj n)
    (when (> (- n 2) 0)
      (swap! result-atom conj (- n 2))))
  result-atom)
