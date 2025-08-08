(ns algorithm-clj.algorithms.recursion-demo
  "第5章 再帰アルゴリズムのデモ"
  (:import [java.util ArrayDeque]))

;; 基本的な再帰関数
(defn factorial [n]
  (if (zero? n) 1 (* n (factorial (dec n)))))

(defn gcd [x y]
  (if (zero? y) x (gcd y (mod x y))))

;; 真に再帰的な関数の実装
(defn recure [n result-atom]
  (when (> n 0)
    (recure (dec n) result-atom)
    (swap! result-atom conj n)
    (recure (- n 2) result-atom))
  @result-atom)

;; デモ実行
(defn run-demo []
  (println "=== 第5章 再帰アルゴリズム デモ ===")
  (println)
  
  (println "1. 階乗の計算:")
  (doseq [n [3 4 5]]
    (println (str "  " n "! = " (factorial n))))
  (println)
  
  (println "2. 最大公約数:")
  (println "  gcd(48, 18) = " (gcd 48 18))
  (println "  gcd(17, 13) = " (gcd 17 13))
  (println)
  
  (println "3. 真に再帰的な関数recure:")
  (doseq [n [1 2 3 4]]
    (let [result-atom (atom [])]
      (recure n result-atom)
      (println (str "  recure(" n ") = " @result-atom))))
  (println))

;; メイン実行
(defn -main [& args]
  (run-demo))
