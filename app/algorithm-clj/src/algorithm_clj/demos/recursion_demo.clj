(ns algorithm-clj.demos.recursion-demo
  "第5章 再帰アルゴリズムのデモ"
  (:require [algorithm-clj.algorithms.recursion :as rec]
            [algorithm-clj.algorithms.recursion-simple :as rec-simple]))

;; デモ実行
(defn run-demo []
  (println "=== 第5章 再帰アルゴリズム デモ ===")
  (println)
  
  (println "1. 階乗の計算:")
  (doseq [n [3 4 5]]
    (println (str "  " n "! = " (rec/factorial n))))
  (println)
  
  (println "2. 最大公約数:")
  (println "  gcd(48, 18) = " (rec/gcd 48 18))
  (println "  gcd(17, 13) = " (rec/gcd 17 13))
  (println)
  
  (println "3. 真に再帰的な関数recure:")
  (doseq [n [1 2 3 4]]
    (let [result-atom (atom [])]
      (rec/recure n result-atom)
      (println (str "  recure(" n ") = " @result-atom))))

  (println)
  (println "4. ハノイの塔デモ:")
  (let [result-atom (atom [])]
    (rec-simple/move-hanoi 3 1 3 result-atom)
    (println "  3枚の円盤を1軸から3軸へ移動:")
    (doseq [move (take 5 @result-atom)]
      (println "    " move))
    (when (> (count @result-atom) 5)
      (println "    ... (全" (count @result-atom) "手)")))

  (println)
  (println "5. 8王妃問題の解の数:")
  (let [result (rec-simple/solve-8-queens)]
    (println "  解の総数:" (:count result)))

  (println)
  (println "6. 8王妃問題の詳細デモ:")
  (println "  8王妃問題の全解を表示（最初の3つ）:")
  (let [result (rec/solve-8-queens)]
    (when (pos? (:count result))
      (doseq [[i solution] (map-indexed vector (take 3 (:solutions result)))]
        (println (str "    解" (inc i) ": " solution))
        (rec/print-queen-board solution))))

  (println)
  (println "7. ハノイの塔の移動回数:")
  (doseq [n [3 4 5]]
    (println (str "  " n "枚の円盤: " (rec/count-hanoi-moves n) "回")))

  (println)
  (println "8. ハノイの塔の実際の移動手順:")
  (let [result-atom (atom [])]
    (rec/move-hanoi 3 1 3 result-atom)
    (println "  3枚の円盤を1軸から3軸へ移動する手順:")
    (doseq [move @result-atom]
      (println "    " move)))

  (println)
  (println "9. 8王妃問題の全解組み合わせ:")
  (let [result (rec/put-queens-all-combinations 4)]  ; 4王妃問題で実演
    (println "  4王妃問題の全解の数:" (count result))
    (println "  最初の3つの解:")
    (doseq [[i solution] (map-indexed vector (take 3 result))]
      (println (str "    解" (inc i) ": " solution))))

  (println)
  (println "10. 再帰の反復版デモ:")
  (let [result-atom (atom [])]
    (rec-simple/recure-iterative 3 result-atom)
    (println "  recure-iterative(3) = " @result-atom))

  (println)
  (println "11. シンプル版recure関数の比較:")
  (let [result-atom1 (atom [])
        result-atom2 (atom [])]
    (rec/recure 3 result-atom1)
    (rec-simple/recure 3 result-atom2)
    (println "  通常版recure(3) = " @result-atom1)
    (println "  シンプル版recure(3) = " @result-atom2))

  (println)
  (println "12. N王妃問題の制約版デモ:")
  (let [result (rec/put-queens-row-col-constraint 4)]  ; 4王妃問題で実演
    (println "  4王妃問題（行・列制約版）の解の数:" (count result))
    (println "  最初の2つの解:")
    (doseq [[i solution] (map-indexed vector (take 2 result))]
      (println (str "    解" (inc i) ": " solution))))

  (println)
  (println "13. 再帰の反復版比較デモ:")
  (let [result-atom1 (atom [])
        result-atom2 (atom [])]
    (rec/recure 3 result-atom1)
    (rec-simple/recure-iterative 3 result-atom2)
    (println "  再帰版recure(3) = " @result-atom1)
    (println "  反復版recure-iterative(3) = " @result-atom2))
  (println))

;; メイン実行
(defn -main [& args]
  (run-demo))
