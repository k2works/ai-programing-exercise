(ns algorithm-clj.demo
  (:require [algorithm-clj.basic-algorithms.core :as basic]))

(defn -main []
  (println "=== アルゴリズムから始めるClojure入門 第1章 デモ ===\n")
  
  ;; 3値の最大値
  (println "【3値の最大値】")
  (println "max3(3, 2, 1) =" (basic/max3 3 2 1))
  (println "max3(1, 3, 2) =" (basic/max3 1 3 2))
  (println)
  
  ;; 3値の中央値
  (println "【3値の中央値】")
  (println "med3(3, 2, 1) =" (basic/med3 3 2 1))
  (println "med3(1, 3, 2) =" (basic/med3 1 3 2))
  (println)
  
  ;; 符号判定
  (println "【符号判定】")
  (println "judge-sign(17) =" (basic/judge-sign 17))
  (println "judge-sign(-5) =" (basic/judge-sign -5))
  (println "judge-sign(0) =" (basic/judge-sign 0))
  (println)
  
  ;; 1からnまでの総和
  (println "【1からnまでの総和】")
  (println "sum-1-to-n-while(5) =" (basic/sum-1-to-n-while 5))
  (println "sum-1-to-n-for(5) =" (basic/sum-1-to-n-for 5))
  (println)
  
  ;; 記号文字の交互表示
  (println "【記号文字の交互表示】")
  (println "alternative-1(12) =" (basic/alternative-1 12))
  (println "alternative-2(12) =" (basic/alternative-2 12))
  (println)
  
  ;; 長方形の辺の長さを列挙
  (println "【長方形の辺の長さを列挙】")
  (println "rectangle(32) =" (basic/rectangle 32))
  (println "rectangle(12) =" (basic/rectangle 12))
  (println)
  
  ;; 九九の表
  (println "【九九の表】")
  (println (basic/multiplication-table))
  (println)
  
  ;; 直角三角形の表示
  (println "【直角三角形の表示】")
  (println (basic/triangle-lb 5)))
