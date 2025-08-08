(ns algorithm-clj.demos.core-demo
  (:require [algorithm-clj.algorithms.core :as basic]))

(defn -main []
  (println "=== アルゴリズムから始めるClojure入門 第1章 デモ ===\n")
  
  ;; 3値の最大値
  (println "【3値の最大値】")
  (println "max3(3, 2, 1) =" (basic/max3 3 2 1))
  (println "max3(1, 3, 2) =" (basic/max3 1 3 2))
  (println "max3-imperative(3, 2, 1) =" (basic/max3-imperative 3 2 1))
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
  (println (basic/triangle-lb 5))

 ;; リストのデモ
 (println "\n【リストのデモ】")
 (println "Clojureの組み込みリスト:")
 (let [my-list '(1 2 3 4 5)]
   (println "元のリスト:" my-list)
   (println "先頭に0を追加:" (cons 0 my-list))
   (println "先頭の要素:" (first my-list))
   (println "先頭以外の要素:" (rest my-list))
   (println "要素数:" (count my-list)))

 (println "\nJava LinkedListのデモ:")
 (let [ll (basic/make-java-linked-list)]
   (println "初期サイズ:" (.size ll))
   (basic/add-first-java-linked-list ll 2)
   (basic/add-first-java-linked-list ll 1)
   (basic/add-last-java-linked-list ll 3)
   (println "要素追加後:" (seq ll))
   (println "要素2のインデックス:" (basic/search-java-linked-list ll 2))
   (println "先頭要素を削除:" (basic/remove-first-java-linked-list ll))
   (println "末尾要素を削除:" (basic/remove-last-java-linked-list ll))
   (println "削除後のリスト:" (seq ll)))

 ;; 木構造のデモ
 (println "\n【木構造のデモ】")
 (let [leaf1 (basic/make-tree-leaf 1)
       leaf2 (basic/make-tree-leaf 2)
       tree (basic/make-tree-node 0 leaf1 leaf2)]
   (println "木のサイズ:" (basic/simple-tree-size tree))
   (println "ノードが葉かどうか:" (basic/tree-leaf? leaf1))

   ;; TreeNodeレコードの直接使用デモ
   (println "\n【TreeNodeレコード直接使用デモ】")
   (let [tree-node (basic/->TreeNode "root"
                                    (basic/->TreeNode "left" nil nil)
                                    (basic/->TreeNode "right" nil nil))]
     (println "TreeNodeレコード作成:" tree-node)
     (println "  ルート値:" (:value tree-node))
     (println "  左の子の値:" (get-in tree-node [:left :value]))
     (println "  右の子の値:" (get-in tree-node [:right :value])))))
