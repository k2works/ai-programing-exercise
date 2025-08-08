(ns algorithm-clj.basic-algorithms.tree-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.basic-algorithms.tree :refer :all]))

;; 二分木の基本操作のテスト

(deftest test-tree-creation
  (testing "木の作成"
    (let [leaf (make-leaf 42)]
      (is (= (:value leaf) 42))
      (is (nil? (:left leaf)))
      (is (nil? (:right leaf))))
    
    (let [left-child (make-leaf 1)
          right-child (make-leaf 3)
          root (make-node 2 left-child right-child)]
      (is (= (:value root) 2))
      (is (= (:value (:left root)) 1))
      (is (= (:value (:right root)) 3)))))

(deftest test-tree-properties
  (testing "木の性質"
    (let [leaf (make-leaf 42)]
      (is (leaf? leaf))
      (is (= (tree-size leaf) 1))
      (is (= (tree-height leaf) 1)))
    
    (let [tree (make-node 2 
                         (make-leaf 1) 
                         (make-leaf 3))]
      (is (not (leaf? tree)))
      (is (= (tree-size tree) 3))
      (is (= (tree-height tree) 2)))))

(deftest test-tree-traversal
  (testing "木の走査"
    (let [tree (make-node 2 
                         (make-leaf 1) 
                         (make-leaf 3))]
      (is (= (preorder-traversal tree) [2 1 3]))
      (is (= (inorder-traversal tree) [1 2 3]))
      (is (= (postorder-traversal tree) [1 3 2])))))

;; 二分探索木のテスト

(deftest test-binary-search-tree-creation
  (testing "二分探索木の作成"
    (let [bst (make-binary-search-tree)]
      (is (nil? (get-root bst))))))

(deftest test-bst-add
  (testing "二分探索木への追加"
    (let [bst (make-binary-search-tree)]
      (is (add-bst bst 1 "one"))
      (is (add-bst bst 3 "three"))
      (is (add-bst bst 2 "two"))
      (is (not (add-bst bst 2 "two")))))) ; 重複キーは追加されない

(deftest test-bst-search
  (testing "二分探索木での検索"
    (let [bst (make-binary-search-tree)]
      (add-bst bst 1 "one")
      (add-bst bst 3 "three")
      (add-bst bst 2 "two")
      (is (= (search-bst bst 2) "two"))
      (is (nil? (search-bst bst 4))))))

(deftest test-bst-min-max
  (testing "二分探索木の最小値・最大値"
    (let [bst (make-binary-search-tree)]
      (add-bst bst 5 "five")
      (add-bst bst 3 "three")
      (add-bst bst 7 "seven")
      (add-bst bst 1 "one")
      (add-bst bst 9 "nine")
      (is (= (bst-min bst) 1))
      (is (= (bst-max bst) 9)))))

(deftest test-bst-dump
  (testing "二分探索木の表示"
    (let [bst (make-binary-search-tree)
          output (atom [])]
      (add-bst bst 1 "one")
      (add-bst bst 3 "three")
      (add-bst bst 2 "two")
      (with-redefs [println (fn [& args] (swap! output conj (apply str args)))]
        (dump-bst bst))
      (is (= @output ["1  one" "2  two" "3  three"]))))) ; 昇順で出力されることを確認

(deftest test-bst-remove
  (testing "二分探索木からの削除"
    (let [bst (make-binary-search-tree)]
      (add-bst bst 5 "five")
      (add-bst bst 3 "three")
      (add-bst bst 7 "seven")
      (add-bst bst 1 "one")
      (add-bst bst 4 "four")
      (add-bst bst 6 "six")
      (add-bst bst 9 "nine")
      
      ;; 葉ノードの削除
      (is (remove-bst bst 1))
      (is (nil? (search-bst bst 1)))
      
      ;; 1つの子を持つノードの削除
      (is (remove-bst bst 6))
      (is (nil? (search-bst bst 6)))
      
      ;; 2つの子を持つノードの削除
      (is (remove-bst bst 3))
      (is (nil? (search-bst bst 3)))
      (is (= (search-bst bst 4) "four")) ; 残りのノードは正常
      
      ;; 存在しないキーの削除
      (is (not (remove-bst bst 10))))))

;; ヒープのテスト

(deftest test-heap-operations
  (testing "ヒープの操作"
    (let [heap (make-heap)]
      (is (heap-empty? heap))
      (is (= (heap-size heap) 0))
      
      ;; 要素の追加
      (heap-add heap 3)
      (heap-add heap 1)
      (heap-add heap 4)
      (heap-add heap 2)
      
      (is (not (heap-empty? heap)))
      (is (= (heap-size heap) 4))
      
      ;; 最小値の確認
      (is (= (heap-peek heap) 1))
      (is (= (heap-size heap) 4)) ; peekは削除しない
      
      ;; 最小値の取得と削除
      (is (= (heap-poll heap) 1))
      (is (= (heap-poll heap) 2))
      (is (= (heap-poll heap) 3))
      (is (= (heap-poll heap) 4))
      
      (is (heap-empty? heap)))))
