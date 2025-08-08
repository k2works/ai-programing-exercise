(ns algorithm-clj.basic-algorithms.tree-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.basic-algorithms.tree :refer :all]))

;; 基本的な木のテスト

(deftest test-tree-creation
  (testing "木の作成"
    (let [leaf (make-leaf 42)]
      (is (= (:value leaf) 42))
      (is (nil? (:left leaf)))
      (is (nil? (:right leaf)))
      (is (leaf? leaf)))
    
    (let [left-child (make-leaf 1)
          right-child (make-leaf 3)
          root (make-node 2 left-child right-child)]
      (is (= (:value root) 2))
      (is (= (:value (:left root)) 1))
      (is (= (:value (:right root)) 3))
      (is (not (leaf? root))))))

(deftest test-tree-size
  (testing "木のサイズ計算"
    (let [leaf (make-leaf 42)]
      (is (= (tree-size leaf) 1)))
    
    (let [tree (make-node 2 
                         (make-leaf 1) 
                         (make-leaf 3))]
      (is (= (tree-size tree) 3)))))

(deftest test-tree-height
  (testing "木の高さ計算"
    (let [leaf (make-leaf 42)]
      (is (= (tree-height leaf) 1)))
    
    (let [tree (make-node 2 
                         (make-leaf 1) 
                         (make-leaf 3))]
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

(deftest test-bst-insert-and-search
  (testing "BST への挿入と検索"
    (let [bst (-> nil
                  (insert-bst 5)
                  (insert-bst 3)
                  (insert-bst 7)
                  (insert-bst 2)
                  (insert-bst 4))]
      (is (search-bst bst 5))
      (is (search-bst bst 3))
      (is (search-bst bst 7))
      (is (search-bst bst 2))
      (is (search-bst bst 4))
      (is (not (search-bst bst 1)))
      (is (not (search-bst bst 6))))))

(deftest test-bst-inorder
  (testing "BST の中順走査（ソート順）"
    (let [bst (-> nil
                  (insert-bst 5)
                  (insert-bst 3)
                  (insert-bst 7)
                  (insert-bst 2)
                  (insert-bst 4))]
      (is (= (inorder-traversal bst) [2 3 4 5 7])))))

(deftest test-bst-remove
  (testing "BST からの削除"
    ;; 葉ノードの削除
    (let [bst (-> nil
                  (insert-bst 5)
                  (insert-bst 3)
                  (remove-bst 3))]
      (is (not (search-bst bst 3)))
      (is (search-bst bst 5)))
    
    ;; 子が一つのノードの削除
    (let [bst (-> nil
                  (insert-bst 5)
                  (insert-bst 3)
                  (insert-bst 2)
                  (remove-bst 3))]
      (is (not (search-bst bst 3)))
      (is (search-bst bst 5))
      (is (search-bst bst 2)))
    
    ;; 子が二つのノードの削除
    (let [bst (-> nil
                  (insert-bst 5)
                  (insert-bst 3)
                  (insert-bst 7)
                  (insert-bst 2)
                  (insert-bst 4)
                  (insert-bst 6)
                  (insert-bst 8)
                  (remove-bst 5))]
      (is (not (search-bst bst 5)))
      (is (search-bst bst 3))
      (is (search-bst bst 7))
      (is (search-bst bst 2))
      (is (search-bst bst 4))
      (is (search-bst bst 6))
      (is (search-bst bst 8)))))

;; ヒープのテスト

(deftest test-heap-operations
  (testing "ヒープの基本操作"
    (let [heap (make-heap)]
      (is (heap-empty? heap))
      (is (= (heap-size heap) 0))
      
      (heap-add heap 5)
      (heap-add heap 3)
      (heap-add heap 7)
      (heap-add heap 1)
      
      (is (not (heap-empty? heap)))
      (is (= (heap-size heap) 4))
      (is (= (heap-peek heap) 1)) ; 最小値
      
      (is (= (heap-poll heap) 1))
      (is (= (heap-poll heap) 3))
      (is (= (heap-poll heap) 5))
      (is (= (heap-poll heap) 7))
      
      (is (heap-empty? heap)))))

(deftest test-heap-with-comparator
  (testing "カスタムコンパレータ付きヒープ"
    (let [max-heap (make-heap #(compare %2 %1))] ; 逆順（最大ヒープ）
      (heap-add max-heap 5)
      (heap-add max-heap 3)
      (heap-add max-heap 7)
      (heap-add max-heap 1)
      
      (is (= (heap-peek max-heap) 7)) ; 最大値
      
      (is (= (heap-poll max-heap) 7))
      (is (= (heap-poll max-heap) 5))
      (is (= (heap-poll max-heap) 3))
      (is (= (heap-poll max-heap) 1))
      
      (is (heap-empty? max-heap)))))

(deftest test-find-min
  (testing "BST の最小値検索"
    (let [bst (-> nil
                  (insert-bst 5)
                  (insert-bst 3)
                  (insert-bst 7)
                  (insert-bst 2)
                  (insert-bst 4))]
      (is (= (find-min bst) 2)))))

(deftest test-complex-tree-operations
  (testing "複雑な木操作"
    (let [complex-tree (make-node 1
                                 (make-node 2
                                           (make-leaf 4)
                                           (make-leaf 5))
                                 (make-node 3
                                           (make-leaf 6)
                                           (make-leaf 7)))]
      (is (= (tree-size complex-tree) 7))
      (is (= (tree-height complex-tree) 3))
      (is (= (preorder-traversal complex-tree) [1 2 4 5 3 6 7]))
      (is (= (inorder-traversal complex-tree) [4 2 5 1 6 3 7]))
      (is (= (postorder-traversal complex-tree) [4 5 2 6 7 3 1])))))
