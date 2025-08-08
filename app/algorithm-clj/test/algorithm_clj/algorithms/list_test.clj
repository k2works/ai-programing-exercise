(ns algorithm-clj.algorithms.list-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.algorithms.list :refer :all]))

;; Java LinkedList のテスト

(deftest test-java-linked-list
  (testing "Java LinkedList 初期化"
    (let [ll (make-java-linked-list)]
      (is (zero? (.size ll)))
      (is (.isEmpty ll)))))

(deftest test-java-linked-list-add
  (testing "Java LinkedList 要素追加"
    (let [ll (make-java-linked-list)]
      (add-first-java-linked-list ll 1)
      (is (= (.size ll) 1))
      (is (= (.getFirst ll) 1))
      (add-last-java-linked-list ll 2)
      (is (= (.size ll) 2))
      (is (= (.getLast ll) 2)))))

(deftest test-java-linked-list-remove
  (testing "Java LinkedList 要素削除"
    (let [ll (make-java-linked-list)]
      (add-first-java-linked-list ll 1)
      (add-last-java-linked-list ll 2)
      (is (= (remove-first-java-linked-list ll) 1))
      (is (= (.size ll) 1))
      (is (= (remove-last-java-linked-list ll) 2))
      (is (zero? (.size ll))))))

(deftest test-java-linked-list-search
  (testing "Java LinkedList 要素検索"
    (let [ll (make-java-linked-list)]
      (add-first-java-linked-list ll 1)
      (add-first-java-linked-list ll 2)
      (add-first-java-linked-list ll 3)
      ;; リストの状態: (3 2 1)
      (is (= (search-java-linked-list ll 1) 2))
      (is (= (search-java-linked-list ll 2) 1))
      (is (= (search-java-linked-list ll 3) 0))
      (is (= (search-java-linked-list ll 4) -1)))))

(deftest test-java-linked-list-bidirectional
  (testing "Java LinkedList 双方向アクセス"
    (let [ll (make-java-linked-list)]
      (add-first-java-linked-list ll 1)
      (add-last-java-linked-list ll 2)
      (add-first-java-linked-list ll 0)
      ;; リストの状態: (0 1 2)

      (is (= (.get ll 0) 0)) ; 先頭からアクセス
      (is (= (.get ll 1) 1))
      (is (= (.get ll 2) 2)) ; 末尾からアクセス

      ;; イテレータを使った双方向走査の確認
      (let [it (.listIterator ll (.size ll)) ; 末尾から開始するイテレータ
            elements-reverse (atom [])]
        (while (.hasPrevious it)
          (swap! elements-reverse conj (.previous it)))
        (is (= @elements-reverse [2 1 0]))))))

;; Clojure組み込みリストのテスト

(deftest test-clojure-list-demo
  (testing "Clojureリストの基本操作"
    (let [result (clojure-list-demo)]
      (is (= (:original result) '(1 2 3 4 5)))
      (is (= (:with-cons result) '(0 1 2 3 4 5)))
      (is (= (:first-element result) 1))
      (is (= (:rest-elements result) '(2 3 4 5)))
      (is (= (:count result) 5)))))

(deftest test-traverse-list
  (testing "リストの走査"
    (is (= (traverse-list '(1 2 3 4 5)) [1 2 3 4 5]))
    (is (= (traverse-list '()) []))
    (is (= (traverse-list '(42)) [42]))))
