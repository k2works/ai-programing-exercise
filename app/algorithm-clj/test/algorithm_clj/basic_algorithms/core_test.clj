(ns algorithm-clj.basic-algorithms.core-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.basic-algorithms.core :refer :all]))

;; 3値の最大値のテスト
(deftest test-max3
  (testing "max3 function"
    (is (= (max3 3 2 1) 3))  ; a > b > c
    (is (= (max3 3 2 2) 3))  ; a > b = c
    (is (= (max3 3 1 2) 3))  ; a > c > b
    (is (= (max3 3 2 3) 3))  ; a = c > b
    (is (= (max3 2 1 3) 3))  ; c > a > b
    (is (= (max3 3 3 2) 3))  ; a = b > c
    (is (= (max3 3 3 3) 3))  ; a = b = c
    (is (= (max3 2 2 3) 3))  ; c > a = b
    (is (= (max3 2 3 1) 3))  ; b > a > c
    (is (= (max3 2 3 2) 3))  ; b > a = c
    (is (= (max3 1 3 2) 3))  ; b > c > a
    (is (= (max3 2 3 3) 3))  ; b = c > a
    (is (= (max3 1 2 3) 3)))) ; c > b > a

;; 3値の中央値のテスト
(deftest test-med3
  (testing "med3 function"
    (is (= (med3 3 2 1) 2))  ; a > b > c
    (is (= (med3 3 2 2) 2))  ; a > b = c
    (is (= (med3 3 1 2) 2))  ; a > c > b
    (is (= (med3 3 2 3) 3))  ; a = c > b
    (is (= (med3 2 1 3) 2))  ; c > a > b
    (is (= (med3 3 3 2) 3))  ; a = b > c
    (is (= (med3 3 3 3) 3))  ; a = b = c
    (is (= (med3 2 2 3) 2))  ; c > a = b
    (is (= (med3 2 3 1) 2))  ; b > a > c
    (is (= (med3 2 3 2) 2))  ; b > a = c
    (is (= (med3 1 3 2) 2))  ; b > c > a
    (is (= (med3 2 3 3) 3))  ; b = c > a
    (is (= (med3 1 2 3) 2)))) ; c > b > a

;; 符号判定のテスト
(deftest test-judge-sign
  (testing "judge-sign function"
    (is (= (judge-sign 17) "その値は正です。"))
    (is (= (judge-sign -5) "その値は負です。"))
    (is (= (judge-sign 0) "その値は0です。"))))

;; 1からnまでの総和のテスト
(deftest test-sum-1-to-n
  (testing "sum-1-to-n-while function"
    (is (= (sum-1-to-n-while 5) 15))
    (is (= (sum-1-to-n-while 10) 55))
    (is (= (sum-1-to-n-while 1) 1)))
  (testing "sum-1-to-n-for function"
    (is (= (sum-1-to-n-for 5) 15))
    (is (= (sum-1-to-n-for 10) 55))
    (is (= (sum-1-to-n-for 1) 1))))

;; 記号文字の交互表示のテスト
(deftest test-alternative
  (testing "alternative-1 function"
    (is (= (alternative-1 12) "+-+-+-+-+-+-"))
    (is (= (alternative-1 5) "+-+-+"))
    (is (= (alternative-1 1) "+")))
  (testing "alternative-2 function"
    (is (= (alternative-2 12) "+-+-+-+-+-+-"))
    (is (= (alternative-2 5) "+-+-+"))
    (is (= (alternative-2 1) "+"))))

;; 長方形の辺の長さを列挙のテスト
(deftest test-rectangle
  (testing "rectangle function"
    (is (= (rectangle 32) "1x32 2x16 4x8 "))
    (is (= (rectangle 12) "1x12 2x6 3x4 "))
    (is (= (rectangle 1) "1x1 "))))

;; 九九の表のテスト
(deftest test-multiplication-table
  (testing "multiplication-table function"
    (let [expected (str (apply str (repeat 27 "-")) "\n"
                        "  1  2  3  4  5  6  7  8  9\n"
                        "  2  4  6  8 10 12 14 16 18\n"
                        "  3  6  9 12 15 18 21 24 27\n"
                        "  4  8 12 16 20 24 28 32 36\n"
                        "  5 10 15 20 25 30 35 40 45\n"
                        "  6 12 18 24 30 36 42 48 54\n"
                        "  7 14 21 28 35 42 49 56 63\n"
                        "  8 16 24 32 40 48 56 64 72\n"
                        "  9 18 27 36 45 54 63 72 81\n"
                        (apply str (repeat 27 "-")))]
      (is (= (multiplication-table) expected)))))

;; 直角三角形の表示のテスト
(deftest test-triangle-lb
  (testing "triangle-lb function"
    (let [expected (str "*\n"
                        "**\n"
                        "***\n"
                        "****\n"
                        "*****\n")]
      (is (= (triangle-lb 5) expected)))))

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
