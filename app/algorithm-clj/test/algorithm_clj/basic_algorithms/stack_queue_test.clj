(ns algorithm-clj.basic-algorithms.stack-queue-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.basic-algorithms.stack-queue :refer :all])
  (:import [java.util ArrayDeque]))

;; ============================
;; 固定長スタック (FixedStack) のテスト
;; ============================

(deftest test-fixed-stack-basic-operations
  (testing "固定長スタックの基本操作"
    (let [s (make-fixed-stack 5)]
      
      ;; 初期状態のテスト
      (is (is-empty-fixed-stack s))
      (is (not (is-full-fixed-stack s)))
      (is (= 0 (count-fixed-stack-elements s)))
      (is (= [] (dump-fixed-stack s)))
      
      ;; プッシュ操作のテスト
      (push-fixed-stack s 1)
      (is (= 1 (count-fixed-stack-elements s)))
      (is (= [1] (dump-fixed-stack s)))
      
      (push-fixed-stack s 2)
      (push-fixed-stack s 3)
      (is (= 3 (count-fixed-stack-elements s)))
      (is (= [1 2 3] (dump-fixed-stack s)))
      
      ;; ピーク操作のテスト
      (is (= 3 (peek-fixed-stack s)))
      (is (= 3 (count-fixed-stack-elements s))) ; ピークでは要素数は変わらない
      
      ;; ポップ操作のテスト
      (is (= 3 (pop-fixed-stack s)))
      (is (= 2 (count-fixed-stack-elements s)))
      (is (= [1 2] (dump-fixed-stack s)))
      
      (is (= 2 (pop-fixed-stack s)))
      (is (= 1 (pop-fixed-stack s)))
      (is (is-empty-fixed-stack s)))))

(deftest test-fixed-stack-search-operations
  (testing "固定長スタックの検索・カウント操作"
    (let [s (make-fixed-stack 10)]
      (push-fixed-stack s 1)
      (push-fixed-stack s 2)
      (push-fixed-stack s 1)
      (push-fixed-stack s 3)
      (push-fixed-stack s 1)
      
      ;; 検索のテスト
      (is (= 4 (find-fixed-stack s 1))) ; 最後にプッシュされた1のインデックス
      (is (= 3 (find-fixed-stack s 3)))
      (is (= 1 (find-fixed-stack s 2)))
      (is (= -1 (find-fixed-stack s 9))) ; 存在しない値
      
      ;; カウントのテスト
      (is (= 3 (count-fixed-stack s 1)))
      (is (= 1 (count-fixed-stack s 2)))
      (is (= 1 (count-fixed-stack s 3)))
      (is (= 0 (count-fixed-stack s 9))))))

(deftest test-fixed-stack-edge-cases
  (testing "固定長スタックのエッジケース"
    (let [s (make-fixed-stack 2)]
      
      ;; 空のスタックでのエラー
      (is (thrown? Exception (pop-fixed-stack s)))
      (is (thrown? Exception (peek-fixed-stack s)))
      
      ;; 満杯のスタックでのエラー
      (push-fixed-stack s 1)
      (push-fixed-stack s 2)
      (is (is-full-fixed-stack s))
      (is (thrown? Exception (push-fixed-stack s 3)))
      
      ;; クリア操作
      (clear-fixed-stack s)
      (is (is-empty-fixed-stack s))
      (is (= 0 (count-fixed-stack-elements s))))))

;; ============================
;; ArrayDeque スタックのテスト
;; ============================

(deftest test-deque-stack-operations
  (testing "ArrayDeque スタックの基本操作"
    (let [s (make-stack-deque)]
      
      ;; 初期状態
      (is (is-empty-deque s))
      (is (= 0 (count-deque-elements s)))
      
      ;; プッシュ・ポップ操作
      (push-deque s 1)
      (push-deque s 2)
      (push-deque s 3)
      (is (= 3 (count-deque-elements s)))
      
      ;; ピーク操作
      (is (= 3 (peek-deque s)))
      (is (= 3 (count-deque-elements s)))
      
      ;; ポップ操作
      (is (= 3 (pop-deque s)))
      (is (= 2 (pop-deque s)))
      (is (= 1 (pop-deque s)))
      (is (is-empty-deque s))
      
      ;; クリア操作
      (push-deque s 10)
      (clear-deque s)
      (is (is-empty-deque s)))))

;; ============================
;; 固定長キュー (FixedQueue) のテスト
;; ============================

(deftest test-fixed-queue-basic-operations
  (testing "固定長キューの基本操作"
    (let [q (make-fixed-queue 5)]
      
      ;; 初期状態のテスト
      (is (is-empty-queue q))
      (is (not (is-full-queue q)))
      (is (= 0 (count-queue-elements q)))
      (is (= [] (dump-queue q)))
      
      ;; エンキュー操作のテスト
      (enque q 1)
      (is (= 1 (count-queue-elements q)))
      (is (= [1] (dump-queue q)))
      
      (enque q 2)
      (enque q 3)
      (is (= 3 (count-queue-elements q)))
      (is (= [1 2 3] (dump-queue q)))
      
      ;; ピーク操作のテスト
      (is (= 1 (peek-queue q)))
      (is (= 3 (count-queue-elements q))) ; ピークでは要素数は変わらない
      
      ;; デキュー操作のテスト
      (is (= 1 (deque q)))
      (is (= 2 (count-queue-elements q)))
      (is (= [2 3] (dump-queue q)))
      
      (is (= 2 (deque q)))
      (is (= 3 (deque q)))
      (is (is-empty-queue q)))))

(deftest test-fixed-queue-ring-buffer
  (testing "固定長キューのリングバッファ動作"
    (let [q (make-fixed-queue 3)]
      
      ;; キューを満杯にする
      (enque q 1)
      (enque q 2)
      (enque q 3)
      (is (is-full-queue q))
      
      ;; 一つデキューして、新しい要素をエンキュー
      (is (= 1 (deque q)))
      (enque q 4)
      (is (= [2 3 4] (dump-queue q)))
      
      ;; さらにデキューとエンキューを繰り返す
      (is (= 2 (deque q)))
      (enque q 5)
      (is (= [3 4 5] (dump-queue q)))
      
      (is (= 3 (deque q)))
      (enque q 6)
      (is (= [4 5 6] (dump-queue q))))))

(deftest test-fixed-queue-search-operations
  (testing "固定長キューの検索・カウント操作"
    (let [q (make-fixed-queue 10)]
      (enque q 1)
      (enque q 2)
      (enque q 1)
      (enque q 3)
      (enque q 1)
      
      ;; 検索のテスト
      (is (= 0 (find-queue q 1))) ; 最初の1の論理的位置
      (is (= 1 (find-queue q 2)))
      (is (= 3 (find-queue q 3)))
      (is (= -1 (find-queue q 9))) ; 存在しない値
      
      ;; カウントのテスト
      (is (= 3 (count-queue q 1)))
      (is (= 1 (count-queue q 2)))
      (is (= 1 (count-queue q 3)))
      (is (= 0 (count-queue q 9))))))

(deftest test-fixed-queue-edge-cases
  (testing "固定長キューのエッジケース"
    (let [q (make-fixed-queue 2)]
      
      ;; 空のキューでのエラー
      (is (thrown? Exception (deque q)))
      (is (thrown? Exception (peek-queue q)))
      
      ;; 満杯のキューでのエラー
      (enque q 1)
      (enque q 2)
      (is (is-full-queue q))
      (is (thrown? Exception (enque q 3)))
      
      ;; クリア操作
      (clear-queue q)
      (is (is-empty-queue q))
      (is (= 0 (count-queue-elements q))))))

;; ============================
;; ArrayDeque キューのテスト
;; ============================

(deftest test-deque-queue-operations
  (testing "ArrayDeque キューの基本操作"
    (let [q (make-queue-deque)]
      
      ;; 初期状態
      (is (is-empty-deque q))
      (is (= 0 (count-deque-elements q)))
      
      ;; エンキュー・デキュー操作
      (offer-deque q 1)
      (offer-deque q 2)
      (offer-deque q 3)
      (is (= 3 (count-deque-elements q)))
      
      ;; ピーク操作
      (is (= 1 (peek-queue-deque q)))
      (is (= 3 (count-deque-elements q)))
      
      ;; デキュー操作（FIFO）
      (is (= 1 (poll-deque q)))
      (is (= 2 (poll-deque q)))
      (is (= 3 (poll-deque q)))
      (is (is-empty-deque q))
      
      ;; クリア操作
      (offer-deque q 10)
      (clear-deque q)
      (is (is-empty-deque q)))))

(deftest test-stack-vs-queue-behavior
  (testing "スタックとキューの動作の違い"
    (let [stack (make-fixed-stack 5)
          queue (make-fixed-queue 5)]
      
      ;; 同じデータを追加
      (doseq [x [1 2 3]]
        (push-fixed-stack stack x)
        (enque queue x))
      
      ;; スタック: LIFO (3, 2, 1の順序)
      (is (= 3 (pop-fixed-stack stack)))
      (is (= 2 (pop-fixed-stack stack)))
      (is (= 1 (pop-fixed-stack stack)))
      
      ;; キュー: FIFO (1, 2, 3の順序)
      (is (= 1 (deque queue)))
      (is (= 2 (deque queue)))
      (is (= 3 (deque queue))))))
