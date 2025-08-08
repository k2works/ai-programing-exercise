(ns algorithm-clj.demos.stack-queue-demo
  "第4章 スタックとキューのデモプログラム"
  (:require [algorithm-clj.algorithms.stack-queue :refer :all])
  (:import [java.util ArrayDeque]))

(defn -main []
  (println "=== アルゴリズムから始めるClojure入門 第4章 スタックとキュー デモ ===")
  (println)
  
  ;; 固定長スタックのデモ
  (println "【固定長スタックのデモ】")
  (let [stack (make-fixed-stack 5)]
    (println "スタック（容量: 5）を作成")
    
    ;; データをプッシュ
    (doseq [x [10 20 30 40]]
      (push-fixed-stack stack x)
      (println (str "  プッシュ: " x " -> " (dump-fixed-stack stack))))
    
    ;; ピーク操作
    (println (str "  ピーク: " (peek-fixed-stack stack)))
    
    ;; ポップ操作
    (dotimes [_ 3]
      (let [value (pop-fixed-stack stack)]
        (println (str "  ポップ: " value " -> " (dump-fixed-stack stack)))))
    
    ;; 検索とカウント
    (push-fixed-stack stack 20)
    (push-fixed-stack stack 10)
    (println (str "  現在のスタック: " (dump-fixed-stack stack)))
    (println (str "  値 10 の検索: インデックス " (find-fixed-stack stack 10)))
    (println (str "  値 20 の個数: " (count-fixed-stack-elements stack 20) "個"))
    (println (str "  スタック内の総要素数: " (count-fixed-stack stack) "個"))

    ;; エラーハンドリング
    (try
      (dotimes [_ 10] (push-fixed-stack stack 99))
      (catch Exception e (println (str "  満杯エラー: " (.getMessage e)))))

    (try
      (clear-fixed-stack stack)
      (pop-fixed-stack stack)
      (catch Exception e (println (str "  空エラー: " (.getMessage e))))))

  (println)
  
  ;; ArrayDeque スタックのデモ
  (println "【ArrayDeque スタックのデモ】")
  (let [stack (make-stack-deque)]
    (println "ArrayDeque スタックを作成")
    
    ;; データをプッシュ
    (doseq [x ["A" "B" "C"]]
      (push-deque stack x)
      (println (str "  プッシュ: " x " (サイズ: " (count-deque-elements stack) ")")))
    
    ;; ピーク操作
    (println (str "  ピーク: " (peek-deque stack)))
    
    ;; ポップ操作
    (while (not (is-empty-deque stack))
      (let [value (pop-deque stack)]
        (println (str "  ポップ: " value " (残り: " (count-deque-elements stack) ")"))))

    ;; ArrayDequeのクリア機能をテスト
    (push-deque stack "Test1")
    (push-deque stack "Test2")
    (println (str "  テストデータ追加後のサイズ: " (count-deque-elements stack)))
    (clear-deque stack)
    (println (str "  クリア後のサイズ: " (count-deque-elements stack))))

  (println)
  
  ;; 固定長キューのデモ
  (println "【固定長キューのデモ】")
  (let [queue (make-fixed-queue 5)]
    (println "キュー（容量: 5）を作成")
    
    ;; データをエンキュー
    (doseq [x [100 200 300 400]]
      (enque queue x)
      (println (str "  エンキュー: " x " -> " (dump-queue queue))))
    
    ;; ピーク操作
    (println (str "  ピーク: " (peek-queue queue)))
    
    ;; デキュー操作
    (dotimes [_ 2]
      (let [value (deque queue)]
        (println (str "  デキュー: " value " -> " (dump-queue queue)))))
    
    ;; 新しいデータを追加（リングバッファ動作）
    (doseq [x [500 600]]
      (enque queue x)
      (println (str "  エンキュー: " x " -> " (dump-queue queue))))
    
    ;; 検索とカウント
    (println (str "  現在のキュー: " (dump-queue queue)))
    (println (str "  値 300 の検索: 位置 " (find-queue queue 300)))
    (println (str "  値 400 の個数: " (count-queue queue 400) "個"))
    (println (str "  総要素数: " (count-queue-elements queue) "個"))

    ;; リングバッファの動作確認
    (println "【リングバッファ動作確認】")
    (let [ring-queue (make-fixed-queue 3)]
      (doseq [x [1 2 3]]
        (enque ring-queue x))
      (println (str "  満杯状態: " (dump-queue ring-queue)))
      
      (deque ring-queue)
      (enque ring-queue 4)
      (println (str "  デキューして4をエンキュー: " (dump-queue ring-queue)))
      
      (deque ring-queue)
      (enque ring-queue 5)
      (println (str "  デキューして5をエンキュー: " (dump-queue ring-queue)))

      ;; キューのクリア
      (clear-queue ring-queue)
      (println (str "  クリア後: " (dump-queue ring-queue)))))

  (println)
  
  ;; ArrayDeque キューのデモ
  (println "【ArrayDeque キューのデモ】")
  (let [queue (make-queue-deque)]
    (println "ArrayDeque キューを作成")
    
    ;; データをエンキュー
    (doseq [x ["First" "Second" "Third"]]
      (offer-deque queue x)
      (println (str "  エンキュー: " x " (サイズ: " (count-deque-elements queue) ")")))
    
    ;; ピーク操作
    (println (str "  ピーク: " (peek-queue-deque queue)))
    
    ;; デキュー操作（FIFO）
    (while (not (is-empty-deque queue))
      (let [value (poll-deque queue)]
        (println (str "  デキュー: " value " (残り: " (count-deque-elements queue) ")")))))
  
  (println)
  
  ;; スタックとキューの動作比較
  (println "【スタックとキューの動作比較】")
  (let [stack (make-fixed-stack 5)
        queue (make-fixed-queue 5)]
    
    (println "同じデータ [10, 20, 30] を追加:")
    (doseq [x [10 20 30]]
      (push-fixed-stack stack x)
      (enque queue x))
    
    (println (str "  スタック状態: " (dump-fixed-stack stack)))
    (println (str "  キュー状態: " (dump-queue queue)))
    
    (println "取り出し順序の違い:")
    (print "  スタック (LIFO): ")
    (while (not (is-empty-fixed-stack stack))
      (print (pop-fixed-stack stack))
      (when (not (is-empty-fixed-stack stack))
        (print " -> ")))
    (println)
    
    (print "  キュー (FIFO): ")
    (while (not (is-empty-queue queue))
      (print (deque queue))
      (when (not (is-empty-queue queue))
        (print " -> ")))
    (println))
  
  (println)
  
  ;; 実用例：括弧の対応チェック（スタック使用）
  (println "【実用例: 括弧の対応チェック（スタック使用）】")
  (letfn [(check-brackets [s]
            (let [stack (make-stack-deque)
                  open-brackets #{\( \[ \{}
                  close-brackets #{\) \] \}}
                  pairs {\) \(, \] \[, \} \{}]
              (loop [chars (seq s)]
                (cond
                  (empty? chars) (is-empty-deque stack)
                  
                  (open-brackets (first chars))
                  (do
                    (push-deque stack (first chars))
                    (recur (rest chars)))
                  
                  (close-brackets (first chars))
                  (if (and (not (is-empty-deque stack))
                           (= (pop-deque stack) (pairs (first chars))))
                    (recur (rest chars))
                    false)
                  
                  :else (recur (rest chars))))))]
    
    (let [test-cases ["(())" "([{}])" "(((" ")))" "([)]" "abc(def)ghi"]]
      (doseq [test test-cases]
        (println (str "  \"" test "\" -> " 
                     (if (check-brackets test) "正しい" "不正"))))))
  
  (println)
  (println "第4章のデモが完了しました！"))
