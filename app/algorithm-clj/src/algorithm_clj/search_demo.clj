(ns algorithm-clj.search-demo
  (:require [algorithm-clj.basic-algorithms.search :as search]))

(defn demonstrate-linear-search []
  (println "【線形探索のデモ】")
  (let [data [6 4 3 2 1 2 8]
        key 2]
    (println "データ:" data)
    (println "探索キー:" key)
    (println "  ssearch-while 結果:" (search/ssearch-while data key))
    (println "  ssearch-for 結果:" (search/ssearch-for data key))
    (println "  ssearch-sentinel 結果:" (search/ssearch-sentinel data key))
    (println "  存在しないキー(9):" (search/ssearch-while data 9)))
  (println))

(defn demonstrate-binary-search []
  (println "【二分探索のデモ】")
  (let [sorted-data [1 2 3 5 7 8 9]
        key 5]
    (println "ソート済みデータ:" sorted-data)
    (println "探索キー:" key)
    (println "  bsearch 結果:" (search/bsearch sorted-data key))
    (println "  存在しないキー(4):" (search/bsearch sorted-data 4))
    (println "  存在しないキー(10):" (search/bsearch sorted-data 10)))
  (println))

(defn demonstrate-chained-hash []
  (println "【チェイン法ハッシュテーブルのデモ】")
  (let [ch (search/make-chained-hash 13)]
    (println "ハッシュテーブル（容量: 13）を作成")
    
    ;; データの追加
    (search/add-chained-hash ch 1 "赤尾")
    (search/add-chained-hash ch 5 "武田")
    (search/add-chained-hash ch 10 "小野")
    (search/add-chained-hash ch 12 "鈴木")
    (search/add-chained-hash ch 14 "神崎")
    (search/add-chained-hash ch 27 "田中") ; 14と同じハッシュ値になる（14 % 13 = 1, 27 % 13 = 1）
    
    (println "データを追加しました: {1: 赤尾, 5: 武田, 10: 小野, 12: 鈴木, 14: 神崎, 27: 田中}")
    
    ;; 探索
    (println "探索結果:")
    (println "  key=1:" (search/search-chained-hash ch 1))
    (println "  key=14:" (search/search-chained-hash ch 14))
    (println "  key=27:" (search/search-chained-hash ch 27))
    (println "  key=100 (存在しない):" (search/search-chained-hash ch 100))
    
    ;; 削除
    (println "key=14を削除:" (search/remove-chained-hash ch 14))
    (println "削除後のkey=14:" (search/search-chained-hash ch 14))
    (println "削除後のkey=27:" (search/search-chained-hash ch 27)))
  (println))

(defn demonstrate-open-hash []
  (println "【オープンアドレス法ハッシュテーブルのデモ】")
  (let [oh (search/make-open-hash 13)]
    (println "ハッシュテーブル（容量: 13）を作成")
    
    ;; データの追加
    (search/add-open-hash oh 1 "赤尾")
    (search/add-open-hash oh 5 "武田")
    (search/add-open-hash oh 10 "小野")
    (search/add-open-hash oh 12 "鈴木")
    (search/add-open-hash oh 14 "神崎")
    (search/add-open-hash oh 27 "田中") ; 14と同じハッシュ値になるが、別の位置に格納される
    
    (println "データを追加しました: {1: 赤尾, 5: 武田, 10: 小野, 12: 鈴木, 14: 神崎, 27: 田中}")
    
    ;; 探索
    (println "探索結果:")
    (println "  key=1:" (search/search-open-hash oh 1))
    (println "  key=14:" (search/search-open-hash oh 14))
    (println "  key=27:" (search/search-open-hash oh 27))
    (println "  key=100 (存在しない):" (search/search-open-hash oh 100))
    
    ;; 削除
    (println "key=14を削除:" (search/remove-open-hash oh 14))
    (println "削除後のkey=14:" (search/search-open-hash oh 14))
    (println "削除後のkey=27:" (search/search-open-hash oh 27)))
  (println))

(defn demonstrate-search-comparison []
  (println "【探索アルゴリズムの比較】")
  (let [large-data (vec (range 0 10000 2)) ; 0, 2, 4, ..., 9998
        key 5000]
    (println "大きなデータセット（5000要素）での探索比較")
    (println "データ: [0, 2, 4, 6, ..., 9998]")
    (println "探索キー:" key)
    
    ;; 線形探索
    (let [start-time (System/nanoTime)
          result (search/ssearch-while large-data key)
          end-time (System/nanoTime)
          elapsed (/ (- end-time start-time) 1000000.0)]
      (println (format "  線形探索: インデックス=%d, 時間=%.2fms" result elapsed)))
    
    ;; 二分探索
    (let [start-time (System/nanoTime)
          result (search/bsearch large-data key)
          end-time (System/nanoTime)
          elapsed (/ (- end-time start-time) 1000000.0)]
      (println (format "  二分探索: インデックス=%d, 時間=%.2fms" result elapsed))))
  (println))

(defn -main []
  (println "=== アルゴリズムから始めるClojure入門 第3章 探索アルゴリズム デモ ===\n")
  
  (demonstrate-linear-search)
  (demonstrate-binary-search)
  (demonstrate-chained-hash)
  (demonstrate-open-hash)
  (demonstrate-search-comparison))
