(ns fizz-buzz.core
  (:require [fizz-buzz.model :refer [create-fizz-buzz-value]]
            [fizz-buzz.type :refer [create-type generate-string]]
            [fizz-buzz.application :refer [create-value-command execute]])
  (:gen-class))

;; 純粋関数: 数値からFizzBuzz文字列への変換
(defn number->fizz-buzz-string
  "数値をFizzBuzz文字列に変換する純粋関数"
  ([number] (number->fizz-buzz-string number 1))
  ([number type]
   (->> number
        create-fizz-buzz-value
        (generate-string (create-type type)))))

;; 純粋関数: 範囲からFizzBuzzシーケンスを生成
(defn numbers->fizz-buzz-seq
  "数値の範囲をFizzBuzzシーケンスに変換する純粋関数"
  ([max-number] (numbers->fizz-buzz-seq max-number 1))
  ([max-number type]
   (->> (range 1 (inc max-number))
        (map #(number->fizz-buzz-string % type)))))

;; 高階関数: フィルタリング機能
(defn fizz-buzz-filter
  "指定した条件でFizzBuzzシーケンスをフィルタリング"
  [pred fizz-buzz-seq]
  (filter pred fizz-buzz-seq))

;; 高階関数: 変換機能
(defn fizz-buzz-transform
  "FizzBuzzシーケンスに変換関数を適用"
  [transform-fn fizz-buzz-seq]
  (map transform-fn fizz-buzz-seq))

;; 関数合成: 複数の操作をパイプライン化
(defn fizz-buzz-pipeline
  "FizzBuzz生成パイプライン"
  [max-number type & transforms]
  (reduce (fn [seq transform-fn] (transform-fn seq))
          (numbers->fizz-buzz-seq max-number type)
          transforms))

;; 既存APIとの互換性のための関数
(defn generate 
  ([number] (number->fizz-buzz-string number))
  ([number type] (number->fizz-buzz-string number type)))

(defn generate-list
  ([max-number] (numbers->fizz-buzz-seq max-number))
  ([max-number type] (numbers->fizz-buzz-seq max-number type)))

;; FizzBuzzレコードとファクトリー（互換性のため）
(defrecord FizzBuzz [number type-obj])

(defn create-fizz-buzz [number type]
  (->FizzBuzz number (create-type type)))

(defn fizz-buzz-generate [fb]
  (let [{:keys [number type-obj]} fb
        fizz-buzz-value (create-fizz-buzz-value number)]
    (generate-string type-obj fizz-buzz-value)))

;; 副作用: 出力処理
(defn print-fizz-buzz-seq
  "FizzBuzzシーケンスを出力する（副作用のある関数）"
  [fizz-buzz-seq]
  (doseq [result fizz-buzz-seq]
    (println result)))

;; 副作用: タイトル付き出力
(defn print-fizz-buzz-with-title
  "タイトル付きでFizzBuzzを出力する"
  ([fizz-buzz-seq] (print-fizz-buzz-with-title "FizzBuzz:" fizz-buzz-seq))
  ([title fizz-buzz-seq]
   (println title)
   (print-fizz-buzz-seq fizz-buzz-seq)))

;; アプリケーションのメイン関数
(defn run-fizz-buzz-app
  "FizzBuzzアプリケーションを実行する"
  ([] (run-fizz-buzz-app 100 1))
  ([max-number] (run-fizz-buzz-app max-number 1))
  ([max-number type]
   (->> (numbers->fizz-buzz-seq max-number type)
        (print-fizz-buzz-with-title))))

(defn -main
  "FizzBuzz application main entry point."
  [& args]
  (run-fizz-buzz-app))

;; 関数型ユーティリティ
(defn fizz-only
  "Fizzのみを抽出するフィルタ"
  [fizz-buzz-seq]
  (fizz-buzz-filter #(= % "Fizz") fizz-buzz-seq))

(defn buzz-only  
  "Buzzのみを抽出するフィルタ"
  [fizz-buzz-seq]
  (fizz-buzz-filter #(= % "Buzz") fizz-buzz-seq))

(defn fizz-buzz-only
  "FizzBuzzのみを抽出するフィルタ"
  [fizz-buzz-seq]
  (fizz-buzz-filter #(= % "FizzBuzz") fizz-buzz-seq))

(defn add-index
  "インデックスを追加する変換"
  [fizz-buzz-seq]
  (fizz-buzz-transform 
    (fn [item] (str (inc (.indexOf (vec fizz-buzz-seq) item)) ": " item))
    fizz-buzz-seq))

;; 使用例を示すデモ関数
(defn demo-functional-features
  "関数型機能のデモンストレーション"
  []
  (let [basic-seq (numbers->fizz-buzz-seq 15)]
    (println "\n=== 基本的なFizzBuzz ===")
    (print-fizz-buzz-seq basic-seq)
    
    (println "\n=== Fizzのみ ===")
    (->> basic-seq fizz-only print-fizz-buzz-seq)
    
    (println "\n=== パイプライン処理（Fizz, Buzz, FizzBuzzのみ）===")
    (->> (numbers->fizz-buzz-seq 50)
         (fizz-buzz-filter #(not (re-matches #"\d+" %)))
         (take 10)
         print-fizz-buzz-seq)))

;; 高階関数の例
(defn create-custom-filter
  "カスタムフィルタを作成する高階関数"
  [pattern]
  (fn [fizz-buzz-seq]
    (fizz-buzz-filter #(.contains % pattern) fizz-buzz-seq)))

(defn create-custom-transform
  "カスタム変換を作成する高階関数"
  [prefix]
  (fn [fizz-buzz-seq]
    (fizz-buzz-transform #(str prefix %) fizz-buzz-seq)))