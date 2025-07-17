(ns fizz-buzz.application
  (:require [fizz-buzz.model :refer [create-fizz-buzz-value pipe]]
            [fizz-buzz.type :refer [create-type generate-string]]))

;; 関数型のCommandパターン: 関数を値として扱う
(defn create-fizz-buzz-processor
  "FizzBuzz処理関数を作成する高階関数"
  [type]
  (let [type-obj (create-type type)]
    (fn [number]
      (->> number
           create-fizz-buzz-value
           (generate-string type-obj)))))

;; 純粋関数: バッチ処理
(defn process-numbers
  "数値のシーケンスを一括処理する純粋関数"
  [numbers type]
  (let [processor (create-fizz-buzz-processor type)]
    (map processor numbers)))

;; 関数合成を使ったパイプライン処理
(defn create-processing-pipeline
  "処理パイプラインを作成する"
  [& processors]
  (fn [data]
    (reduce (fn [result processor] (processor result))
            data
            processors)))

;; 非同期処理のための関数（関数型アプローチ）
(defn process-numbers-async
  "数値を並列処理する（関数型アプローチ）"
  [numbers type]
  (let [processor (create-fizz-buzz-processor type)]
    (->> numbers
         (pmap processor)  ;; 並列map
         doall)))         ;; 遅延評価を強制実行

;; エラーハンドリングを含む安全な処理
(defn safe-process-number
  "エラーハンドリングを含む安全な数値処理"
  [number type]
  (try
    {:success true 
     :result ((create-fizz-buzz-processor type) number)}
    (catch Exception e
      {:success false 
       :error (.getMessage e)
       :input number})))

(defn safe-process-numbers
  "エラーハンドリングを含む安全なバッチ処理"
  [numbers type]
  (map #(safe-process-number % type) numbers))

;; 関数型のメモ化（パフォーマンス最適化）
(def memoized-fizz-buzz-processor
  "メモ化されたFizzBuzz処理関数"
  (memoize create-fizz-buzz-processor))

;; 条件付き処理（関数型）
(defn process-with-condition
  "条件を満たす数値のみ処理する"
  [numbers type condition-fn]
  (let [processor (create-fizz-buzz-processor type)]
    (->> numbers
         (filter condition-fn)
         (map processor))))

;; 統計情報を収集する処理
(defn process-with-stats
  "処理結果と統計情報を返す"
  [numbers type]
  (let [results (process-numbers numbers type)
        stats {:total-count (count numbers)
               :fizz-count (count (filter #(= % "Fizz") results))
               :buzz-count (count (filter #(= % "Buzz") results))
               :fizzbuzz-count (count (filter #(= % "FizzBuzz") results))
               :number-count (count (filter #(not (contains? #{"Fizz" "Buzz" "FizzBuzz"} %)) results))}]
    {:results results
     :stats stats}))

;; 遅延評価を活用した無限シーケンス処理
(defn infinite-fizz-buzz-seq
  "無限FizzBuzzシーケンスを生成"
  ([] (infinite-fizz-buzz-seq 1))
  ([type]
   (let [processor (create-fizz-buzz-processor type)]
     (->> (range 1 Long/MAX_VALUE)
          (map processor)))))

;; 従来のCommandパターン（互換性のため）
(defprotocol FizzBuzzCommand
  (execute [this]))

(defrecord FizzBuzzValueCommand [number type]
  FizzBuzzCommand
  (execute [this]
    ((create-fizz-buzz-processor type) number)))

(defrecord FizzBuzzListCommand [numbers type]
  FizzBuzzCommand
  (execute [this]
    (process-numbers numbers type)))

;; ファクトリーメソッド
(defn create-value-command [number type]
  (->FizzBuzzValueCommand number type))

(defn create-list-command [numbers type]
  (->FizzBuzzListCommand numbers type))

;; 関数型のコマンドファクトリ
(defn create-functional-command
  "関数型のコマンドを作成"
  [operation-type]
  (case operation-type
    :single-value (fn [number type] ((create-fizz-buzz-processor type) number))
    :batch (fn [numbers type] (process-numbers numbers type))
    :async (fn [numbers type] (process-numbers-async numbers type))
    :safe (fn [numbers type] (safe-process-numbers numbers type))
    :with-stats (fn [numbers type] (process-with-stats numbers type))
    (throw (IllegalArgumentException. (str "Unknown operation type: " operation-type)))))

;; 関数合成のデモンストレーション
(defn demo-function-composition
  "関数合成のデモ"
  [numbers]
  (let [process-pipeline (create-processing-pipeline
                           #(filter odd? %)
                           #(take 10 %)
                           #(process-numbers % 1))]
    (process-pipeline numbers)))
