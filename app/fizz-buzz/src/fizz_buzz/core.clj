(ns fizz-buzz.core
  (:require [fizz-buzz.model :refer [create-fizz-buzz-value]]
            [fizz-buzz.type :refer [create-type generate-string]]
            [fizz-buzz.application :refer [create-value-command execute]])
  (:gen-class))

; 既存のAPIとの互換性のための関数
(defn generate 
  ([number] (generate number 1))
  ([number type]
   (let [command (create-value-command number type)]
     (execute command))))

; FizzBuzzレコードとファクトリー（互換性のため）
(defrecord FizzBuzz [number type-obj])

(defn create-fizz-buzz [number type]
  (->FizzBuzz number (create-type type)))

(defn fizz-buzz-generate [fb]
  (let [{:keys [number type-obj]} fb
        fizz-buzz-value (create-fizz-buzz-value number)]
    (generate-string type-obj fizz-buzz-value)))

; リスト生成用の関数
(defn generate-list
  ([max-number] (generate-list max-number 1))
  ([max-number type]
   (let [command (fizz-buzz.application/create-list-command (range 1 (inc max-number)) type)]
     (execute command))))

(defn -main
  "FizzBuzz application main entry point."
  [& args]
  (println "FizzBuzz:")
  (doseq [result (generate-list 100)]
    (println result)))