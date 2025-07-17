(ns fizz-buzz.application
  (:require [fizz-buzz.model :refer [create-fizz-buzz-value]]
            [fizz-buzz.type :refer [create-type generate-string]]))

; Commandパターンのインターフェース
(defprotocol FizzBuzzCommand
  (execute [this]))

; 単一の値を生成するコマンド
(defrecord FizzBuzzValueCommand [number type]
  FizzBuzzCommand
  (execute [this]
    (let [fizz-buzz-value (create-fizz-buzz-value number)
          type-obj (create-type type)]
      (generate-string type-obj fizz-buzz-value))))

; リストを生成するコマンド
(defrecord FizzBuzzListCommand [numbers type]
  FizzBuzzCommand
  (execute [this]
    (map (fn [number]
           (let [fizz-buzz-value (create-fizz-buzz-value number)
                 type-obj (create-type type)]
             (generate-string type-obj fizz-buzz-value)))
         numbers)))

; ファクトリーメソッド
(defn create-value-command [number type]
  (->FizzBuzzValueCommand number type))

(defn create-list-command [numbers type]
  (->FizzBuzzListCommand numbers type))
