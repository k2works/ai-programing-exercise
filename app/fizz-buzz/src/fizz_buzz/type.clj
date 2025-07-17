(ns fizz-buzz.type
  (:require [fizz-buzz.model :refer [fizz? buzz? fizz-buzz?]]))

; ポリモーフィズムの準備: Typeに対応するプロトコル
(defprotocol FizzBuzzType
  (generate-string [this fizz-buzz-value]))

; FizzBuzzの通常パターン実装
(defrecord FizzBuzzType01 []
  FizzBuzzType
  (generate-string [this fizz-buzz-value]
    (cond
      (fizz-buzz? fizz-buzz-value) "FizzBuzz"
      (fizz? fizz-buzz-value) "Fizz"
      (buzz? fizz-buzz-value) "Buzz"
      :else (str (:value fizz-buzz-value)))))

; 数字のみ実装
(defrecord FizzBuzzType02 []
  FizzBuzzType
  (generate-string [this fizz-buzz-value]
    (str (:value fizz-buzz-value))))

; FizzBuzzのみ実装
(defrecord FizzBuzzType03 []
  FizzBuzzType
  (generate-string [this fizz-buzz-value]
    (if (fizz-buzz? fizz-buzz-value)
      "FizzBuzz"
      (str (:value fizz-buzz-value)))))

; 例外となる未定義タイプ
(defrecord FizzBuzzTypeNotDefined []
  FizzBuzzType
  (generate-string [this fizz-buzz-value]
    (throw (Exception. "不正なタイプです"))))

; TypeFactoryメソッド
(defn create-type [type-id]
  (case type-id
    1 (->FizzBuzzType01)
    2 (->FizzBuzzType02)
    3 (->FizzBuzzType03)
    (->FizzBuzzTypeNotDefined)))
