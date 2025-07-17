(ns fizz-buzz.model)

; 値オブジェクト
(defrecord FizzBuzzValue [value])

(defn create-fizz-buzz-value [value]
  (->FizzBuzzValue value))

; 数値判定ユーティリティ関数
(defn fizz? [fizz-buzz-value] 
  (zero? (mod (:value fizz-buzz-value) 3)))

(defn buzz? [fizz-buzz-value] 
  (zero? (mod (:value fizz-buzz-value) 5)))

(defn fizz-buzz? [fizz-buzz-value] 
  (and (fizz? fizz-buzz-value) (buzz? fizz-buzz-value)))

; ファーストクラスコレクション
(defrecord FizzBuzzList [list])

(defn create-fizz-buzz-list [numbers]
  (->FizzBuzzList (map create-fizz-buzz-value numbers)))
