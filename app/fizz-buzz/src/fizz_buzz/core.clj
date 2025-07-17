(ns fizz-buzz.core
  (:gen-class))

; 数値判定ユーティリティ関数
(defn fizz? [number] (zero? (mod number 3)))
(defn buzz? [number] (zero? (mod number 5)))
(defn fizz-buzz? [number] (and (fizz? number) (buzz? number)))

; ポリモーフィズムの準備: Typeに対応するプロトコル
(defprotocol FizzBuzzType
  (generate-string [this number]))

; FizzBuzzの通常パターン実装
(defrecord FizzBuzzType01 []
  FizzBuzzType
  (generate-string [this number]
    (cond
      (fizz-buzz? number) "FizzBuzz"
      (fizz? number) "Fizz"
      (buzz? number) "Buzz"
      :else (str number))))

; 数字のみ実装
(defrecord FizzBuzzType02 []
  FizzBuzzType
  (generate-string [this number]
    (str number)))

; FizzBuzzのみ実装
(defrecord FizzBuzzType03 []
  FizzBuzzType
  (generate-string [this number]
    (if (fizz-buzz? number)
      "FizzBuzz"
      (str number))))

; TypeFactoryメソッド
(defn create-type [type-id]
  (case type-id
    1 (->FizzBuzzType01)
    2 (->FizzBuzzType02)
    3 (->FizzBuzzType03)
    (throw (Exception. "不正なタイプです"))))

(defrecord FizzBuzz [number type-obj])

; ファクトリーメソッド：setterを削除してイミュータブルにする
(defn create-fizz-buzz [number type]
  (->FizzBuzz number (create-type type)))

(defn generate 
  ([number] (generate number 1))
  ([number type]
   (let [type-obj (create-type type)]
     (generate-string type-obj number))))

(defn fizz-buzz-generate [fb]
  (let [{:keys [number type-obj]} fb]
    (generate-string type-obj number)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
