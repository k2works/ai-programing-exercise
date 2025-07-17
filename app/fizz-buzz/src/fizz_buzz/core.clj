(ns fizz-buzz.core
  (:gen-class))

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
   (let [fizz-buzz-value (create-fizz-buzz-value number)
        type-obj (create-type type)]
    (generate-string type-obj fizz-buzz-value))))

(defn fizz-buzz-generate [fb]
  (let [{:keys [number type-obj]} fb
       fizz-buzz-value (create-fizz-buzz-value number)]
   (generate-string type-obj fizz-buzz-value)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
