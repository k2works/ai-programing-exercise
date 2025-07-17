(ns fizz-buzz.core
  (:gen-class))

; ポリモーフィズムの準備: Typeに対応するプロトコル
(defprotocol FizzBuzzType
  (execute [this number]))

; Type1の実装
(defrecord Type1 []
  FizzBuzzType
  (execute [this number]
    (let [fizz? (zero? (mod number 3))
          buzz? (zero? (mod number 5))]
      (cond
        (and fizz? buzz?) "FizzBuzz"
        fizz? "Fizz"
        buzz? "Buzz"
        :else (str number)))))

; Type2の実装 
(defrecord Type2 []
  FizzBuzzType
  (execute [this number]
    (str number)))

; Type3の実装
(defrecord Type3 []
  FizzBuzzType
  (execute [this number]
    (let [fizz? (zero? (mod number 3))
          buzz? (zero? (mod number 5))]
      (if (and fizz? buzz?)
        "FizzBuzz"
        (str number)))))

; TypeFactoryメソッド
(defn create-type [type-id]
  (case type-id
    1 (->Type1)
    2 (->Type2)
    3 (->Type3)
    (throw (Exception. "不正なタイプです"))))

(defrecord FizzBuzz [number type-obj])

; ファクトリーメソッド：setterを削除してイミュータブルにする
(defn create-fizz-buzz [number type]
  (->FizzBuzz number (create-type type)))

(defn generate 
  ([number] (generate number 1))
  ([number type]
   (let [type-obj (create-type type)]
     (execute type-obj number))))

(defn fizz-buzz-generate [fb]
  (let [{:keys [number type-obj]} fb]
    (execute type-obj number)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
