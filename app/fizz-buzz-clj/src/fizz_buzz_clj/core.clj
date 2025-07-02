(ns fizz-buzz-clj.core
  (:gen-class))

(def MAX-NUMBER 100)

(defn generate
  "指定された数値に対してFizzBuzzルールを適用する"
  [number]
  (let [is-fizz (zero? (mod number 3))
        is-buzz (zero? (mod number 5))]
    (cond
      (and is-fizz is-buzz) "FizzBuzz"
      is-fizz "Fizz"
      is-buzz "Buzz"
      :else (str number))))

(defn generate-list
  "1から最大値までのFizzBuzz配列を生成する"
  []
  (map generate (range 1 (inc MAX-NUMBER))))

(defn -main
  "FizzBuzzリストを出力する"
  [& _args]
  (doseq [item (generate-list)]
    (println item)))
