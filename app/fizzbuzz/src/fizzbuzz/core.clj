(ns fizzbuzz.core)

(defn generate
  "Generate fizz buzz number"
  [n]
  (cond
    (zero? (mod n 15)) "FizzBuzz"
(zero? (mod n 3)) "Fizz"
(zero? (mod n 5)) "Buzz"
    :else (str n)))

(defn print-fizzbuzz
  "Print fizz buzz numbers from 1 to 100"
  []
  (let [results (map generate (range 1 101))]
    (doseq [result results]
      (println result))
    results))
