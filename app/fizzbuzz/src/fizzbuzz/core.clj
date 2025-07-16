(ns fizzbuzz.core)

(defn generate
  "Generate fizz buzz number"
  [n]
  (let [result (str n)]
    (cond
      (zero? (mod n 3)) "Fizz"
      (zero? (mod n 5)) "Buzz"
      :else result)))
