(ns fizzbuzz.core)

(defn generate
  "Generate fizz buzz number"
  [n]
  (let [result (str n)]
    (if (zero? (mod n 3))
      "Fizz"
      result)))
