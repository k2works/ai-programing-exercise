(ns fizzbuzz.core)

(defn fizzbuzz [n]
  (cond
    (and (= 0 (mod n 3)) (= 0 (mod n 5))) "FizzBuzz"
    (= 0 (mod n 3)) "Fizz"
    (= 0 (mod n 5)) "Buzz"
    :else (str n)))

(defn fizzbuzz-list [start end]
  (map fizzbuzz (range start (inc end))))

(defn print-fizzbuzz [start end]
  (doseq [item (fizzbuzz-list start end)]
    (println item)))

(defn -main [& args]
  (print-fizzbuzz 1 100))
