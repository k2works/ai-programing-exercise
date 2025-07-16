(ns fizzbuzz.core)

(defn- divisible-by? 
  "Check if n is divisible by divisor"
  [divisor n]
  (zero? (mod n divisor)))

(defn- fizzbuzz-rule
  "Apply fizzbuzz rules to a number using function composition"
  [n]
  (let [div-by-3? (partial divisible-by? 3)
        div-by-5? (partial divisible-by? 5)
        div-by-15? (every-pred div-by-3? div-by-5?)]
    (cond
      (div-by-15? n) "FizzBuzz"
      (div-by-3? n) "Fizz"
      (div-by-5? n) "Buzz"
      :else (str n))))

;; Alternative implementation using function composition
(defn- fizzbuzz-rule-v2
  "Alternative fizzbuzz implementation using function composition"
  [n]
  (let [fizz (when (divisible-by? 3 n) "Fizz")
        buzz (when (divisible-by? 5 n) "Buzz")
        result (str fizz buzz)]
    (if (empty? result) (str n) result)))

(defn generate
  "Generate fizz buzz number using functional approach"
  [n]
  (fizzbuzz-rule n))

(defn generate-v2
  "Generate fizz buzz number using alternative functional approach"
  [n]
  (fizzbuzz-rule-v2 n))

(defn fizzbuzz-sequence
  "Generate fizzbuzz sequence for given range"
  ([end] (fizzbuzz-sequence 1 end))
  ([start end]
   (map generate (range start (inc end)))))

(defn fizzbuzz-lazy-seq
  "Generate infinite fizzbuzz lazy sequence starting from 1"
  []
  (map generate (iterate inc 1)))

(defn print-fizzbuzz
  "Print fizz buzz numbers from 1 to 100"
  []
  (let [results (fizzbuzz-sequence 100)]
    (run! println results)
    results))
