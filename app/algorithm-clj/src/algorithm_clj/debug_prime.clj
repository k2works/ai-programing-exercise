(ns algorithm-clj.debug-prime
  (:require [algorithm-clj.basic-algorithms.array :as array]))

(defn prime-debug
  "素数を実際に列挙してデバッグ用に表示"
  [x]
  (loop [n 2
         primes []]
    (if (> n x)
      primes
      (let [is-prime? (loop [i 2]
                        (cond
                          (>= i n) true
                          (zero? (mod n i)) false
                          :else (recur (inc i))))]
        (if is-prime?
          (recur (inc n) (conj primes n))
          (recur (inc n) primes))))))

;; 100以下の素数を数える
(defn count-primes [max-n]
  (count (prime-debug max-n)))

(defn -main []
  (println "100以下の素数:")
  (println (prime-debug 100))
  (println "100以下の素数の個数:" (count-primes 100))
  (println "1000以下の素数の個数:" (count-primes 1000))
  
  (println "\n各アルゴリズムの除算回数:")
  (println "prime1(100):" (array/prime1 100))
  (println "prime1(1000):" (array/prime1 1000))
  (println "prime2(100):" (array/prime2 100))
  (println "prime2(1000):" (array/prime2 1000))
  (println "prime3(100):" (array/prime3 100))
  (println "prime3(1000):" (array/prime3 1000)))
