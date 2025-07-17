(ns fizz-buzz.core
  (:gen-class))

(defn generate 
  ([number] (generate number 1))
  ([number type]
   (let [fizz? (zero? (mod number 3))
         buzz? (zero? (mod number 5))]
     (cond
       (and fizz? buzz?) "FizzBuzz"
       fizz? "Fizz"
       buzz? "Buzz"
       :else (str number)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
