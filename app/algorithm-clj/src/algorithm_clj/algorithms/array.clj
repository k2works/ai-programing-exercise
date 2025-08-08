(ns algorithm-clj.algorithms.array)

;; 5人の点数から合計と平均を計算（個別引数版）
(defn calculate-scores-individual
  "5人の点数を個別に受け取り、合計と平均を計算する"
  [tensu1 tensu2 tensu3 tensu4 tensu5]
  (let [total (+ tensu1 tensu2 tensu3 tensu4 tensu5)
        average (/ total 5.0)]
    (str total "," average)))

;; 配列の要素の最大値を求める
(defn max-of
  "シーケンスaの要素の最大値を返却する"
  [a]
  (reduce max a))

;; 配列の要素の並びを反転する
(defn reverse-vector
  "ベクターaの要素の並びを反転した新しいベクターを返却する"
  [a]
  (vec (reverse a)))

;; 基数変換
(defn card-conv
  "整数値xをr進数に変換した数値を表す文字列を返却"
  [x r]
  (if (zero? x)
    "0"
    (let [dchar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
      (loop [n x
             result ""]
        (if (zero? n)
          (apply str (reverse result))
          (recur (quot n r)
                 (str result (nth dchar (mod n r)))))))))

;; 素数の列挙（第1版）
(defn prime1
  "x以下の素数を列挙（第１版）- 除算回数を返す"
  [x]
  (loop [n 2
         counter 0]
    (if (> n x)
      counter
      (let [new-counter (loop [i 2
                               current-counter counter]
                          (if (>= i n)
                            current-counter
                            (let [c (inc current-counter)]
                              (if (zero? (mod n i))
                                c ; break
                                (recur (inc i) c)))))]
        (recur (inc n) new-counter)))))

;; 素数の列挙（第2版）
(defn prime2
  "x以下の素数を列挙（第２版）- 除算回数を返す"
  [x]
  (if (< x 2)
    0
    (loop [n 3
           counter 0
           primes [2] ; 見つかった素数を格納するベクター
           ptr 1] ; primesの次の空きインデックス
      (if (> n x)
        counter
        (let [broke? (atom false)
              new-counter (loop [i 0
                                 current-counter counter]
                            (if (or @broke? (>= i ptr))
                              current-counter
                              (let [p (nth primes i)
                                    c (inc current-counter)]
                                (if (zero? (mod n p))
                                  (do (reset! broke? true)
                                      c) ; break
                                  (recur (inc i) c)))))]
          (if @broke?
            (recur (+ n 2) new-counter primes ptr)
            (recur (+ n 2) new-counter (conj primes n) (inc ptr))))))))

;; 素数の列挙（第3版）
(defn prime3
  "x以下の素数を列挙（第３版）- 除算回数を返す"
  [x]
  (if (< x 2)
    0
    (loop [n 5
           counter 0
           primes [2 3] ; 見つかった素数を格納するベクター
           ptr 2] ; primesの次の空きインデックス
      (if (> n x)
        counter
        (let [broke? (atom false)
              new-counter (loop [i 1 ; primes[0]は2なので1から
                                 current-counter counter]
                            (if (or @broke? (>= i ptr))
                              current-counter
                              (let [p (nth primes i)]
                                (if (> (* p p) n)
                                  (+ current-counter 1) ; else節のcounter += 1
                                  (let [c (+ current-counter 2)] ; counter += 2
                                    (if (zero? (mod n p))
                                      (do (reset! broke? true)
                                          c) ; break
                                      (recur (inc i) c)))))))]
          (if @broke?
            (recur (+ n 2) new-counter primes ptr)
            (recur (+ n 2) new-counter (conj primes n) (inc ptr))))))))
