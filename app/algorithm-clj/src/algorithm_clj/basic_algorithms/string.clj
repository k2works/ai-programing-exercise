(ns algorithm-clj.basic-algorithms.string
  "第7章: 文字列アルゴリズムの実装"
  (:require [clojure.string :as str]))

;; ============================
;; 文字列の基本操作
;; ============================

(defn string-length
  "文字列の長さを求める"
  [s]
  (count s))

(defn string-concat
  "文字列を連結する"
  [& strings]
  (apply str strings))

(defn substring
  "部分文字列を抽出する"
  [s start end]
  (subs s start end))

;; ============================
;; 文字列検索アルゴリズム
;; ============================

(defn string-search-simple
  "単純な文字列検索（総当たり法）"
  [text pattern]
  (let [text-len (count text)
        pattern-len (count pattern)]
    (cond
      (zero? pattern-len) 0
      (> pattern-len text-len) -1
      :else
      (loop [i 0]
        (cond
          (> (+ i pattern-len) text-len) -1
          (= (subs text i (+ i pattern-len)) pattern) i
          :else (recur (inc i)))))))

;; KMP法の実装
(defn compute-lps
  "KMP法のための最長共通接頭辞・接尾辞配列を計算"
  [pattern]
  (let [m (count pattern)
        lps (vec (repeat m 0))]
    (loop [len 0
           i 1
           current-lps lps]
      (cond
        (>= i m) current-lps
        (= (nth pattern i) (nth pattern len))
        (recur (inc len) (inc i) (assoc current-lps i (inc len)))
        (> len 0)
        (recur (nth current-lps (dec len)) i current-lps)
        :else
        (recur 0 (inc i) (assoc current-lps i 0))))))

(defn string-search-kmp
  "KMP法による文字列検索"
  [text pattern]
  (let [n (count text)
        m (count pattern)]
    (cond
      (zero? m) 0
      (> m n) -1
      :else
      (let [lps (compute-lps pattern)]
        (loop [i 0  ; text のインデックス
               j 0] ; pattern のインデックス
          (cond
            (>= i n) -1
            (= (nth text i) (nth pattern j))
            (if (= j (dec m))
              (- i j)
              (recur (inc i) (inc j)))
            (> j 0)
            (recur i (nth lps (dec j)))
            :else
            (recur (inc i) 0)))))))

;; Boyer-Moore法の実装
(defn build-bad-char-table
  "Boyer-Moore法のための不正文字テーブルを構築"
  [pattern]
  (let [m (count pattern)]
    (reduce (fn [table i]
              (assoc table (nth pattern i) i))
            {}
            (range m))))

(defn string-search-boyer-moore
  "Boyer-Moore法による文字列検索（簡易版）"
  [text pattern]
  (let [n (count text)
        m (count pattern)]
    (cond
      (zero? m) 0
      (> m n) -1
      :else
      (let [bad-char (build-bad-char-table pattern)]
        (loop [s 0] ; text での開始位置
          (if (> (+ s m) n)
            -1
            (let [j (loop [j (dec m)]
                      (cond
                        (< j 0) j
                        (= (nth text (+ s j)) (nth pattern j)) (recur (dec j))
                        :else j))]
              (if (< j 0)
                s
                (let [bad-char-shift (max 1 (- j (get bad-char (nth text (+ s j)) -1)))]
                  (recur (+ s bad-char-shift)))))))))))

;; ============================
;; 文字列変換
;; ============================

(defn string-reverse
  "文字列を反転する"
  [s]
  (apply str (reverse s)))

(defn string-uppercase
  "文字列を大文字に変換"
  [s]
  (str/upper-case s))

(defn string-lowercase
  "文字列を小文字に変換"
  [s]
  (str/lower-case s))

;; ============================
;; 文字列解析
;; ============================

(defn is-palindrome
  "回文かどうかを判定"
  [s]
  (let [cleaned (str/lower-case s)]
    (= cleaned (string-reverse cleaned))))

(defn is-anagram
  "2つの文字列がアナグラムかどうかを判定"
  [s1 s2]
  (= (sort (str/lower-case s1))
     (sort (str/lower-case s2))))

;; ============================
;; 文字列圧縮
;; ============================

(defn run-length-encode
  "ランレングス符号化"
  [s]
  (if (empty? s)
    ""
    (loop [i 0
           current-char (first s)
           char-count 1
           result ""]
      (if (< i (dec (count s)))
        (let [next-char (nth s (inc i))]
          (if (= current-char next-char)
            (recur (inc i) current-char (inc char-count) result)
            (recur (inc i) next-char 1 (str result current-char char-count))))
        (str result current-char char-count)))))

(defn run-length-decode
  "ランレングス復号化"
  [s]
  (if (empty? s)
    ""
    (loop [i 0
           result ""]
      (if (>= i (count s))
        result
        (let [char (nth s i)
              [count-str next-i] (loop [j (inc i)
                                       num-str ""]
                                   (if (or (>= j (count s))
                                          (not (Character/isDigit (nth s j))))
                                     [num-str j]
                                     (recur (inc j) (str num-str (nth s j)))))
              repeat-count (Integer/parseInt count-str)]
          (recur next-i (str result (apply str (repeat repeat-count char)))))))))

;; ============================
;; パターンマッチング
;; ============================

(defn pattern-match-wildcard
  "ワイルドカードパターンマッチング (* は任意文字列、? は任意1文字)"
  [text pattern]
  (letfn [(match-helper [t-idx p-idx]
            (cond
              (= p-idx (count pattern)) (= t-idx (count text))
              (= (nth pattern p-idx) \*) 
              (or (match-helper t-idx (inc p-idx))
                  (and (< t-idx (count text))
                       (match-helper (inc t-idx) p-idx)))
              (= t-idx (count text)) false
              (or (= (nth pattern p-idx) \?)
                  (= (nth pattern p-idx) (nth text t-idx)))
              (match-helper (inc t-idx) (inc p-idx))
              :else false))]
    (match-helper 0 0)))

;; ============================
;; 編集距離
;; ============================

(defn edit-distance
  "レーベンシュタイン距離（編集距離）を計算"
  [s1 s2]
  (let [m (count s1)
        n (count s2)]
    (if (zero? m)
      n
      (if (zero? n)
        m
        (let [dp (vec (repeat (inc m) (vec (repeat (inc n) 0))))
              ; 初期化
              dp (reduce (fn [dp i] (assoc-in dp [i 0] i)) dp (range (inc m)))
              dp (reduce (fn [dp j] (assoc-in dp [0 j] j)) dp (range (inc n)))]
          (loop [i 1
                 current-dp dp]
            (if (> i m)
              (get-in current-dp [m n])
              (recur (inc i)
                     (loop [j 1
                            inner-dp current-dp]
                       (if (> j n)
                         inner-dp
                         (let [cost (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1)
                               deletion (inc (get-in inner-dp [(dec i) j]))
                               insertion (inc (get-in inner-dp [i (dec j)]))
                               substitution (+ (get-in inner-dp [(dec i) (dec j)]) cost)]
                           (recur (inc j)
                                  (assoc-in inner-dp [i j] (min deletion insertion substitution))))))))))))))
