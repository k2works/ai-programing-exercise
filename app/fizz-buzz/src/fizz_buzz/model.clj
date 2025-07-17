(ns fizz-buzz.model)

;; 値オブジェクト
(defrecord FizzBuzzValue [value])

(defn create-fizz-buzz-value [value]
  {:pre [(number? value) (pos? value)]}
  (->FizzBuzzValue value))

;; 純粋関数: 数値判定ユーティリティ
(defn divisible-by? 
  "数値が指定した除数で割り切れるかチェックする純粋関数"
  [divisor number]
  (zero? (mod number divisor)))

(defn fizz? 
  "3で割り切れるかチェック"
  [fizz-buzz-value] 
  (divisible-by? 3 (:value fizz-buzz-value)))

(defn buzz? 
  "5で割り切れるかチェック"
  [fizz-buzz-value] 
  (divisible-by? 5 (:value fizz-buzz-value)))

(defn fizz-buzz? 
  "15で割り切れるかチェック（3と5の両方）"
  [fizz-buzz-value] 
  (divisible-by? 15 (:value fizz-buzz-value)))

;; 高階関数: 条件判定の合成
(defn satisfies-any? 
  "いずれかの条件を満たすかチェック"
  [predicates fizz-buzz-value]
  (some #(% fizz-buzz-value) predicates))

(defn satisfies-all? 
  "すべての条件を満たすかチェック"
  [predicates fizz-buzz-value]
  (every? #(% fizz-buzz-value) predicates))

;; 関数型のファーストクラスコレクション
(defrecord FizzBuzzList [values])

(defn create-fizz-buzz-list 
  "数値のシーケンスからFizzBuzzListを作成"
  [numbers]
  (->> numbers
       (map create-fizz-buzz-value)
       ->FizzBuzzList))

;; 純粋関数: コレクション操作
(defn filter-values
  "条件に合うFizzBuzzValueをフィルタリング"
  [predicate fizz-buzz-list]
  (->> (:values fizz-buzz-list)
       (filter predicate)
       ->FizzBuzzList))

(defn transform-values
  "FizzBuzzValueに変換関数を適用"
  [transform-fn fizz-buzz-list]
  (->> (:values fizz-buzz-list)
       (map transform-fn)
       ->FizzBuzzList))

(defn reduce-values
  "FizzBuzzValueを畳み込み演算"
  [reduce-fn init-val fizz-buzz-list]
  (reduce reduce-fn init-val (:values fizz-buzz-list)))

;; 関数合成のためのユーティリティ
(defn pipe
  "パイプライン演算子"
  [val & fns]
  (reduce (fn [v f] (f v)) val fns))

;; 具体的な判定関数の組み合わせ
(def fizz-or-buzz? 
  "FizzまたはBuzzの条件を満たすか"
  (partial satisfies-any? [fizz? buzz?]))

(def special-numbers?
  "特別な数値（Fizz, Buzz, FizzBuzz）かどうか"
  fizz-or-buzz?)

;; 数学的なユーティリティ関数
(defn gcd 
  "最大公約数を求める"
  [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm 
  "最小公倍数を求める"
  [a b]
  (/ (* a b) (gcd a b)))

;; 関数型のバリデーション
(defn valid-fizz-buzz-value?
  "有効なFizzBuzzValueかチェック"
  [fizz-buzz-value]
  (and (instance? FizzBuzzValue fizz-buzz-value)
       (number? (:value fizz-buzz-value))
       (pos? (:value fizz-buzz-value))))
