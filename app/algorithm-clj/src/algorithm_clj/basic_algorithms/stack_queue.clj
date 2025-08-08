(ns algorithm-clj.basic-algorithms.stack-queue
  "第4章 スタックとキューの実装"
  (:import [java.util ArrayDeque]))

;; ============================
;; 固定長スタック (FixedStack)
;; ============================

(defrecord FixedStack [stk capacity ptr])

(defn make-fixed-stack
  "容量 capacity の固定長スタックを作成"
  [capacity]
  (->FixedStack (atom (vec (repeat capacity nil))) capacity (atom 0)))

(defn count-fixed-stack-elements
  "スタック内の要素数を返す"
  [^FixedStack s]
  @(:ptr s))

(defn is-empty-fixed-stack
  "スタックが空かどうかを判定"
  [^FixedStack s]
  (<= @(:ptr s) 0))

(defn is-full-fixed-stack
  "スタックが満杯かどうかを判定"
  [^FixedStack s]
  (>= @(:ptr s) (:capacity s)))

(defn push-fixed-stack
  "スタックにデータをプッシュ"
  [^FixedStack s value]
  (if (is-full-fixed-stack s)
    (throw (ex-info "Stack Full" {:stack s}))
    (do
      (swap! (:stk s) assoc @(:ptr s) value)
      (swap! (:ptr s) inc))))

(defn pop-fixed-stack
  "スタックからデータをポップ"
  [^FixedStack s]
  (if (is-empty-fixed-stack s)
    (throw (ex-info "Stack Empty" {:stack s}))
    (do
      (swap! (:ptr s) dec)
      (get @(:stk s) @(:ptr s)))))

(defn peek-fixed-stack
  "スタックの最上位要素を参照（削除しない）"
  [^FixedStack s]
  (if (is-empty-fixed-stack s)
    (throw (ex-info "Stack Empty" {:stack s}))
    (get @(:stk s) (dec @(:ptr s)))))

(defn find-fixed-stack
  "スタック内の指定した値のインデックスを検索（見つからない場合 -1）"
  [^FixedStack s value]
  (loop [i (dec @(:ptr s))]
    (if (>= i 0)
      (if (= (get @(:stk s) i) value)
        i
        (recur (dec i)))
      -1)))

(defn count-fixed-stack
  "スタック内の指定した値の個数を数える"
  [^FixedStack s value]
  (loop [i 0
         c 0]
    (if (< i @(:ptr s))
      (if (= (get @(:stk s) i) value)
        (recur (inc i) (inc c))
        (recur (inc i) c))
      c)))

(defn clear-fixed-stack
  "スタックをクリア"
  [^FixedStack s]
  (reset! (:ptr s) 0))

(defn dump-fixed-stack
  "スタックの内容をベクターで表示"
  [^FixedStack s]
  (if (is-empty-fixed-stack s)
    []
    (vec (take @(:ptr s) @(:stk s)))))

;; ============================
;; ArrayDeque を使ったスタック
;; ============================

(defn make-stack-deque
  "ArrayDeque を使ったスタックを作成"
  []
  (ArrayDeque.))

(defn push-deque
  "ArrayDeque スタックにデータをプッシュ"
  [^ArrayDeque s value]
  (.push s value))

(defn pop-deque
  "ArrayDeque スタックからデータをポップ"
  [^ArrayDeque s]
  (.pop s))

(defn peek-deque
  "ArrayDeque スタックの最上位要素を参照"
  [^ArrayDeque s]
  (.peek s))

(defn is-empty-deque
  "ArrayDeque スタックが空かどうかを判定"
  [^ArrayDeque s]
  (.isEmpty s))

(defn count-deque-elements
  "ArrayDeque スタック内の要素数を返す"
  [^ArrayDeque s]
  (.size s))

(defn clear-deque
  "ArrayDeque スタックをクリア"
  [^ArrayDeque s]
  (.clear s))

;; ============================
;; 固定長キュー (FixedQueue)
;; ============================

(defrecord FixedQueue [que capacity no front rear])

(defn make-fixed-queue
  "容量 capacity の固定長キューを作成"
  [capacity]
  (->FixedQueue (atom (vec (repeat capacity nil))) capacity (atom 0) (atom 0) (atom 0)))

(defn count-queue-elements
  "キュー内の要素数を返す"
  [^FixedQueue q]
  @(:no q))

(defn is-empty-queue
  "キューが空かどうかを判定"
  [^FixedQueue q]
  (<= @(:no q) 0))

(defn is-full-queue
  "キューが満杯かどうかを判定"
  [^FixedQueue q]
  (>= @(:no q) (:capacity q)))

(defn enque
  "キューにデータをエンキュー"
  [^FixedQueue q x]
  (if (is-full-queue q)
    (throw (ex-info "Queue Full" {:queue q}))
    (do
      (swap! (:que q) assoc @(:rear q) x)
      (swap! (:rear q) #(mod (inc %) (:capacity q)))
      (swap! (:no q) inc))))

(defn deque
  "キューからデータをデキュー"
  [^FixedQueue q]
  (if (is-empty-queue q)
    (throw (ex-info "Queue Empty" {:queue q}))
    (let [x (get @(:que q) @(:front q))]
      (swap! (:front q) #(mod (inc %) (:capacity q)))
      (swap! (:no q) dec)
      x)))

(defn peek-queue
  "キューの先頭要素を参照（削除しない）"
  [^FixedQueue q]
  (if (is-empty-queue q)
    (throw (ex-info "Queue Empty" {:queue q}))
    (get @(:que q) @(:front q))))

(defn find-queue
  "キュー内の指定した値のインデックスを検索（見つからない場合 -1）"
  [^FixedQueue q value]
  (loop [i 0]
    (if (< i @(:no q))
      (let [idx (mod (+ i @(:front q)) (:capacity q))]
        (if (= (get @(:que q) idx) value)
          i  ; 論理的な位置を返す
          (recur (inc i))))
      -1)))

(defn count-queue
  "キュー内の指定した値の個数を数える"
  [^FixedQueue q value]
  (loop [i 0
         c 0]
    (if (< i @(:no q))
      (let [idx (mod (+ i @(:front q)) (:capacity q))]
        (if (= (get @(:que q) idx) value)
          (recur (inc i) (inc c))
          (recur (inc i) c)))
      c)))

(defn clear-queue
  "キューをクリア"
  [^FixedQueue q]
  (reset! (:no q) 0)
  (reset! (:front q) 0)
  (reset! (:rear q) 0))

(defn dump-queue
  "キューの内容をベクターで表示"
  [^FixedQueue q]
  (if (is-empty-queue q)
    []
    (loop [i 0
           result []]
      (if (< i @(:no q))
        (let [idx (mod (+ i @(:front q)) (:capacity q))]
          (recur (inc i) (conj result (get @(:que q) idx))))
        result))))

;; ============================
;; ArrayDeque を使ったキュー
;; ============================

(defn make-queue-deque
  "ArrayDeque を使ったキューを作成"
  []
  (ArrayDeque.))

(defn offer-deque
  "ArrayDeque キューにデータをエンキュー"
  [^ArrayDeque q value]
  (.offer q value))

(defn poll-deque
  "ArrayDeque キューからデータをデキュー"
  [^ArrayDeque q]
  (.poll q))

(defn peek-queue-deque
  "ArrayDeque キューの先頭要素を参照"
  [^ArrayDeque q]
  (.peek q))
