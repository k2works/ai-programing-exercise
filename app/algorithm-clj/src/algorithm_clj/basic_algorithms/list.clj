(ns algorithm-clj.basic-algorithms.list
  (:import [java.util LinkedList]))

;; Javaの LinkedList を使った線形リスト操作

(defn make-java-linked-list
  "新しいJava LinkedListを作成"
  []
  (LinkedList.))

(defn add-first-java-linked-list
  "LinkedListの先頭に要素を追加"
  [^LinkedList ll data]
  (.addFirst ll data))

(defn add-last-java-linked-list
  "LinkedListの末尾に要素を追加"
  [^LinkedList ll data]
  (.addLast ll data))

(defn remove-first-java-linked-list
  "LinkedListの先頭要素を削除して返す"
  [^LinkedList ll]
  (.removeFirst ll))

(defn remove-last-java-linked-list
  "LinkedListの末尾要素を削除して返す"
  [^LinkedList ll]
  (.removeLast ll))

(defn search-java-linked-list
  "LinkedList内で指定したデータのインデックスを検索"
  [^LinkedList ll data]
  (.indexOf ll data))

;; Clojureの組み込みリスト操作の例

(defn clojure-list-demo
  "Clojureリストの基本操作のデモ"
  []
  (let [my-list '(1 2 3 4 5)]
    {:original my-list
     :with-cons (cons 0 my-list)
     :first-element (first my-list)
     :rest-elements (rest my-list)
     :count (count my-list)}))

(defn traverse-list
  "リストの要素を走査して収集"
  [lst]
  (loop [current lst
         result []]
    (if (empty? current)
      result
      (recur (rest current) (conj result (first current))))))
