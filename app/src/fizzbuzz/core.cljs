(ns fizzbuzz.core)

(defn fizzbuzz [n]
  "FizzBuzz関数: 数値を受け取ってFizzBuzz判定結果を返す"
  (cond
    (and (zero? (mod n 3)) (zero? (mod n 5))) "FizzBuzz"
    (zero? (mod n 3)) "Fizz"
    (zero? (mod n 5)) "Buzz"
    :else (str n)))

(defn fizzbuzz-list [start end]
  "指定された範囲のFizzBuzzリストを生成"
  (map fizzbuzz (range start (inc end))))

(defn format-fizzbuzz-item [item]
  "FizzBuzz結果をHTML用にフォーマット"
  (let [class-name (cond
                     (= item "FizzBuzz") "fizzbuzz"
                     (= item "Fizz") "fizz"
                     (= item "Buzz") "buzz"
                     :else "number")]
    [:span {:class class-name} item]))

(defn render-fizzbuzz-list [result-list]
  "FizzBuzzリストをHTMLとして描画"
  (let [result-element (.getElementById js/document "result")]
    (set! (.-innerHTML result-element)
          (->> result-list
               (map #(str "<span class=\"" 
                          (cond
                            (= % "FizzBuzz") "fizzbuzz"
                            (= % "Fizz") "fizz"
                            (= % "Buzz") "buzz"
                            :else "number")
                          "\">" % "</span>"))
               (interpose "<br>")
               (apply str)))))

(defn get-input-value [id]
  "入力フィールドから値を取得"
  (let [element (.getElementById js/document id)]
    (js/parseInt (.-value element))))

(defn generate-fizzbuzz []
  "FizzBuzz生成ボタンのイベントハンドラ"
  (let [start (get-input-value "start")
        end (get-input-value "end")]
    (if (and (not (js/isNaN start))
             (not (js/isNaN end))
             (<= start end)
             (> start 0))
      (let [result (fizzbuzz-list start end)]
        (render-fizzbuzz-list result))
      (let [result-element (.getElementById js/document "result")]
        (set! (.-innerHTML result-element) 
              "<span style='color: red;'>無効な入力です。開始数と終了数を正しく入力してください。</span>")))))

(defn clear-result []
  "結果をクリア"
  (let [result-element (.getElementById js/document "result")]
    (set! (.-innerHTML result-element) "")))

(defn setup-event-listeners []
  "イベントリスナーをセットアップ"
  (let [generate-btn (.getElementById js/document "generate")
        clear-btn (.getElementById js/document "clear")]
    (.addEventListener generate-btn "click" generate-fizzbuzz)
    (.addEventListener clear-btn "click" clear-result)))

(defn init []
  "アプリケーション初期化"
  (js/console.log "FizzBuzz ClojureScript アプリケーションが開始されました!")
  (setup-event-listeners))
