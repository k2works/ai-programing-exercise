(ns fizzbuzz.core
  "テスト駆動開発から始めるClojureScript入門 - FizzBuzzメイン実装
   
   このコードはテスト駆動開発のアプローチに従って作成されています:
   1. Red: 失敗するテストを書く
   2. Green: テストをパスする最小限のコードを書く
   3. Refactor: コードをリファクタリングして品質を向上させる
   
   ソフトウェア開発の三種の神器を活用:
   - バージョン管理: Git
   - テスティング: cljs.test
   - 自動化: shadow-cljs + Gulp")

(defn fizzbuzz
  "FizzBuzz関数: 数値を受け取ってFizzBuzz判定結果を返す
   
   TDDで実装された核となる関数:
   - 3の倍数の場合は 'Fizz'
   - 5の倍数の場合は 'Buzz'  
   - 3と5両方の倍数の場合は 'FizzBuzz'
   - その他の場合は数値の文字列表現"
  [n]
  (cond
    (and (zero? (mod n 3)) (zero? (mod n 5))) "FizzBuzz"
    (zero? (mod n 3)) "Fizz"
    (zero? (mod n 5)) "Buzz"
    :else (str n)))

(defn fizzbuzz-list
  "指定された範囲のFizzBuzzリストを生成
   
   引数:
   - start: 開始数値
   - end: 終了数値（この数値を含む）
   
   戻り値: FizzBuzz判定結果のベクター"
  [start end]
  (map fizzbuzz (range start (inc end))))

(defn format-fizzbuzz-item
  "FizzBuzz結果をHTML用にフォーマット（将来の拡張用）
   
   現在は使用されていませんが、リファクタリングの一環として
   UI表示ロジックの分離を想定して作成"
  [item]
  (let [class-name (cond
                     (= item "FizzBuzz") "fizzbuzz"
                     (= item "Fizz") "fizz"
                     (= item "Buzz") "buzz"
                     :else "number")]
    [:span {:class class-name} item]))

(defn render-fizzbuzz-list
  "FizzBuzzリストをHTMLとして描画
   
   DOM操作を行ってブラウザに結果を表示します。
   関数型プログラミングの原則に従い、副作用を明確に分離。"
  [result-list]
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

(defn get-input-value
  "入力フィールドから値を取得
   
   DOM要素から数値を取得し、JavaScriptの数値に変換"
  [id]
  (let [element (.getElementById js/document id)]
    (js/parseInt (.-value element))))

(defn generate-fizzbuzz
  "FizzBuzz生成ボタンのイベントハンドラ
   
   ユーザー入力を検証し、有効な場合はFizzBuzzを生成・表示"
  []
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

(defn clear-result
  "結果をクリア
   
   表示結果をクリアしてリセット状態に戻す"
  []
  (let [result-element (.getElementById js/document "result")]
    (set! (.-innerHTML result-element) "")))

(defn setup-event-listeners
  "イベントリスナーをセットアップ
   
   DOM要素にイベントハンドラを登録"
  []
  (let [generate-btn (.getElementById js/document "generate")
        clear-btn (.getElementById js/document "clear")]
    (.addEventListener generate-btn "click" generate-fizzbuzz)
    (.addEventListener clear-btn "click" clear-result)))

(defn init
  "アプリケーション初期化
   
   ClojureScriptアプリケーションのエントリーポイント。
   DOM準備完了後にイベントリスナーをセットアップ。"
  []
  (js/console.log "FizzBuzz ClojureScript アプリケーションが開始されました!")
  (setup-event-listeners))
