(ns puyo.core)

;; ゲーム状態の初期化
(defonce game-state (atom {:board []
                           :current-piece nil
                           :score 0
                           :level 1
                           :game-running false}))

;; HTML要素への参照
(defonce canvas (atom nil))
(defonce ctx (atom nil))

;; ゲームボードの設定
(def board-width 8)
(def board-height 12)
(def cell-size 40)

;; 色の定義
(def colors {0 "#ffffff"  ; 空
             1 "#ff0000"  ; 赤
             2 "#00ff00"  ; 緑
             3 "#0000ff"  ; 青
             4 "#ffff00"  ; 黄
             5 "#ff00ff"}) ; 紫

(defn create-empty-board
  "空のゲームボードを作成"
  []
  (vec (repeat board-height (vec (repeat board-width 0)))))

(defn init-game-state!
  "ゲーム状態を初期化"
  []
  (reset! game-state {:board (create-empty-board)
                      :current-piece nil
                      :score 0
                      :level 1
                      :game-running false}))

(defn draw-cell
  "セルを描画"
  [x y color]
  (when @ctx
    (set! (.-fillStyle @ctx) color)
    (.fillRect @ctx (* x cell-size) (* y cell-size) cell-size cell-size)
    (set! (.-strokeStyle @ctx) "#000000")
    (.strokeRect @ctx (* x cell-size) (* y cell-size) cell-size cell-size)))

(defn draw-board
  "ゲームボードを描画"
  []
  (let [board (:board @game-state)]
    (doseq [y (range board-height)
            x (range board-width)]
      (let [cell-value (get-in board [y x])
            color (get colors cell-value "#ffffff")]
        (draw-cell x y color)))))

(defn update-score-display
  "スコア表示を更新"
  []
  (when-let [score-elem (.getElementById js/document "score")]
    (set! (.-textContent score-elem) (str (:score @game-state))))
  (when-let [level-elem (.getElementById js/document "level")]
    (set! (.-textContent level-elem) (str (:level @game-state)))))

(defn render-game
  "ゲーム画面を描画"
  []
  (when @ctx
    ;; 画面クリア
    (set! (.-fillStyle @ctx) "#f0f0f0")
    (.fillRect @ctx 0 0 (* board-width cell-size) (* board-height cell-size))

    ;; ボード描画
    (draw-board)

    ;; UI更新
    (update-score-display)))

(defn start-game
  "ゲームを開始"
  []
  (init-game-state!)
  (swap! game-state assoc :game-running true)
  (render-game)
  (js/console.log "ゲーム開始!"))

(defn reset-game
  "ゲームをリセット"
  []
  (init-game-state!)
  (render-game)
  (js/console.log "ゲームリセット"))

(defn setup-event-listeners
  "イベントリスナーを設定"
  []
  ;; ゲーム開始ボタン
  (when-let [start-btn (.getElementById js/document "start-button")]
    (.addEventListener start-btn "click" start-game))

  ;; リセットボタン
  (when-let [reset-btn (.getElementById js/document "reset-button")]
    (.addEventListener reset-btn "click" reset-game))

  ;; キーボードイベント
  (.addEventListener js/document "keydown"
                     (fn [event]
                       (when (:game-running @game-state)
                         (let [key (.-key event)]
                           (case key
                             "ArrowLeft" (js/console.log "左移動")
                             "ArrowRight" (js/console.log "右移動")
                             "ArrowDown" (js/console.log "高速落下")
                             "ArrowUp" (js/console.log "回転")
                             " " (js/console.log "ハードドロップ")
                             nil))))))

(defn init
  "アプリケーション初期化"
  []
  (js/console.log "Puyo Puyo Game 初期化中...")

  ;; Canvas要素の取得
  (when-let [canvas-elem (.getElementById js/document "game-board")]
    (reset! canvas canvas-elem)
    (reset! ctx (.getContext canvas-elem "2d")))

  ;; ゲーム状態初期化
  (init-game-state!)

  ;; イベントリスナー設定
  (setup-event-listeners)

  ;; 初期描画
  (render-game)

  (js/console.log "初期化完了!"))
