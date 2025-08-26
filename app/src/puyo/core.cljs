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

;; 有効な色の範囲
(def valid-colors #{1 2 3 4 5})

;; 有効な回転状態（0: 縦, 1: 右, 2: 逆縦, 3: 左）
(def valid-rotations #{0 1 2 3})

;; 回転状態の名前マッピング
(def rotation-names {0 "縦" 1 "右" 2 "逆縦" 3 "左"})

(defn valid-color?
  "色が有効かどうかチェック
   有効な色: 1(赤) 2(緑) 3(青) 4(黄) 5(紫)"
  [color]
  (contains? valid-colors color))

(defn valid-rotation?
  "回転状態が有効かどうかチェック
   有効な回転: 0(縦) 1(右) 2(逆縦) 3(左)"
  [rotation]
  (contains? valid-rotations rotation))

(defn create-puyo-pair
  "組ぷよ（2個セット）を作成
   
   Args:
     color1: puyo1の色 (1-5)
     color2: puyo2の色 (1-5)  
     x: 基準位置のx座標
     y: 基準位置のy座標
   
   Returns:
     組ぷよマップ {:puyo1 {...} :puyo2 {...} :rotation 0}"
  [color1 color2 x y]
  (when-not (valid-color? color1)
    (throw (js/Error. (str "Invalid color for puyo1: " color1))))
  (when-not (valid-color? color2)
    (throw (js/Error. (str "Invalid color for puyo2: " color2))))
  (when-not (and (>= y 0) (>= x 0))
    (throw (js/Error. (str "Invalid position: x=" x " y=" y))))

  {:puyo1 {:color color1 :x x :y y}
   :puyo2 {:color color2 :x x :y (inc y)}
   :rotation 0})

(defn get-puyo-pair-positions
  "組ぷよの回転状態に基づいて2つのぷよの位置を計算
   
   Args:
     x, y: 基準ぷよ（puyo1）の位置
     rotation: 回転状態 (0-3)
   
   Returns:
     [{:x x1 :y y1} {:x x2 :y y2}] の形式で2つのぷよの位置"
  [x y rotation]
  (when-not (valid-rotation? rotation)
    (throw (js/Error. (str "Invalid rotation: " rotation))))

  (case rotation
    0 [{:x x :y y} {:x x :y (inc y)}]        ; 縦向き: puyo2が下
    1 [{:x x :y y} {:x (inc x) :y y}]        ; 右向き: puyo2が右
    2 [{:x x :y y} {:x x :y (dec y)}]        ; 逆縦向き: puyo2が上
    3 [{:x x :y y} {:x (dec x) :y y}]))      ; 左向き: puyo2が左

(defn rotate-puyo-pair
  "組ぷよを時計回りに90度回転
   
   Args:
     puyo-pair: 組ぷよマップ
   
   Returns:
     回転後の組ぷよマップ"
  [puyo-pair]
  (let [current-rotation (:rotation puyo-pair)
        new-rotation (mod (inc current-rotation) 4)
        base-pos {:x (get-in puyo-pair [:puyo1 :x])
                  :y (get-in puyo-pair [:puyo1 :y])}
        positions (get-puyo-pair-positions (:x base-pos) (:y base-pos) new-rotation)]
    (assoc puyo-pair
           :rotation new-rotation
           :puyo1 (assoc (get-in puyo-pair [:puyo1])
                         :x (:x (first positions))
                         :y (:y (first positions)))
           :puyo2 (assoc (get-in puyo-pair [:puyo2])
                         :x (:x (second positions))
                         :y (:y (second positions))))))

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
