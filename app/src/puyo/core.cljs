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

;; ランダム生成システム
(defn generate-random-color
  "有効なぷよの色をランダムに生成
   
   Returns:
     1-5の範囲でランダムな色番号"
  []
  (+ 1 (rand-int 5)))

(defn generate-random-puyo-pair
  "指定位置にランダムな色の組ぷよを生成
   
   Args:
     x: 初期x座標
     y: 初期y座標
   
   Returns:
     ランダムな色の組ぷよマップ"
  [x y]
  (let [color1 (generate-random-color)
        color2 (generate-random-color)]
    (create-puyo-pair color1 color2 x y)))

(defn setup-next-puyo
  "NEXTぷよを生成
   
   Returns:
     NEXTぷよとして使用する組ぷよマップ"
  []
  {:puyo1 {:color (generate-random-color)}
   :puyo2 {:color (generate-random-color)}})

(defn get-current-puyo-from-next
  "NEXTぷよから現在の組ぷよを生成
   
   Args:
     next-puyo: NEXTぷよマップ
     x: 初期配置のx座標
     y: 初期配置のy座標
   
   Returns:
     指定位置に配置された組ぷよマップ"
  [next-puyo x y]
  (create-puyo-pair
   (get-in next-puyo [:puyo1 :color])
   (get-in next-puyo [:puyo2 :color])
   x y))

(defn update-next-puyo
  "新しいNEXTぷよを生成
   
   Returns:
     新しいNEXTぷよマップ"
  []
  (setup-next-puyo))

;; 移動システム
(defn valid-direction?
  "移動方向が有効かどうかチェック
   
   Args:
     direction: :left または :right
   
   Returns:
     有効な方向の場合true"
  [direction]
  (contains? #{:left :right} direction))

(defn can-move?
  "組ぷよが指定方向に移動可能かチェック
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
     direction: 移動方向 (:left または :right)
   
   Returns:
     移動可能な場合true"
  [puyo-pair board direction]
  (when-not (valid-direction? direction)
    (throw (js/Error. (str "Invalid direction: " direction))))
  
  (let [positions (get-puyo-pair-positions
                   (get-in puyo-pair [:puyo1 :x])
                   (get-in puyo-pair [:puyo1 :y])
                   (:rotation puyo-pair))
        offset (case direction
                 :left -1
                 :right 1)
        new-positions (map #(assoc % :x (+ (:x %) offset)) positions)]
    
    ;; すべての新しい位置が有効な範囲内かチェック
    (every? (fn [{:keys [x y]}]
              (and (>= x 0) (< x board-width)
                   (>= y 0) (< y board-height)
                   ;; 将来的にはボードの衝突判定も追加予定
                   (= 0 (get-in board [y x] 0))))
            new-positions)))

(defn move-puyo-pair-left
  "組ぷよを左に移動
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
   
   Returns:
     移動後の組ぷよマップ（移動不可の場合は元のまま）"
  [puyo-pair board]
  (if (can-move? puyo-pair board :left)
    (let [new-x (dec (get-in puyo-pair [:puyo1 :x]))
          new-y (get-in puyo-pair [:puyo1 :y])
          positions (get-puyo-pair-positions new-x new-y (:rotation puyo-pair))]
      (assoc puyo-pair
             :puyo1 (assoc (get-in puyo-pair [:puyo1])
                           :x (:x (first positions))
                           :y (:y (first positions)))
             :puyo2 (assoc (get-in puyo-pair [:puyo2])
                           :x (:x (second positions))
                           :y (:y (second positions)))))
    puyo-pair))

(defn move-puyo-pair-right
  "組ぷよを右に移動
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
   
   Returns:
     移動後の組ぷよマップ（移動不可の場合は元のまま）"
  [puyo-pair board]
  (if (can-move? puyo-pair board :right)
    (let [new-x (inc (get-in puyo-pair [:puyo1 :x]))
          new-y (get-in puyo-pair [:puyo1 :y])
          positions (get-puyo-pair-positions new-x new-y (:rotation puyo-pair))]
      (assoc puyo-pair
             :puyo1 (assoc (get-in puyo-pair [:puyo1])
                           :x (:x (first positions))
                           :y (:y (first positions)))
             :puyo2 (assoc (get-in puyo-pair [:puyo2])
                           :x (:x (second positions))
                           :y (:y (second positions)))))
    puyo-pair))

;; 下移動と重力システム
(defn can-fall?
  "組ぷよが落下可能かチェック
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
   
   Returns:
     落下可能な場合true"
  [puyo-pair board]
  (let [positions (get-puyo-pair-positions
                   (get-in puyo-pair [:puyo1 :x])
                   (get-in puyo-pair [:puyo1 :y])
                   (:rotation puyo-pair))]
    ;; 各ぷよが底面に到達していないかチェック
    (every? (fn [pos]
              (let [new-y (inc (:y pos))]
                (and (< new-y board-height)
                     (= 0 (get-in board [new-y (:x pos)])))))
            positions)))

(defn move-puyo-pair-down
  "組ぷよを下に移動
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
   
   Returns:
     移動後の組ぷよマップ（移動不可の場合は元のまま）"
  [puyo-pair board]
  (if (can-fall? puyo-pair board)
    (let [new-x (get-in puyo-pair [:puyo1 :x])
          new-y (inc (get-in puyo-pair [:puyo1 :y]))
          positions (get-puyo-pair-positions new-x new-y (:rotation puyo-pair))]
      (assoc puyo-pair
             :puyo1 (assoc (get-in puyo-pair [:puyo1])
                           :x (:x (first positions))
                           :y (:y (first positions)))
             :puyo2 (assoc (get-in puyo-pair [:puyo2])
                           :x (:x (second positions))
                           :y (:y (second positions)))))
    puyo-pair))

(defn soft-drop
  "ソフトドロップ（高速落下）- 1段階下に移動
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
   
   Returns:
     移動後の組ぷよマップ"
  [puyo-pair board]
  (move-puyo-pair-down puyo-pair board))

(defn hard-drop
  "ハードドロップ（瞬間落下）- 底面まで一気に移動
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
   
   Returns:
     底面に到達した組ぷよマップ"
  [puyo-pair board]
  (loop [current-pair puyo-pair]
    (if (can-fall? current-pair board)
      (recur (move-puyo-pair-down current-pair board))
      current-pair)))

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
