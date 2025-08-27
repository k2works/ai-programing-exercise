(ns puyo.core)

;; ゲーム状態の初期化
(defonce game-state (atom {:board []
                           :current-piece nil
                           :score 0
                           :level 1
                           :chain-count 0
                           :game-time 0
                           :game-running false}))

;; HTML要素への参照
(defonce canvas (atom nil))
(defonce ctx (atom nil))
(defonce game-timer (atom nil))
(defonce drop-timer (atom nil))

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

(defn spawn-new-puyo-pair
  "新しい組ぷよをボード上部中央に生成
   
   Returns:
     初期位置に配置された組ぷよマップ"
  []
  (let [start-x (quot board-width 2)
        start-y 0]
    (generate-random-puyo-pair start-x start-y)))

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

(defn can-place-puyo-pair?
  "組ぷよが指定位置に配置可能かチェック
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
   
   Returns:
     配置可能な場合true"
  [puyo-pair board]
  (let [positions (get-puyo-pair-positions
                   (get-in puyo-pair [:puyo1 :x])
                   (get-in puyo-pair [:puyo1 :y])
                   (:rotation puyo-pair))]
    ;; すべての位置が有効な範囲内かつ空きマスかチェック
    (every? (fn [{:keys [x y]}]
              (and (>= x 0) (< x board-width)
                   (>= y 0) (< y board-height)
                   (= 0 (get-in board [y x] 0))))
            positions)))

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

;; ぷよ固定システム
(defn should-fix-puyo?
  "組ぷよが固定されるべきかどうか判定
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
   
   Returns:
     固定すべき場合true"
  [puyo-pair board]
  (not (can-fall? puyo-pair board)))

(defn fix-puyo-pair-to-board
  "組ぷよをボードに固定
   
   Args:
     puyo-pair: 組ぷよマップ
     board: ゲームボード
   
   Returns:
     ぷよが固定された新しいボード"
  [puyo-pair board]
  (let [positions (get-puyo-pair-positions
                   (get-in puyo-pair [:puyo1 :x])
                   (get-in puyo-pair [:puyo1 :y])
                   (:rotation puyo-pair))
        colors [(get-in puyo-pair [:puyo1 :color])
                (get-in puyo-pair [:puyo2 :color])]]
    (reduce (fn [board [pos color]]
              (assoc-in board [(:y pos) (:x pos)] color))
            board
            (map vector positions colors))))

(defn drop-floating-puyos
  "浮いているぷよを落下させる
   
   Args:
     board: ゲームボード
   
   Returns:
     浮いているぷよが落下した新しいボード"
  [board]
  (letfn [(drop-column [column]
            (let [non-empty-puyos (filter #(not= 0 %) column)
                  empty-spaces (- (count column) (count non-empty-puyos))
                  dropped-column (concat (repeat empty-spaces 0) non-empty-puyos)]
              (vec dropped-column)))]
    (let [columns (for [x (range board-width)]
                    (drop-column (mapv #(get % x) board)))]
      (reduce (fn [new-board [x column]]
                (reduce (fn [board [y value]]
                          (assoc-in board [y x] value))
                        new-board
                        (map-indexed vector column)))
              (vec (repeat board-height (vec (repeat board-width 0))))
              (map-indexed vector columns)))))

;; ぷよ消去システム
(defn find-adjacent-puyos
  "指定位置から同色の隣接ぷよを検索（幅優先探索）
   
   Args:
     board: ゲームボード
     start-y: 開始位置のy座標
     start-x: 開始位置のx座標
   
   Returns:
     同色で隣接するぷよの座標リスト [[y x] [y x] ...]"
  [board start-y start-x]
  (let [target-color (get-in board [start-y start-x])
        visited (atom #{})
        result (atom [])]
    (when (and target-color (not= 0 target-color))
      (letfn [(bfs [queue]
                (when-not (empty? queue)
                  (let [[y x] (first queue)
                        remaining (rest queue)]
                    (when-not (contains? @visited [y x])
                      (swap! visited conj [y x])
                      (swap! result conj [y x])
                      (let [neighbors (filter (fn [[ny nx]]
                                                (and (>= ny 0) (< ny board-height)
                                                     (>= nx 0) (< nx board-width)
                                                     (= target-color (get-in board [ny nx]))
                                                     (not (contains? @visited [ny nx]))))
                                              [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])]
                        (bfs (concat remaining neighbors)))))))]
        (bfs [[start-y start-x]])))
    @result))

(defn find-erasable-groups
  "ボード上の消去可能なぷよグループを検出
   
   Args:
     board: ゲームボード
   
   Returns:
     消去可能グループのリスト [[[y x] [y x] ...] [[y x] [y x] ...] ...]"
  [board]
  (let [visited (atom #{})
        erasable-groups (atom [])]
    (dotimes [y board-height]
      (dotimes [x board-width]
        (when (and (not= 0 (get-in board [y x]))
                   (not (contains? @visited [y x])))
          (let [group (find-adjacent-puyos board y x)]
            (doseq [pos group]
              (swap! visited conj pos))
            (when (>= (count group) 4)
              (swap! erasable-groups conj group))))))
    @erasable-groups))

(defn erase-puyos
  "ぷよ消去の実行
   
   Args:
     board: ゲームボード
   
   Returns:
     {:board new-board :erased-count count} - 消去後のボードと消去数"
  [board]
  (let [erasable-groups (find-erasable-groups board)
        erase-positions (set (apply concat erasable-groups))
        erased-count (count erase-positions)
        new-board (reduce (fn [b pos]
                            (assoc-in b pos 0))
                          board
                          erase-positions)]
    {:board new-board
     :erased-count erased-count}))

(defn calculate-score
  "スコア計算
   
   Args:
     erased-count: 消去したぷよの数
     chain-count: 連鎖回数
   
   Returns:
     計算されたスコア"
  [erased-count chain-count]
  (let [base-score (* erased-count 10)
        chain-bonus (if (> chain-count 1)
                      (* (dec chain-count) 50)
                      0)]
    (+ base-score chain-bonus)))

(defn calculate-base-score
  "消去ぷよ数に基づくベーススコア計算
   
   Args:
     erased-count: 消去したぷよの数
     _chain-count: 連鎖回数（未使用、互換性のため）
     _group-count: グループ数（未使用、互換性のため）
     _color-count: 色数（未使用、互換性のため）
   
   Returns:
     ベーススコア"
  [erased-count _chain-count _group-count _color-count]
  (* erased-count 10))

(defn calculate-chain-multiplier
  "連鎖倍率の計算
   
   Args:
     chain-count: 連鎖回数
   
   Returns:
     連鎖倍率"
  [chain-count]
  (case chain-count
    1 1
    2 8
    3 16
    4 32
    5 64
    6 96
    7 128
    8 160
    9 192
    10 224
    256)) ; 11連鎖以上は固定

(defn calculate-group-bonus
  "同時消し倍率の計算
   
   Args:
     group-count: 同時に消去されたグループ数
   
   Returns:
     グループボーナス倍率"
  [group-count]
  (case group-count
    1 1
    2 3
    3 6
    4 12
    5 24
    (* group-count 24))) ; 6グループ以上

(defn calculate-color-bonus
  "色数ボーナスの計算
   
   Args:
     color-count: 消去に関わった色の数
   
   Returns:
     色ボーナス倍率"
  [color-count]
  (case color-count
    1 1
    2 3
    3 6
    4 12
    5 24
    (* color-count 24))) ; 6色以上

(defn calculate-total-score
  "総合スコア計算
   
   Args:
     erased-count: 消去したぷよの数
     chain-count: 連鎖回数
     group-count: 同時消去グループ数
     color-count: 消去に関わった色数
   
   Returns:
     計算された総合スコア"
  [erased-count chain-count group-count color-count]
  (let [base-score (calculate-base-score erased-count chain-count group-count color-count)
        chain-mult (calculate-chain-multiplier chain-count)
        group-mult (if (= group-count 1) 0 (calculate-group-bonus group-count))
        color-mult (if (= color-count 1) 0 (calculate-color-bonus color-count))
        ; 連鎖のみまたはボーナスがある場合の計算
        total-mult (max 1 (+ chain-mult group-mult color-mult))]
    (* base-score total-mult)))

(defn is-perfect-clear?
  "ボードが完全に空（全消し）かどうかを判定
   
   Args:
     board: ゲームボード
   
   Returns:
     ボードが空ならtrue、そうでなければfalse"
  [board]
  (every? #(every? zero? %) board))

(defn calculate-perfect-clear-bonus
  "全消しボーナススコアを計算
   
   Returns:
     全消しボーナススコア（8500点）"
  []
  8500)

(defn execute-chain
  "連鎖の実行
   
   Args:
     board: ゲームボード
   
   Returns:
     {:board new-board :chain-count count :total-score score} - 連鎖実行結果"
  [board]
  (loop [current-board board
         chain-count 0
         total-score 0]
    (let [erase-result (erase-puyos current-board)]
      (if (> (:erased-count erase-result) 0)
        ;; 消去があった場合：落下処理して再帰
        (let [new-chain-count (inc chain-count)
              chain-score (calculate-score (:erased-count erase-result) new-chain-count)
              dropped-board (drop-floating-puyos (:board erase-result))]
          (recur dropped-board new-chain-count (+ total-score chain-score)))
        ;; 消去がなかった場合：連鎖終了
        {:board current-board
         :chain-count chain-count
         :total-score total-score}))))

(defn execute-perfect-clear
  "全消し処理の実行
   
   Args:
     board: ゲームボード
   
   Returns:
     {:is-perfect-clear boolean :perfect-clear-bonus score :total-score score}"
  [board]
  (let [chain-result (execute-chain board)
        final-board (:board chain-result)
        is-perfect (is-perfect-clear? final-board)
        perfect-bonus (if is-perfect (calculate-perfect-clear-bonus) 0)
        total-score (+ (:total-score chain-result) perfect-bonus)]
    {:is-perfect-clear is-perfect
     :perfect-clear-bonus perfect-bonus
     :total-score total-score
     :board final-board
     :chain-count (:chain-count chain-result)}))

(defn create-empty-board
  "空のゲームボードを作成"
  []
  (vec (repeat board-height (vec (repeat board-width 0)))))

(defn place-puyo-pair!
  "組ぷよをボードに配置する"
  [puyo-pair]
  (let [positions (get-puyo-pair-positions
                   (get-in puyo-pair [:puyo1 :x])
                   (get-in puyo-pair [:puyo1 :y])
                   (:rotation puyo-pair))
        puyo1-pos (first positions)
        puyo2-pos (second positions)
        color1 (get-in puyo-pair [:puyo1 :color])
        color2 (get-in puyo-pair [:puyo2 :color])]
    (swap! game-state
           update :board
           #(-> %
                (assoc-in [(:y puyo1-pos) (:x puyo1-pos)] color1)
                (assoc-in [(:y puyo2-pos) (:x puyo2-pos)] color2)))))

(defn process-line-clear!
  "連鎖処理を実行し、結果をゲーム状態に反映"
  []
  (let [board (:board @game-state)
        chain-result (execute-chain board)]
    (swap! game-state merge
           {:board (:board chain-result)
            :score (+ (:score @game-state) (:total-score chain-result))
            :chain-count (:chain-count chain-result)})
    (update-all-game-info!)))

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

(defn init-canvas
  "Canvas初期化
   
   Args:
     canvas-id: CanvasのID
   
   Returns:
     初期化成功ならtrue、失敗ならfalse"
  [canvas-id]
  (try
    (if (exists? js/document)
      (when-let [canvas (.getElementById js/document canvas-id)]
        (reset! ctx (.getContext canvas "2d"))
        true)
      ;; テスト環境ではモック
      true)
    (catch js/Error _
      false)))

(defn get-puyo-color
  "ぷよの色番号に対応するカラーコードを取得
   
   Args:
     color-num: 色番号
   
   Returns:
     カラーコード文字列"
  [color-num]
  (get colors color-num "#ffffff"))

(defn render-board
  "ボード描画処理
   
   Args:
     board: ゲームボード
   
   Returns:
     nil"
  [board]
  (when @ctx
    (doseq [y (range board-height)
            x (range board-width)]
      (let [cell-value (get-in board [y x])
            color (get-puyo-color cell-value)]
        (draw-cell x y color))))
  nil)

(defn render-puyo-pair
  "組ぷよ描画処理
   
   Args:
     puyo-pair: 組ぷよデータ
   
   Returns:
     nil"
  [puyo-pair]
  (when @ctx
    (let [{:keys [puyo1 puyo2]} puyo-pair
          color1 (get-puyo-color (:color puyo1))
          color2 (get-puyo-color (:color puyo2))]
      (draw-cell (:x puyo1) (:y puyo1) color1)
      (draw-cell (:x puyo2) (:y puyo2) color2)))
  nil)

(defn update-game-display
  "ゲーム状態表示更新
   
   Args:
     game-state: ゲーム状態
   
   Returns:
     nil"
  [game-state]
  (when (exists? js/document)
    (when-let [score-elem (.getElementById js/document "score")]
      (set! (.-textContent score-elem) (str (:score game-state))))
    (when-let [level-elem (.getElementById js/document "level")]
      (set! (.-textContent level-elem) (str (:level game-state))))
    (when-let [chain-elem (.getElementById js/document "chain")]
      (set! (.-textContent chain-elem) (str (:chain-count game-state 0)))))
  nil)

;; =============================================================================
;; T016: ゲーム情報の表示
;; =============================================================================

;; 連鎖数管理関数
(defn reset-chain-count!
  "連鎖数を0にリセット"
  []
  (swap! game-state assoc :chain-count 0))

(defn increment-chain-count!
  "連鎖数を1増加"
  []
  (swap! game-state update :chain-count inc))

(defn set-chain-count!
  "連鎖数を指定値に設定"
  [count]
  (swap! game-state assoc :chain-count count))

;; ゲーム時間管理関数
(defn reset-game-time!
  "ゲーム時間を0にリセット"
  []
  (swap! game-state assoc :game-time 0))

(defn update-game-time!
  "ゲーム時間を更新（秒単位）"
  [seconds]
  (swap! game-state assoc :game-time seconds))

;; 時間フォーマット関数
(defn format-game-time
  "ゲーム時間を M:SS 形式でフォーマット"
  [seconds]
  (let [minutes (quot seconds 60)
        remaining-seconds (mod seconds 60)]
    (str minutes ":" (if (< remaining-seconds 10)
                       (str "0" remaining-seconds)
                       (str remaining-seconds)))))

;; 個別表示更新関数
(defn update-score-display!
  "スコア表示を更新"
  []
  (when (and (exists? js/document)
             (.-getElementById js/document))
    (when-let [score-elem (.getElementById js/document "score")]
      (set! (.-textContent score-elem) (str (:score @game-state))))))

(defn update-chain-display!
  "連鎖数表示を更新"
  []
  (when (and (exists? js/document)
             (.-getElementById js/document))
    (when-let [chain-elem (.getElementById js/document "chain")]
      (set! (.-textContent chain-elem) (str (:chain-count @game-state))))))

(defn update-time-display!
  "時間表示を更新"
  []
  (when (and (exists? js/document)
             (.-getElementById js/document))
    (when-let [time-elem (.getElementById js/document "time")]
      (set! (.-textContent time-elem) (format-game-time (:game-time @game-state))))))

;; 統合表示更新関数
(defn update-all-game-info!
  "すべてのゲーム情報表示を更新"
  []
  (update-score-display!)
  (update-chain-display!)
  (update-time-display!)
  ;; レベル表示も更新
  (when (and (exists? js/document)
             (.-getElementById js/document))
    (when-let [level-elem (.getElementById js/document "level")]
      (set! (.-textContent level-elem) (str (:level @game-state))))))

;; ゲームタイマー管理
(defn start-game-timer!
  "ゲームタイマーを開始（1秒ごとに時間を更新）"
  []
  (when @game-timer
    (js/clearInterval @game-timer))
  (reset! game-timer
          (js/setInterval
           (fn []
             (when (:game-running @game-state)
               (update-game-time! (inc (:game-time @game-state)))
               (update-time-display!)))
           1000)))

(defn stop-game-timer!
  "ゲームタイマーを停止"
  []
  (when @game-timer
    (js/clearInterval @game-timer)
    (reset! game-timer nil)))

(defn start-drop-timer!
  "ぷよ落下タイマーを開始（500msごとにぷよを1マス下に落下）"
  []
  (when @drop-timer
    (js/clearInterval @drop-timer))
  (reset! drop-timer
          (js/setInterval
           (fn []
             (when (:game-running @game-state)
               (process-auto-drop!)))
           500)))

(defn stop-drop-timer!
  "ぷよ落下タイマーを停止"
  []
  (when @drop-timer
    (js/clearInterval @drop-timer)
    (reset! drop-timer nil)))

(defn process-auto-drop!
  "自動落下処理：現在のぷよを1マス下に落下させる"
  []
  (when-let [current-piece (:current-piece @game-state)]
    (let [board (:board @game-state)
          dropped-piece (drop-puyo-pair-one-step current-piece board)]
      (if (= dropped-piece current-piece)
        ;; 落下できない場合はぷよを配置して新しいぷよを生成
        (do
          (place-puyo-pair! current-piece)
          (process-line-clear!)
          (let [new-piece (spawn-new-puyo-pair)]
            (if (can-place-puyo-pair? new-piece (:board @game-state))
              (swap! game-state assoc :current-piece new-piece)
              ;; 新しいぷよが配置できない場合はゲームオーバー
              (do
                (process-game-over!)
                (stop-drop-timer!)))))
        ;; 落下できる場合は位置を更新
        (do
          (swap! game-state assoc :current-piece dropped-piece)
          (render-game))))))

(defn render-game
  "ゲーム画面を描画"
  []
  (when @ctx
    ;; 画面クリア
    (set! (.-fillStyle @ctx) "#f0f0f0")
    (.fillRect @ctx 0 0 (* board-width cell-size) (* board-height cell-size))

    ;; ボード描画
    (draw-board)

    ;; 現在の組ぷよ描画
    (when-let [current-piece (:current-piece @game-state)]
      (render-puyo-pair current-piece))

    ;; UI更新
    (update-all-game-info!)))

(defn start-game
  "ゲームを開始"
  []
  (init-game-state!)
  (reset-chain-count!)
  (reset-game-time!)
  (swap! game-state assoc
         :game-running true
         :current-piece (spawn-new-puyo-pair))
  (update-all-game-info!)
  (start-game-timer!)
  (start-drop-timer!)
  (render-game)
  (js/console.log "ゲーム開始!"))

(defn reset-game
  "ゲームをリセット"
  []
  (stop-game-timer!)
  (stop-drop-timer!)
  (init-game-state!)
  (reset-chain-count!)
  (reset-game-time!)
  (update-all-game-info!)
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
  (render-game))

;; =============================================================================
;; T017: キーボード入力処理
;; =============================================================================

;; 補助関数群（キーボード処理用）
(defn drop-puyo-pair-one-step
  "組ぷよを1マス下に落下"
  [puyo-pair board]
  (let [moved-down (-> puyo-pair
                       (update-in [:puyo1 :y] inc)
                       (update-in [:puyo2 :y] inc))]
    (if (can-place-puyo-pair? moved-down board)
      moved-down
      puyo-pair)))

(defn hard-drop-puyo-pair
  "組ぷよをハードドロップ（最下段まで一気に落下）"
  [puyo-pair board]
  (loop [current-piece puyo-pair]
    (let [dropped-piece (drop-puyo-pair-one-step current-piece board)]
      (if (= dropped-piece current-piece)
        current-piece
        (recur dropped-piece)))))

;; 移動処理関数群
(defn process-left-movement!
  "左移動処理"
  []
  (let [current-piece (:current-piece @game-state)
        board (:board @game-state)]
    (when current-piece
      (let [moved-piece (move-puyo-pair-left current-piece board)]
        (when (not= moved-piece current-piece)
          (swap! game-state assoc :current-piece moved-piece)
          (render-game)
          {:result :moved :direction :left})))))

(defn process-right-movement!
  "右移動処理"
  []
  (let [current-piece (:current-piece @game-state)
        board (:board @game-state)]
    (when current-piece
      (let [moved-piece (move-puyo-pair-right current-piece board)]
        (when (not= moved-piece current-piece)
          (swap! game-state assoc :current-piece moved-piece)
          (render-game)
          {:result :moved :direction :right})))))

(defn process-rotation!
  "回転処理"
  []
  (let [current-piece (:current-piece @game-state)
        board (:board @game-state)]
    (when current-piece
      (let [rotated-piece (rotate-puyo-pair current-piece)]
        (when (can-place-puyo-pair? rotated-piece board)
          (swap! game-state assoc :current-piece rotated-piece)
          (render-game)
          {:result :rotated :new-rotation (:rotation rotated-piece)})))))

(defn process-soft-drop!
  "高速落下処理"
  []
  (let [current-piece (:current-piece @game-state)
        board (:board @game-state)]
    (when current-piece
      (let [dropped-piece (drop-puyo-pair-one-step current-piece board)]
        (if (not= dropped-piece current-piece)
          (do
            (swap! game-state assoc :current-piece dropped-piece)
            (render-game)
            {:result :soft-dropped :new-y (get-in dropped-piece [:puyo1 :y])})
          {:result :bottom-reached})))))

(defn process-hard-drop!
  "ハードドロップ処理"
  []
  (let [current-piece (:current-piece @game-state)
        board (:board @game-state)]
    (when current-piece
      (let [final-piece (hard-drop-puyo-pair current-piece board)]
        (swap! game-state assoc :current-piece final-piece)
        (render-game)
        {:result :hard-dropped :final-y (get-in final-piece [:puyo1 :y])}))))

;; キーボード入力ハンドラ関数
(defn handle-key-input
  "キーボード入力を処理してゲーム状態を更新"
  [key]
  (when (and (:game-running @game-state)
             (:current-piece @game-state))
    (case key
      "ArrowLeft" (process-left-movement!)
      "ArrowRight" (process-right-movement!)
      "ArrowUp" (process-rotation!)
      "ArrowDown" (process-soft-drop!)
      " " (process-hard-drop!)
      nil)))

;; キーボードイベントハンドラの更新
(defn update-keyboard-handler!
  "キーボードイベントハンドラを新しい処理ロジックで更新"
  []
  (.addEventListener js/document "keydown"
                     (fn [event]
                       (when (:game-running @game-state)
                         (let [key (.-key event)]
                           (handle-key-input key))))))

;; T019: ゲーム初期化関数群
(defn reset-game-state!
  "ゲーム状態を初期値にリセット"
  []
  (swap! game-state assoc
         :score 0
         :level 1
         :chain-count 0
         :game-time 0
         :game-running false
         :current-piece nil))

(defn initialize-game-board!
  "ゲームボードを空の状態で初期化"
  []
  (swap! game-state assoc :board (create-empty-board)))

(defn spawn-initial-puyo-pair!
  "初回の組ぷよを生成してゲーム状態に設定"
  []
  (let [initial-x 3  ; ボード中央
        initial-y 0  ; 上部
        initial-pair (generate-random-puyo-pair initial-x initial-y)]
    (swap! game-state assoc :current-piece initial-pair)))

(defn start-new-game!
  "新しいゲームを開始"
  []
  (reset-game-state!)
  (initialize-game-board!)
  (spawn-initial-puyo-pair!)
  (swap! game-state assoc :game-running true))

(defn init-game!
  "ゲーム全体の初期化（統合関数）"
  []
  (start-new-game!))

;; T020: ゲーム終了判定関数群
(defn is-game-over?
  "ゲームオーバー判定：y=0,1の危険ラインにぷよがあるかチェック"
  []
  (let [board (:board @game-state)]
    (boolean
     (some (fn [y]
             (some (fn [x]
                     (pos? (get-in board [y x])))
                   (range board-width)))
           [0 1]))))

(defn process-game-over!
  "ゲームオーバー時の処理"
  []
  (swap! game-state assoc :game-over true :game-running false))

(defn check-and-handle-game-over!
  "ゲームオーバーをチェックし、必要に応じて処理を実行"
  []
  (let [game-over (is-game-over?)]
    (when game-over
      (process-game-over!))
    game-over))
