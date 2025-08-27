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
  "組ぷよをボードに配置する（詳細ログ付き）"
  [puyo-pair]
  (js/console.log "=== place-puyo-pair! 実行開始 ===")
  (js/console.log "配置対象ぷよペア:"
                  "puyo1(" (get-in puyo-pair [:puyo1 :x]) "," (get-in puyo-pair [:puyo1 :y]) ")"
                  "puyo2(" (get-in puyo-pair [:puyo2 :x]) "," (get-in puyo-pair [:puyo2 :y]) ")"
                  "rotation:" (:rotation puyo-pair))
  (let [positions (get-puyo-pair-positions
                   (get-in puyo-pair [:puyo1 :x])
                   (get-in puyo-pair [:puyo1 :y])
                   (:rotation puyo-pair))
        puyo1-pos (first positions)
        puyo2-pos (second positions)
        color1 (get-in puyo-pair [:puyo1 :color])
        color2 (get-in puyo-pair [:puyo2 :color])]
    (js/console.log "配置座標:"
                    "puyo1-pos(" (:x puyo1-pos) "," (:y puyo1-pos) ")"
                    "puyo2-pos(" (:x puyo2-pos) "," (:y puyo2-pos) ")")
    (js/console.log "配置色:" "color1=" color1 "color2=" color2)
    (swap! game-state
           update :board
           #(-> %
                (assoc-in [(:y puyo1-pos) (:x puyo1-pos)] color1)
                (assoc-in [(:y puyo2-pos) (:x puyo2-pos)] color2)
                (drop-floating-puyos)))
    (js/console.log "✓ ボード配置完了")
    (js/console.log "=== place-puyo-pair! 実行終了 ===")))

(defn process-line-clear!
  "連鎖処理を実行し、結果をゲーム状態に反映（詳細ログ付き）"
  []
  (js/console.log "=== process-line-clear! 実行開始 ===")
  (let [board (:board @game-state)
        chain-result (execute-chain board)]
    (js/console.log "連鎖結果:" "chains=" (:chain-count chain-result)
                    "score=" (:total-score chain-result))
    (swap! game-state merge
           {:board (:board chain-result)
            :score (+ (:score @game-state) (:total-score chain-result))
            :chain-count (:chain-count chain-result)})
    (js/console.log "✓ ゲーム状態更新完了")
    ;; TODO: update-all-game-info!の呼び出しを一時的にコメントアウト
    ;; (update-all-game-info!)
    (js/console.log "=== process-line-clear! 実行終了 ===")))

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
  "自動落下処理：現在のぷよを1マス下に落下させる（アトミック操作）"
  []
  (let [result (atom nil)]
    (swap! game-state
           (fn [state]
             (if-let [current-piece (:current-piece state)]
               (let [board (:board state)
                     dropped-piece (drop-puyo-pair-one-step current-piece board)]
                 (if (= dropped-piece current-piece)
                   ;; 落下できない場合はぷよを配置して新しいぷよを生成
                   (do
                     (reset! result :piece-placed)
                     state) ; 配置処理は別途実行
                   ;; 落下できる場合は位置を更新
                   (do
                     (reset! result :dropped)
                     (assoc state :current-piece dropped-piece))))
               state)))
    ;; 結果に応じて後続処理を実行
    (case @result
      :piece-placed (do
                      (place-puyo-pair! (:current-piece @game-state))
                      (process-line-clear!)
                      (let [new-piece (spawn-new-puyo-pair)]
                        (if (can-place-puyo-pair? new-piece (:board @game-state))
                          (swap! game-state assoc :current-piece new-piece)
                          (do
                            (process-game-over!)
                            (stop-drop-timer!)))))
      :dropped (render-game)
      nil)))

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
  (js/console.log "🚨🚨🚨 start-game 関数が実行されました！🚨🚨🚨")
  (js/console.log "=== start-game 実行開始 ===")
  (js/console.log "📍 start-game 呼び出し元を特定中...")

  ;; スタックトレースを出力して呼び出し元を特定
  (try
    (throw (js/Error. "Stack trace for debugging"))
    (catch js/Error e
      (js/console.log "📍 Stack trace:")
      (js/console.log (.-stack e))))

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
  (js/console.log "ゲーム開始!")
  (js/console.log "=== start-game 実行終了 ==="))

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

;; イベントリスナー登録済みフラグ
(defonce event-listeners-setup (atom false))

(defn setup-event-listeners
  "イベントリスナーを設定（重複登録防止付き）"
  []
  (js/console.log "=== イベントリスナー設定開始 ===")
  (if @event-listeners-setup
    (js/console.log "✓ イベントリスナーは既に設定済み - スキップ")
    (do
      (js/console.log "イベントリスナーを新規設定中...")

      ;; ゲーム開始ボタン
      (when-let [start-btn (.getElementById js/document "start-button")]
        (js/console.log "ゲーム開始ボタンのイベントリスナー設定")
        (.addEventListener start-btn "click"
                           (fn [event]
                             (js/console.log "🎮 ゲーム開始ボタンがクリックされました")
                             (js/console.log "現在のゲーム実行状態:" (:game-running @game-state))
                             (if (:game-running @game-state)
                               (js/console.log "⚠️ ゲーム実行中のため、start-gameをスキップ")
                               (do
                                 (js/console.log "✅ ゲーム停止中のため、start-gameを実行")
                                 (start-game))))))

      ;; リセットボタン
      (when-let [reset-btn (.getElementById js/document "reset-button")]
        (js/console.log "リセットボタンのイベントリスナー設定")
        (.addEventListener reset-btn "click" reset-game))

      ;; キーボードイベント
      (js/console.log "キーボードイベントリスナー設定")
      (.addEventListener js/document "keydown"
                         (fn [event]
                           (js/console.log "🎹 キーボードイベント発生 - Key:" (.-key event) "Target:" (.-tagName (.-target event)))
                           (when (:game-running @game-state)
                             (let [key (.-key event)]
                               ;; スペースキーがボタンを誤って発火させないように preventDefault
                               (when (= key " ")
                                 (js/console.log "🚫 スペースキーのデフォルト動作を防止")
                                 (.preventDefault event))
                               (handle-key-input key)))))

      (reset! event-listeners-setup true)
      (js/console.log "✓ イベントリスナー設定完了")))
  (js/console.log "=== イベントリスナー設定終了 ==="))

;; ゲーム初期化フラグ
(defonce app-initialized (atom false))

(defn init
  "アプリケーション初期化"
  []
  (js/console.log "=== アプリケーション初期化開始 ===")
  (if @app-initialized
    (do
      (js/console.log "✗ アプリケーションは既に初期化済みです - スキップ")
      (js/console.log "=== アプリケーション初期化終了（スキップ） ==="))
    (do
      (js/console.log "Puyo Puyo Game 初期化中...")

      ;; Canvas要素の取得
      (when-let [canvas-elem (.getElementById js/document "game-board")]
        (js/console.log "Canvas要素取得成功")
        (reset! canvas canvas-elem)
        (reset! ctx (.getContext canvas-elem "2d")))

      ;; ゲーム状態初期化
      (js/console.log "ゲーム状態初期化実行")
      (init-game-state!)

      ;; イベントリスナー設定
      (js/console.log "イベントリスナー設定実行")
      (setup-event-listeners)

      ;; 初期描画
      (js/console.log "初期描画実行")
      (render-game)

      ;; 初期化完了フラグを設定
      (reset! app-initialized true)
      (js/console.log "初期化完了")
      (js/console.log "=== アプリケーション初期化終了 ==="))))

;; DOMContentLoadedで自動初期化
(when (exists? js/document)
  (js/console.log "=== DOMContentLoadedイベントリスナー設定 ===")
  (.addEventListener js/document "DOMContentLoaded"
                     (fn [e]
                       (js/console.log "✓ DOMContentLoadedイベント発火 - init関数呼び出し")
                       (init)))

  ;; グローバルエラーハンドラー追加
  (.addEventListener js/window "error"
                     (fn [e]
                       (js/console.error "✗✗✗ グローバルエラー検出 ✗✗✗")
                       (js/console.error "Error:" e)
                       (js/console.error "Message:" (.-message e))
                       (js/console.error "Filename:" (.-filename e))
                       (js/console.error "Line:" (.-lineno e))))

  ;; Unhandled Promise Rejectionハンドラー追加  
  (.addEventListener js/window "unhandledrejection"
                     (fn [e]
                       (js/console.error "✗✗✗ Unhandled Promise Rejection 検出 ✗✗✗")
                       (js/console.error "Reason:" (.-reason e)))))

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
  "組ぷよをハードドロップ（最下段まで一気に落下）詳細ログ付き"
  [puyo-pair board]
  (js/console.log "=== hard-drop-puyo-pair 実行開始 ===")
  (js/console.log "開始位置:"
                  "puyo1(" (get-in puyo-pair [:puyo1 :x]) "," (get-in puyo-pair [:puyo1 :y]) ")"
                  "puyo2(" (get-in puyo-pair [:puyo2 :x]) "," (get-in puyo-pair [:puyo2 :y]) ")")
  (loop [current-piece puyo-pair
         step-count 0]
    (js/console.log (str "ハードドロップ ステップ " step-count ":")
                    "puyo1(" (get-in current-piece [:puyo1 :x]) "," (get-in current-piece [:puyo1 :y]) ")"
                    "puyo2(" (get-in current-piece [:puyo2 :x]) "," (get-in current-piece [:puyo2 :y]) ")")
    (let [dropped-piece (drop-puyo-pair-one-step current-piece board)]
      (if (= dropped-piece current-piece)
        (do
          (js/console.log "✓ ハードドロップ完了 - 最終位置:"
                          "puyo1(" (get-in current-piece [:puyo1 :x]) "," (get-in current-piece [:puyo1 :y]) ")"
                          "puyo2(" (get-in current-piece [:puyo2 :x]) "," (get-in current-piece [:puyo2 :y]) ")")
          (js/console.log "=== hard-drop-puyo-pair 実行終了 ===")
          current-piece)
        (recur dropped-piece (inc step-count))))))

;; 移動処理関数群
(defn process-left-movement!
  "左移動処理（アトミック操作 + 詳細ログ）"
  []
  (js/console.log "=== 左移動処理開始 ===")
  (let [result (atom nil)
        start-time (js/Date.now)]
    (swap! game-state
           (fn [state]
             (let [current-piece (:current-piece state)
                   board (:board state)]
               (if current-piece
                 (do
                   (js/console.log "左移動前のピース位置:"
                                   "puyo1(" (get-in current-piece [:puyo1 :x]) "," (get-in current-piece [:puyo1 :y]) ")"
                                   "puyo2(" (get-in current-piece [:puyo2 :x]) "," (get-in current-piece [:puyo2 :y]) ")")
                   (let [moved-piece (move-puyo-pair-left current-piece board)]
                     (if (not= moved-piece current-piece)
                       (do
                         (js/console.log "左移動後のピース位置:"
                                         "puyo1(" (get-in moved-piece [:puyo1 :x]) "," (get-in moved-piece [:puyo1 :y]) ")"
                                         "puyo2(" (get-in moved-piece [:puyo2 :x]) "," (get-in moved-piece [:puyo2 :y]) ")")
                         (js/console.log "✓ 左移動成功")
                         (reset! result {:result :moved :direction :left})
                         (assoc state :current-piece moved-piece))
                       (do
                         (js/console.log "✗ 左移動できません")
                         (reset! result {:result :failed :reason "cannot-move"})
                         state))))
                 (do
                   (js/console.log "✗ 左移動失敗: 現在のピースがありません")
                   (reset! result {:result :failed :reason "no-piece"})
                   state)))))
    (let [end-time (js/Date.now)
          duration (- end-time start-time)]
      (js/console.log "左移動処理時間:" duration "ms")
      (when (= (:result @result) :moved)
        (js/console.log "描画実行")
        (render-game))
      (js/console.log "=== 左移動処理終了 ===")
      @result)))

(defn process-right-movement!
  "右移動処理（アトミック操作 + 詳細ログ）"
  []
  (js/console.log "=== 右移動処理開始 ===")
  (let [result (atom nil)
        start-time (js/Date.now)]
    (swap! game-state
           (fn [state]
             (let [current-piece (:current-piece state)
                   board (:board state)]
               (if current-piece
                 (do
                   (js/console.log "右移動前のピース位置:"
                                   "puyo1(" (get-in current-piece [:puyo1 :x]) "," (get-in current-piece [:puyo1 :y]) ")"
                                   "puyo2(" (get-in current-piece [:puyo2 :x]) "," (get-in current-piece [:puyo2 :y]) ")")
                   (let [moved-piece (move-puyo-pair-right current-piece board)]
                     (if (not= moved-piece current-piece)
                       (do
                         (js/console.log "右移動後のピース位置:"
                                         "puyo1(" (get-in moved-piece [:puyo1 :x]) "," (get-in moved-piece [:puyo1 :y]) ")"
                                         "puyo2(" (get-in moved-piece [:puyo2 :x]) "," (get-in moved-piece [:puyo2 :y]) ")")
                         (js/console.log "✓ 右移動成功")
                         (reset! result {:result :moved :direction :right})
                         (assoc state :current-piece moved-piece))
                       (do
                         (js/console.log "✗ 右移動できません")
                         (reset! result {:result :failed :reason "cannot-move"})
                         state))))
                 (do
                   (js/console.log "✗ 右移動失敗: 現在のピースがありません")
                   (reset! result {:result :failed :reason "no-piece"})
                   state)))))
    (let [end-time (js/Date.now)
          duration (- end-time start-time)]
      (js/console.log "右移動処理時間:" duration "ms")
      (when (= (:result @result) :moved)
        (js/console.log "描画実行")
        (render-game))
      (js/console.log "=== 右移動処理終了 ===")
      @result)))

(defn process-rotation!
  "回転処理（アトミック操作 + 二重実行防止）"
  []
  (js/console.log "=== 回転処理開始 ===")
  ;; スワップ関数を使用してアトミックに状態を更新
  (let [result (atom nil)
        start-time (js/Date.now)]
    (swap! game-state
           (fn [state]
             (let [current-piece (:current-piece state)
                   board (:board state)]
               (if current-piece
                 (do
                   (js/console.log "回転前の現在のピース:" (pr-str current-piece))
                   (js/console.log "回転前の回転状態:" (:rotation current-piece))
                   (js/console.log "回転前puyo1位置:" (get-in current-piece [:puyo1 :x]) (get-in current-piece [:puyo1 :y]))
                   (js/console.log "回転前puyo2位置:" (get-in current-piece [:puyo2 :x]) (get-in current-piece [:puyo2 :y]))
                   (let [rotated-piece (rotate-puyo-pair current-piece)]
                     (js/console.log "回転計算後のピース:" (pr-str rotated-piece))
                     (js/console.log "回転計算後の状態:" (:rotation rotated-piece))
                     (js/console.log "回転計算後puyo1位置:" (get-in rotated-piece [:puyo1 :x]) (get-in rotated-piece [:puyo1 :y]))
                     (js/console.log "回転計算後puyo2位置:" (get-in rotated-piece [:puyo2 :x]) (get-in rotated-piece [:puyo2 :y]))
                     (if (can-place-puyo-pair? rotated-piece board)
                       (do
                         (js/console.log "✓ 回転成功 - 状態更新実行")
                         (reset! result {:result :rotated :new-rotation (:rotation rotated-piece)})
                         (assoc state :current-piece rotated-piece))
                       (do
                         (js/console.log "✗ 回転失敗: 配置できません")
                         (reset! result {:result :failed :reason "cannot-place"})
                         state))))
                 (do
                   (js/console.log "✗ 回転失敗: 現在のピースがありません")
                   (reset! result {:result :failed :reason "no-piece"})
                   state)))))
    ;; 状態更新後に描画を実行
    (let [end-time (js/Date.now)
          duration (- end-time start-time)]
      (js/console.log "回転処理時間:" duration "ms")
      (when (= (:result @result) :rotated)
        (js/console.log "描画実行")
        (render-game))
      (js/console.log "=== 回転処理終了 ===")
      @result)))

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
  "ハードドロップ処理（詳細ログ付き + ぷよ固定処理）"
  []
  (js/console.log "=== ハードドロップ処理開始 ===")

  ;; ハードドロップ中は他のタイマーを一時停止
  (js/console.log "⏸️ ハードドロップ中 - 他のタイマーを一時停止")
  (stop-drop-timer!)

  (try
    (let [current-piece (:current-piece @game-state)
          board (:board @game-state)]
      (if current-piece
        (do
          (js/console.log "ハードドロップ前のピース位置:"
                          "puyo1(" (get-in current-piece [:puyo1 :x]) "," (get-in current-piece [:puyo1 :y]) ")"
                          "puyo2(" (get-in current-piece [:puyo2 :x]) "," (get-in current-piece [:puyo2 :y]) ")")
          (let [final-piece (hard-drop-puyo-pair current-piece board)]
            (js/console.log "ハードドロップ後のピース位置:"
                            "puyo1(" (get-in final-piece [:puyo1 :x]) "," (get-in final-piece [:puyo1 :y]) ")"
                            "puyo2(" (get-in final-piece [:puyo2 :x]) "," (get-in final-piece [:puyo2 :y]) ")")
            (js/console.log "✓ ハードドロップ成功 - ぷよをボードに固定します")

            ;; 現在のピースをクリア
            (swap! game-state assoc :current-piece nil)
            (js/console.log "現在のピースをクリアしました")

            ;; ぷよをボードに固定
            (place-puyo-pair! final-piece)
            (process-line-clear!)

            ;; 新しいぷよを生成（ゲームオーバーチェック改善）
            (let [new-piece (spawn-new-puyo-pair)]
              (js/console.log "新しいぷよペア生成位置:"
                              "puyo1(" (get-in new-piece [:puyo1 :x]) "," (get-in new-piece [:puyo1 :y]) ")"
                              "puyo2(" (get-in new-piece [:puyo2 :x]) "," (get-in new-piece [:puyo2 :y]) ")")
              ;; 新しいぷよが配置可能かチェック（危険ライン判定ではなく配置可能性のみ）
              (if (can-place-puyo-pair? new-piece (:board @game-state))
                (do
                  (swap! game-state assoc :current-piece new-piece)
                  (js/console.log "✓ 新しいぷよペア生成成功"))
                (do
                  (js/console.log "✗ ゲームオーバー: 新しいぷよを配置できません")
                  (js/console.log "ボード状態（上部2行）:")
                  (doseq [y [0 1]]
                    (js/console.log (str "y=" y ": " (vec (for [x (range board-width)] (get-in (:board @game-state) [y x]))))))
                  (process-game-over!)
                  (stop-drop-timer!))))

            ;; ハードドロップ完了後にタイマーを再開
            (js/console.log "▶️ ハードドロップ完了 - タイマーを再開")
            (start-drop-timer!)

            (render-game)
            (js/console.log "=== ハードドロップ処理終了 ===")
            {:result :hard-dropped-and-placed :final-y (get-in final-piece [:puyo1 :y])}))
        (do
          (js/console.log "✗ ハードドロップ失敗: 現在のピースがありません")
          (js/console.log "=== ハードドロップ処理終了 ===")
          {:result :failed :reason "no-piece"})))
    (catch js/Error e
      (js/console.error "✗✗✗ ハードドロップ処理中にエラーが発生しました ✗✗✗")
      (js/console.error "エラー詳細:" e)
      (js/console.error "エラーメッセージ:" (.-message e))
      (js/console.error "エラースタック:" (.-stack e))
      ;; エラー時もタイマーを再開
      (start-drop-timer!)
      (js/console.log "=== ハードドロップ処理終了（エラー） ===")
      {:result :error :error e})))

;; キーボード入力ハンドラ関数
;; キー入力のデバウンス制御
(def ^:private last-rotation-time (atom 0))
(def ^:private last-left-move-time (atom 0))
(def ^:private last-right-move-time (atom 0))
(def ^:private last-hard-drop-time (atom 0))
(def ^:private rotation-debounce-ms 200) ; 200ms以内の連続回転を防ぐ
(def ^:private movement-debounce-ms 100) ; 100ms以内の連続移動を防ぐ
(def ^:private hard-drop-debounce-ms 300) ; 300ms以内の連続ハードドロップを防ぐ

(defn handle-key-input
  "キーボード入力を処理してゲーム状態を更新"
  [key]
  (js/console.log "Key:" key "Game running:" (:game-running @game-state) "Current piece:" (some? (:current-piece @game-state)))
  (when (and (:game-running @game-state)
             (:current-piece @game-state))
    (case key
      "ArrowLeft" (let [current-time (js/Date.now)
                        time-since-last-move (- current-time @last-left-move-time)]
                    (js/console.log "左移動キー検出 - 前回からの経過時間:" time-since-last-move "ms")
                    (if (> time-since-last-move movement-debounce-ms)
                      (do
                        (js/console.log "左移動実行 - デバウンス条件OK")
                        (reset! last-left-move-time current-time)
                        (process-left-movement!))
                      (do
                        (js/console.log "左移動スキップ - デバウンス条件NG")
                        {:result :debounced :reason "too-soon"})))
      "ArrowRight" (let [current-time (js/Date.now)
                         time-since-last-move (- current-time @last-right-move-time)]
                     (js/console.log "右移動キー検出 - 前回からの経過時間:" time-since-last-move "ms")
                     (if (> time-since-last-move movement-debounce-ms)
                       (do
                         (js/console.log "右移動実行 - デバウンス条件OK")
                         (reset! last-right-move-time current-time)
                         (process-right-movement!))
                       (do
                         (js/console.log "右移動スキップ - デバウンス条件NG")
                         {:result :debounced :reason "too-soon"})))
      "ArrowUp" (let [current-time (js/Date.now)
                      time-since-last-rotation (- current-time @last-rotation-time)]
                  (js/console.log "回転キー検出 - 前回からの経過時間:" time-since-last-rotation "ms")
                  (if (> time-since-last-rotation rotation-debounce-ms)
                    (do
                      (js/console.log "回転実行 - デバウンス条件OK")
                      (reset! last-rotation-time current-time)
                      (process-rotation!))
                    (do
                      (js/console.log "回転スキップ - デバウンス条件NG")
                      {:result :debounced :reason "too-soon"})))
      "ArrowDown" (process-soft-drop!)
      " " (let [current-time (js/Date.now)
                time-since-last-drop (- current-time @last-hard-drop-time)]
            (js/console.log "ハードドロップキー検出 - 前回からの経過時間:" time-since-last-drop "ms")
            (if (> time-since-last-drop hard-drop-debounce-ms)
              (do
                (js/console.log "ハードドロップ実行 - デバウンス条件OK")
                (reset! last-hard-drop-time current-time)
                (process-hard-drop!))
              (do
                (js/console.log "ハードドロップスキップ - デバウンス条件NG")
                {:result :debounced :reason "too-soon"})))
      nil)))

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
