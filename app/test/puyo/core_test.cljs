(ns puyo.core-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [puyo.core :as core]))

(deftest basic-test
  (testing "基本機能"
    (is (= 8 core/board-width) "ボード幅")
    (is (= 12 core/board-height) "ボード高さ")))

(deftest board-test
  (testing "ボード作成"
    (let [board (core/create-empty-board)]
      (is (vector? board) "ボードはベクター")
      (is (= 12 (count board)) "高さ12"))))

(deftest puyo-creation-test
  (testing "ぷよ作成"
    (let [pair (core/create-puyo-pair 1 2 3 0)]
      (is (map? pair) "組ぷよはマップ")
      (is (= 1 (get-in pair [:puyo1 :color])) "色1")
      (is (= 2 (get-in pair [:puyo2 :color])) "色2"))))

(deftest movement-test
  (testing "移動機能"
    (let [board (core/create-empty-board)
          pair {:puyo1 {:x 3 :y 1 :color 1}
                :puyo2 {:x 3 :y 2 :color 2}
                :rotation 0}]
      (let [moved-left (core/move-puyo-pair-left pair board)]
        (is (= 2 (get-in moved-left [:puyo1 :x])) "左移動"))
      (let [moved-right (core/move-puyo-pair-right pair board)]
        (is (= 4 (get-in moved-right [:puyo1 :x])) "右移動")))))

(deftest rotation-test
  (testing "回転機能"
    (let [pair (core/create-puyo-pair 1 2 3 1)
          rotated (core/rotate-puyo-pair pair)]
      (is (= 1 (:rotation rotated)) "回転状態が1")
      (is (= 3 (get-in rotated [:puyo1 :x])) "puyo1のx座標")
      (is (= 4 (get-in rotated [:puyo2 :x])) "puyo2が右に移動"))))

(deftest boundary-test
  (testing "境界制限"
    (let [board (core/create-empty-board)
          left-pair {:puyo1 {:x 0 :y 1 :color 1}
                     :puyo2 {:x 0 :y 2 :color 2}
                     :rotation 0}
          right-pair {:puyo1 {:x 7 :y 1 :color 1}
                      :puyo2 {:x 7 :y 2 :color 2}
                      :rotation 0}]
      (is (= left-pair (core/move-puyo-pair-left left-pair board)) "左端移動制限")
      (is (= right-pair (core/move-puyo-pair-right right-pair board)) "右端移動制限"))))

(deftest random-generation-test
  (testing "ランダム生成"
    (dotimes [_ 5]
      (let [color (core/generate-random-color)]
        (is (<= 1 color 5) "色の範囲"))
      (let [pair (core/generate-random-puyo-pair 3 0)]
        (is (<= 1 (get-in pair [:puyo1 :color]) 5) "puyo1色範囲")
        (is (<= 1 (get-in pair [:puyo2 :color]) 5) "puyo2色範囲")))))

(deftest downward-movement-test
  (testing "下移動機能"
    (let [board (core/create-empty-board)
          pair {:puyo1 {:x 3 :y 1 :color 1}
                :puyo2 {:x 3 :y 2 :color 2}
                :rotation 0}
          moved-down (core/move-puyo-pair-down pair board)]
      (is (= 2 (get-in moved-down [:puyo1 :y])) "puyo1の下移動")
      (is (= 3 (get-in moved-down [:puyo2 :y])) "puyo2の下移動"))))

(deftest gravity-test
  (testing "重力システム"
    (let [board (core/create-empty-board)
          pair {:puyo1 {:x 3 :y 1 :color 1}
                :puyo2 {:x 3 :y 2 :color 2}
                :rotation 0}]
      (is (true? (core/can-fall? pair board)) "落下可能判定")
      (let [bottom-pair {:puyo1 {:x 3 :y 10 :color 1}
                         :puyo2 {:x 3 :y 11 :color 2}
                         :rotation 0}]
        (is (false? (core/can-fall? bottom-pair board)) "底面落下不可判定")))))

(deftest soft-drop-test
  (testing "ソフトドロップ機能"
    (let [board (core/create-empty-board)
          pair {:puyo1 {:x 3 :y 1 :color 1}
                :puyo2 {:x 3 :y 2 :color 2}
                :rotation 0}
          soft-dropped (core/soft-drop pair board)]
      (is (>= (get-in soft-dropped [:puyo1 :y]) 1) "ソフトドロップ後の位置")
      (is (>= (get-in soft-dropped [:puyo2 :y]) 2) "ソフトドロップ後の位置"))))

(deftest hard-drop-test
  (testing "ハードドロップ機能"
    (let [board (core/create-empty-board)
          pair {:puyo1 {:x 3 :y 1 :color 1}
                :puyo2 {:x 3 :y 2 :color 2}
                :rotation 0}
          hard-dropped (core/hard-drop pair board)]
      (is (= 10 (get-in hard-dropped [:puyo1 :y])) "ハードドロップ底面到達")
      (is (= 11 (get-in hard-dropped [:puyo2 :y])) "ハードドロップ底面到達"))))

(deftest puyo-placement-test
  (testing "ぷよ固定処理"
    (let [board (core/create-empty-board)
          pair {:puyo1 {:x 3 :y 10 :color 1}
                :puyo2 {:x 3 :y 11 :color 2}
                :rotation 0}]
      (is (true? (core/should-fix-puyo? pair board)) "底面で固定判定")
      (let [fixed-board (core/fix-puyo-pair-to-board pair board)]
        (is (= 1 (get-in fixed-board [10 3])) "puyo1がボードに配置")
        (is (= 2 (get-in fixed-board [11 3])) "puyo2がボードに配置")))))

(deftest floating-puyo-test
  (testing "浮いているぷよの落下"
    (let [board (-> (core/create-empty-board)
                    (assoc-in [11 3] 1)  ; 底面にぷよ
                    (assoc-in [9 3] 2))  ; 浮いているぷよ
          dropped-board (core/drop-floating-puyos board)]
      (is (= 0 (get-in dropped-board [9 3])) "元の位置は空")
      (is (= 2 (get-in dropped-board [10 3])) "落下後の位置"))))

(deftest collision-detection-test
  (testing "衝突判定"
    (let [board (-> (core/create-empty-board)
                    (assoc-in [10 3] 1)  ; 既存のぷよ
                    (assoc-in [11 3] 2))
          pair {:puyo1 {:x 3 :y 9 :color 3}
                :puyo2 {:x 3 :y 10 :color 4}
                :rotation 0}]
      (is (true? (core/should-fix-puyo? pair board)) "既存ぷよとの衝突で固定")
      (is (false? (core/can-fall? pair board)) "衝突により落下不可"))))

(deftest should-fix-test
  (testing "固定判定"
    (let [board (core/create-empty-board)
          bottom-pair {:puyo1 {:x 3 :y 10 :color 1}
                       :puyo2 {:x 3 :y 11 :color 2}
                       :rotation 0}]
      (is (true? (core/should-fix-puyo? bottom-pair board)) "底面で固定判定"))))

(deftest adjacent-search-test
  (testing "隣接ぷよ検索"
    (let [board (-> (core/create-empty-board)
                    (assoc-in [10 3] 1)  ; 赤
                    (assoc-in [10 4] 1)  ; 赤（隣接）
                    (assoc-in [9 3] 1)   ; 赤（隣接）
                    (assoc-in [11 3] 2)) ; 青（非隣接）
          adjacent-puyos (core/find-adjacent-puyos board 10 3)]
      (is (= 3 (count adjacent-puyos)) "3つの隣接する赤ぷよ")
      (is (contains? (set adjacent-puyos) [10 3]) "起点を含む")
      (is (contains? (set adjacent-puyos) [10 4]) "右隣を含む")
      (is (contains? (set adjacent-puyos) [9 3]) "上隣を含む")
      (is (not (contains? (set adjacent-puyos) [11 3])) "異なる色は含まない"))))

(deftest group-detection-test
  (testing "消去可能グループの検出"
    (let [board (-> (core/create-empty-board)
                    ;; 4つ連結の赤グループ（消去可能）
                    (assoc-in [10 3] 1) (assoc-in [10 4] 1)
                    (assoc-in [9 3] 1) (assoc-in [9 4] 1)
                    ;; 3つ連結の青グループ（消去不可）
                    (assoc-in [8 1] 2) (assoc-in [8 2] 2)
                    (assoc-in [7 1] 2))
          erasable-groups (core/find-erasable-groups board)]
      (is (= 1 (count erasable-groups)) "1つの消去可能グループ")
      (is (= 4 (count (first erasable-groups))) "4つのぷよからなるグループ")
      (is (= 1 (get-in board [(first (first (first erasable-groups))) (second (first (first erasable-groups)))])) "赤色グループ"))))

(deftest small-group-test
  (testing "小さなグループの非消去"
    (let [board (-> (core/create-empty-board)
                    (assoc-in [10 3] 1) (assoc-in [10 4] 1)
                    (assoc-in [9 3] 1))  ; 3つ連結（消去不可）
          erasable-groups (core/find-erasable-groups board)]
      (is (= 0 (count erasable-groups)) "3つ以下は消去されない"))))

(deftest puyo-erasure-test
  (testing "ぷよ消去の実行"
    (let [board (-> (core/create-empty-board)
                    ;; 4つ連結の赤グループ（消去対象）
                    (assoc-in [10 3] 1) (assoc-in [10 4] 1)
                    (assoc-in [9 3] 1) (assoc-in [9 4] 1)
                    ;; 単独の青ぷよ（残存）
                    (assoc-in [8 1] 2))
          result (core/erase-puyos board)]
      (is (= 4 (:erased-count result)) "4つのぷよが消去")
      (is (= 0 (get-in (:board result) [10 3])) "消去位置は空")
      (is (= 0 (get-in (:board result) [10 4])) "消去位置は空")
      (is (= 0 (get-in (:board result) [9 3])) "消去位置は空")
      (is (= 0 (get-in (:board result) [9 4])) "消去位置は空")
      (is (= 2 (get-in (:board result) [8 1])) "他色ぷよは残存"))))

(deftest no-erasure-test
  (testing "消去対象がない場合"
    (let [board (-> (core/create-empty-board)
                    ;; 3つ連結（消去対象外）
                    (assoc-in [10 3] 1) (assoc-in [10 4] 1)
                    (assoc-in [9 3] 1))
          result (core/erase-puyos board)]
      (is (= 0 (:erased-count result)) "消去数は0")
      (is (= board (:board result)) "ボードは変化なし"))))

(deftest multiple-groups-test
  (testing "複数グループの同時消去"
    (let [board (-> (core/create-empty-board)
                    ;; 赤グループ（4つ）
                    (assoc-in [10 1] 1) (assoc-in [10 2] 1)
                    (assoc-in [9 1] 1) (assoc-in [9 2] 1)
                    ;; 青グループ（4つ）
                    (assoc-in [10 5] 2) (assoc-in [10 6] 2)
                    (assoc-in [9 5] 2) (assoc-in [9 6] 2))
          result (core/erase-puyos board)]
      (is (= 8 (:erased-count result)) "8つのぷよが消去")
      (is (= 0 (get-in (:board result) [10 1])) "赤グループ消去")
      (is (= 0 (get-in (:board result) [10 5])) "青グループ消去"))))

;; T012: 連鎖の実装テスト
(deftest chain-detection-test
  (testing "基本的な消去処理"
    (let [board (-> (core/create-empty-board)
                    ;; 青の4つ組（確実に消去される）
                    (assoc-in [11 3] 2) (assoc-in [11 4] 2)
                    (assoc-in [10 3] 2) (assoc-in [10 4] 2))
          result (core/execute-chain board)]
      (is (>= (:chain-count result) 1) "消去が発生")
      (is (> (:total-score result) 0) "スコア加算"))))

(deftest no-chain-test
  (testing "連鎖が発生しない場合"
    (let [board (-> (core/create-empty-board)
                    ;; 消去できない3つ組
                    (assoc-in [11 2] 1) (assoc-in [11 3] 1)
                    (assoc-in [10 2] 1))
          result (core/execute-chain board)]
      (is (= 0 (:chain-count result)) "連鎖なし")
      (is (= 0 (:total-score result)) "スコアなし"))))

(deftest multiple-chain-test
  (testing "基本的な連鎖システム"
    (let [board (-> (core/create-empty-board)
                    ;; 青の4つ組
                    (assoc-in [11 1] 2) (assoc-in [11 2] 2)
                    (assoc-in [10 1] 2) (assoc-in [10 2] 2))
          result (core/execute-chain board)]
      (is (>= (:chain-count result) 1) "消去処理が実行される")
      (is (> (:total-score result) 30) "基本スコア取得"))))

;; T013: 基本スコア計算テスト
(deftest basic-score-test
  (testing "消去ぷよ数に基づくベーススコア"
    (let [score-4 (core/calculate-base-score 4 1 1 1)
          score-8 (core/calculate-base-score 8 1 1 1)]
      (is (= 40 score-4) "4つ消去のベーススコア")
      (is (= 80 score-8) "8つ消去のベーススコア"))))

(deftest chain-multiplier-test
  (testing "連鎖倍率の計算"
    (let [chain-1 (core/calculate-chain-multiplier 1)
          chain-2 (core/calculate-chain-multiplier 2)
          chain-3 (core/calculate-chain-multiplier 3)]
      (is (= 1 chain-1) "1連鎖は倍率1")
      (is (= 8 chain-2) "2連鎖は倍率8")
      (is (= 16 chain-3) "3連鎖は倍率16"))))

(deftest group-bonus-test
  (testing "同時消し倍率の計算"
    (let [single-group (core/calculate-group-bonus 1)
          dual-group (core/calculate-group-bonus 2)
          triple-group (core/calculate-group-bonus 3)]
      (is (= 1 single-group) "1グループは倍率1")
      (is (= 3 dual-group) "2グループは倍率3")
      (is (= 6 triple-group) "3グループは倍率6"))))

(deftest color-bonus-test
  (testing "色数ボーナスの計算"
    (let [one-color (core/calculate-color-bonus 1)
          two-colors (core/calculate-color-bonus 2)
          three-colors (core/calculate-color-bonus 3)]
      (is (= 1 one-color) "1色は倍率1")
      (is (= 3 two-colors) "2色は倍率3")
      (is (= 6 three-colors) "3色は倍率6"))))

(deftest total-score-calculation-test
  (testing "総合スコア計算"
    (let [simple-score (core/calculate-total-score 4 1 1 1)
          chain-score (core/calculate-total-score 4 2 1 1)
          bonus-score (core/calculate-total-score 8 2 2 2)]
      (is (= 40 simple-score) "基本スコア")
      (is (= 320 chain-score) "連鎖込みスコア")
      (is (> bonus-score 1000) "ボーナス込みスコア"))))

;; T014: 全消し判定テスト
(deftest perfect-clear-detection-test
  (testing "全消し（ぜんけし）判定"
    (let [empty-board (core/create-empty-board)
          non-empty-board (-> (core/create-empty-board)
                              (assoc-in [11 3] 1))]
      (is (true? (core/is-perfect-clear? empty-board)) "空ボードは全消し")
      (is (false? (core/is-perfect-clear? non-empty-board)) "ぷよが残っている場合は全消しではない"))))

(deftest perfect-clear-bonus-test
  (testing "全消しボーナススコア計算"
    (let [bonus-score (core/calculate-perfect-clear-bonus)]
      (is (= 8500 bonus-score) "全消しボーナスは8500点"))))

(deftest perfect-clear-execution-test
  (testing "全消し実行とボーナス計算"
    (let [board (-> (core/create-empty-board)
                    ;; 4つの赤ぷよ（最後の1グループ）
                    (assoc-in [11 3] 1) (assoc-in [11 4] 1)
                    (assoc-in [10 3] 1) (assoc-in [10 4] 1))
          result (core/execute-perfect-clear board)]
      (is (true? (:is-perfect-clear result)) "全消し発生")
      (is (= 8500 (:perfect-clear-bonus result)) "全消しボーナス取得")
      (is (> (:total-score result) 8500) "総スコアに全消しボーナス加算"))))

(deftest no-perfect-clear-test
  (testing "全消しが発生しない場合"
    (let [board (-> (core/create-empty-board)
                    ;; 消去されないぷよが残る
                    (assoc-in [11 3] 1) (assoc-in [11 4] 1)
                    (assoc-in [10 3] 1) (assoc-in [9 4] 2))
          result (core/execute-perfect-clear board)]
      (is (false? (:is-perfect-clear result)) "全消し未発生")
      (is (= 0 (:perfect-clear-bonus result)) "全消しボーナスなし")
      (is (= 0 (:total-score result)) "基本スコアのみ"))))

;; T015: ゲーム画面の描画テスト
(deftest canvas-initialization-test
  (testing "Canvas初期化"
    (let [canvas-id "test-canvas"
          result (core/init-canvas canvas-id)]
      (is (boolean? result) "初期化結果はboolean"))))

(deftest color-mapping-test
  (testing "色とカラーコードのマッピング"
    (let [red-color (core/get-puyo-color 1)
          blue-color (core/get-puyo-color 2)
          empty-color (core/get-puyo-color 0)]
      (is (string? red-color) "赤色はカラーコード文字列")
      (is (string? blue-color) "青色はカラーコード文字列")
      (is (string? empty-color) "空はカラーコード文字列")
      (is (not= red-color blue-color) "色は異なる"))))

(deftest board-rendering-test
  (testing "ボード描画処理"
    (let [board (-> (core/create-empty-board)
                    (assoc-in [11 3] 1)
                    (assoc-in [10 2] 2))
          result (core/render-board board)]
      (is (nil? result) "描画処理は戻り値なし"))))

(deftest puyo-pair-rendering-test
  (testing "組ぷよ描画処理"
    (let [puyo-pair {:puyo1 {:x 3 :y 1 :color 1}
                     :puyo2 {:x 3 :y 2 :color 2}
                     :rotation 0}
          result (core/render-puyo-pair puyo-pair)]
      (is (nil? result) "組ぷよ描画処理は戻り値なし"))))

(deftest game-state-display-test
  (testing "ゲーム状態表示"
    (let [game-state {:score 1500 :level 2 :chain-count 3}
          result (core/update-game-display game-state)]
      (is (nil? result) "状態表示更新は戻り値なし"))))

;; 組ぷよ表示テスト
(deftest current-puyo-display-test
  (testing "現在の組ぷよ生成と表示"
    (let [puyo-pair (core/spawn-new-puyo-pair)]
      (is (map? puyo-pair) "組ぷよは辞書形式")
      (is (contains? puyo-pair :puyo1) "puyo1を含む")
      (is (contains? puyo-pair :puyo2) "puyo2を含む")
      (is (number? (get-in puyo-pair [:puyo1 :x])) "puyo1のx座標")
      (is (number? (get-in puyo-pair [:puyo1 :y])) "puyo1のy座標")
      (is (>= (get-in puyo-pair [:puyo1 :x]) 0) "x座標は0以上")
      (is (>= (get-in puyo-pair [:puyo1 :y]) 0) "y座標は0以上"))))

(deftest validation-test
  (testing "バリデーション"
    (is (true? (core/valid-color? 1)) "有効な色")
    (is (false? (core/valid-color? 0)) "無効な色")
    (is (true? (core/valid-rotation? 0)) "有効な回転")
    (is (false? (core/valid-rotation? 4)) "無効な回転")
    (is (true? (core/valid-direction? :left)) "有効な方向")
    (is (false? (core/valid-direction? :up)) "無効な方向")))

;; T016: ゲーム情報の表示テスト
(deftest chain-count-management-test
  (testing "連鎖数管理"
    (let [initial-state @core/game-state]
      ;; 連鎖数の初期化
      (core/reset-chain-count!)
      (is (= 0 (:chain-count @core/game-state)) "連鎖数初期値は0")

      ;; 連鎖数の増加
      (core/increment-chain-count!)
      (is (= 1 (:chain-count @core/game-state)) "連鎖数が1増加")

      ;; 連鎖数の設定
      (core/set-chain-count! 5)
      (is (= 5 (:chain-count @core/game-state)) "連鎖数を直接設定")

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest game-time-management-test
  (testing "ゲーム時間管理"
    (let [initial-state @core/game-state]
      ;; ゲーム時間の初期化
      (core/reset-game-time!)
      (is (= 0 (:game-time @core/game-state)) "ゲーム時間初期値は0")

      ;; ゲーム時間の更新
      (core/update-game-time! 120)
      (is (= 120 (:game-time @core/game-state)) "ゲーム時間を更新")

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest game-info-display-test
  (testing "ゲーム情報表示機能"
    (let [initial-state @core/game-state]
      ;; テスト用状態設定
      (swap! core/game-state assoc
             :score 2500
             :level 3
             :chain-count 4
             :game-time 180)

      ;; 表示更新機能のテスト
      (let [result (core/update-all-game-info!)]
        (is (nil? result) "ゲーム情報更新は戻り値なし"))

      ;; スコア表示のテスト
      (let [result (core/update-score-display!)]
        (is (nil? result) "スコア表示更新は戻り値なし"))

      ;; 連鎖数表示のテスト
      (let [result (core/update-chain-display!)]
        (is (nil? result) "連鎖数表示更新は戻り値なし"))

      ;; 時間表示のテスト
      (let [result (core/update-time-display!)]
        (is (nil? result) "時間表示更新は戻り値なし"))

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest time-formatting-test
  (testing "時間フォーマット機能"
    (is (= "0:00" (core/format-game-time 0)) "0秒のフォーマット")
    (is (= "0:30" (core/format-game-time 30)) "30秒のフォーマット")
    (is (= "1:00" (core/format-game-time 60)) "1分のフォーマット")
    (is (= "2:15" (core/format-game-time 135)) "2分15秒のフォーマット")
    (is (= "10:05" (core/format-game-time 605)) "10分5秒のフォーマット")))

;; T017: キーボード入力処理テスト
(deftest keyboard-input-processing-test
  (testing "キーボード入力処理機能"
    (let [initial-state @core/game-state
          test-puyo-pair {:puyo1 {:x 3 :y 1 :color 1}
                          :puyo2 {:x 3 :y 2 :color 2}
                          :rotation 0}]
      ;; ゲーム状態をテスト用に設定
      (swap! core/game-state assoc
             :game-running true
             :current-piece test-puyo-pair
             :board (core/create-empty-board))

      ;; 左移動処理のテスト
      (let [result (core/handle-key-input "ArrowLeft")]
        (is (map? result) "左移動処理は結果を返す"))

      ;; 右移動処理のテスト
      (let [result (core/handle-key-input "ArrowRight")]
        (is (map? result) "右移動処理は結果を返す"))

      ;; 回転処理のテスト
      (let [result (core/handle-key-input "ArrowUp")]
        (is (map? result) "回転処理は結果を返す"))

      ;; 高速落下処理のテスト
      (let [result (core/handle-key-input "ArrowDown")]
        (is (map? result) "高速落下処理は結果を返す"))

      ;; ハードドロップ処理のテスト
      (let [result (core/handle-key-input " ")]
        (is (map? result) "ハードドロップ処理は結果を返す"))

      ;; 無効なキー処理のテスト
      (let [result (core/handle-key-input "Escape")]
        (is (nil? result) "無効なキーは何も処理しない"))

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest puyo-movement-integration-test
  (testing "ぷよ移動統合機能"
    (let [initial-state @core/game-state
          test-board (core/create-empty-board)
          test-puyo-pair {:puyo1 {:x 3 :y 1 :color 1}
                          :puyo2 {:x 3 :y 2 :color 2}
                          :rotation 0}]

      ;; テスト用ゲーム状態設定
      (swap! core/game-state assoc
             :game-running true
             :current-piece test-puyo-pair
             :board test-board)

      ;; 左移動統合テスト
      (core/process-left-movement!)
      (let [updated-piece (:current-piece @core/game-state)]
        (is (= 2 (get-in updated-piece [:puyo1 :x])) "左移動後のx座標"))

      ;; 右移動統合テスト
      (core/process-right-movement!)
      (let [updated-piece (:current-piece @core/game-state)]
        (is (= 3 (get-in updated-piece [:puyo1 :x])) "右移動後のx座標"))

      ;; 回転統合テスト
      (core/process-rotation!)
      (let [updated-piece (:current-piece @core/game-state)]
        (is (= 1 (:rotation updated-piece)) "回転後の回転状態"))

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest keyboard-game-state-validation-test
  (testing "キーボード入力時のゲーム状態検証"
    (let [initial-state @core/game-state]

      ;; ゲーム停止時のキー入力テスト
      (swap! core/game-state assoc :game-running false)
      (let [result (core/handle-key-input "ArrowLeft")]
        (is (nil? result) "ゲーム停止時はキー入力を無視"))

      ;; 現在の組ぷよがない場合のテスト
      (swap! core/game-state assoc
             :game-running true
             :current-piece nil)
      (let [result (core/handle-key-input "ArrowLeft")]
        (is (nil? result) "現在の組ぷよがない場合はキー入力を無視"))

      ;; 状態復元
      (reset! core/game-state initial-state))))

;; T019: ゲーム初期化テスト
(deftest game-initialization-test
  (testing "ゲーム初期化機能"
    (let [initial-state @core/game-state]
      ;; ゲーム状態のリセット
      (swap! core/game-state assoc
             :score 1000
             :level 5
             :chain-count 3
             :game-time 120
             :game-running true
             :current-piece {:puyo1 {:x 3 :y 1 :color 1} :puyo2 {:x 3 :y 2 :color 2} :rotation 0}
             :board [[1 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0]
                     [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0]
                     [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0]])

      ;; ゲーム状態の初期化テスト
      (core/reset-game-state!)
      (is (= 0 (:score @core/game-state)) "スコアが0にリセット")
      (is (= 1 (:level @core/game-state)) "レベルが1にリセット")
      (is (= 0 (:chain-count @core/game-state)) "連鎖数が0にリセット")
      (is (= 0 (:game-time @core/game-state)) "ゲーム時間が0にリセット")
      (is (false? (:game-running @core/game-state)) "ゲーム実行フラグがfalseにリセット")
      (is (nil? (:current-piece @core/game-state)) "現在の組ぷよがnilにリセット")

      ;; ボード初期化テスト
      (core/initialize-game-board!)
      (let [board (:board @core/game-state)]
        (is (vector? board) "ボードがベクター")
        (is (= 12 (count board)) "ボードの高さが12")
        (is (every? #(= 8 (count %)) board) "各行の幅が8")
        (is (every? #(every? zero? %) board) "すべてのセルが0（空）"))

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest new-game-start-test
  (testing "新しいゲーム開始機能"
    (let [initial-state @core/game-state]
      ;; 新しいゲーム開始
      (core/start-new-game!)

      ;; ゲーム状態の確認
      (is (true? (:game-running @core/game-state)) "ゲームが開始状態")
      (is (= 0 (:score @core/game-state)) "スコアが初期値")
      (is (= 1 (:level @core/game-state)) "レベルが初期値")
      (is (= 0 (:chain-count @core/game-state)) "連鎖数が初期値")
      (is (= 0 (:game-time @core/game-state)) "ゲーム時間が初期値")

      ;; ボードが初期化されている
      (let [board (:board @core/game-state)]
        (is (vector? board) "ボードがベクター")
        (is (= 12 (count board)) "ボードの高さが12")
        (is (every? #(= 8 (count %)) board) "各行の幅が8")
        (is (every? #(every? zero? %) board) "すべてのセルが空"))

      ;; 初期の組ぷよが生成されている
      (let [current-piece (:current-piece @core/game-state)]
        (is (map? current-piece) "現在の組ぷよが存在")
        (is (contains? current-piece :puyo1) "puyo1が存在")
        (is (contains? current-piece :puyo2) "puyo2が存在")
        (is (contains? current-piece :rotation) "回転状態が存在")
        (is (core/valid-color? (get-in current-piece [:puyo1 :color])) "puyo1の色が有効")
        (is (core/valid-color? (get-in current-piece [:puyo2 :color])) "puyo2の色が有効")
        (is (core/valid-rotation? (:rotation current-piece)) "回転状態が有効"))

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest game-initialization-integration-test
  (testing "ゲーム初期化統合機能"
    (let [initial-state @core/game-state]
      ;; ゲーム全体の初期化
      (core/init-game!)

      ;; すべてが適切に初期化されている
      (is (true? (:game-running @core/game-state)) "ゲームが開始状態")
      (is (map? (:current-piece @core/game-state)) "組ぷよが生成済み")
      (is (every? #(every? zero? %) (:board @core/game-state)) "ボードが空")
      (is (= 0 (:score @core/game-state)) "スコア初期化")
      (is (= 0 (:chain-count @core/game-state)) "連鎖数初期化")
      (is (= 0 (:game-time @core/game-state)) "時間初期化")

      ;; 状態復元
      (reset! core/game-state initial-state))))

;; T020: ゲーム終了判定テスト
(deftest game-over-detection-test
  (testing "ゲームオーバー判定機能"
    (let [initial-state @core/game-state]
      ;; 空のボードの場合（ゲームオーバーではない）
      (swap! core/game-state assoc :board (core/create-empty-board))
      (is (false? (core/is-game-over?)) "空のボードはゲームオーバーではない")

      ;; ボード上部（y=0）にぷよがある場合（ゲームオーバー）
      (let [game-over-board (assoc-in (core/create-empty-board) [0 3] 1)]
        (swap! core/game-state assoc :board game-over-board)
        (is (true? (core/is-game-over?)) "上部にぷよがあるとゲームオーバー"))

      ;; ボード上部（y=1）にぷよがある場合（ゲームオーバー）
      (let [game-over-board (assoc-in (core/create-empty-board) [1 3] 2)]
        (swap! core/game-state assoc :board game-over-board)
        (is (true? (core/is-game-over?)) "上部2行目にぷよがあるとゲームオーバー"))

      ;; ボード下部のみにぷよがある場合（ゲームオーバーではない）
      (let [normal-board (assoc-in (core/create-empty-board) [10 3] 1)]
        (swap! core/game-state assoc :board normal-board)
        (is (false? (core/is-game-over?)) "下部のみのぷよはゲームオーバーではない"))

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest game-over-processing-test
  (testing "ゲームオーバー処理機能"
    (let [initial-state @core/game-state]
      ;; ゲーム実行中の状態を設定
      (swap! core/game-state assoc
             :game-running true
             :score 1500
             :level 3
             :chain-count 2
             :game-time 180)

      ;; ゲームオーバー処理を実行
      (core/process-game-over!)

      ;; ゲームが停止状態になる
      (is (false? (:game-running @core/game-state)) "ゲームが停止状態")

      ;; スコアやその他の情報は保持される
      (is (= 1500 (:score @core/game-state)) "スコアが保持される")
      (is (= 3 (:level @core/game-state)) "レベルが保持される")
      (is (= 2 (:chain-count @core/game-state)) "連鎖数が保持される")
      (is (= 180 (:game-time @core/game-state)) "ゲーム時間が保持される")

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest game-over-integration-test
  (testing "ゲームオーバー統合機能"
    (let [initial-state @core/game-state]
      ;; ゲーム実行中でボード上部にぷよがある状態
      (let [game-over-board (assoc-in (core/create-empty-board) [0 3] 1)]
        (swap! core/game-state assoc
               :game-running true
               :board game-over-board
               :score 2000
               :current-piece {:puyo1 {:x 3 :y 1 :color 1} :puyo2 {:x 3 :y 2 :color 2} :rotation 0}))

      ;; ゲームオーバーチェックと処理
      (let [game-over-result (core/check-and-handle-game-over!)]
        (is (true? game-over-result) "ゲームオーバーが検出される")
        (is (false? (:game-running @core/game-state)) "ゲームが停止される")
        (is (= 2000 (:score @core/game-state)) "最終スコアが保持される"))

      ;; 状態復元
      (reset! core/game-state initial-state))))

(deftest game-over-conditions-test
  (testing "ゲームオーバー条件の詳細テスト"
    (let [initial-state @core/game-state]
      ;; 危険ライン（y=0, y=1）のテスト
      (doseq [dangerous-y [0 1]]
        (doseq [x (range 8)]
          (let [test-board (assoc-in (core/create-empty-board) [dangerous-y x] 1)]
            (swap! core/game-state assoc :board test-board)
            (is (true? (core/is-game-over?))
                (str "位置 [" dangerous-y " " x "] のぷよでゲームオーバー")))))

      ;; 安全ライン（y=2以降）のテスト
      (doseq [safe-y (range 2 12)]
        (doseq [x (range 8)]
          (let [test-board (assoc-in (core/create-empty-board) [safe-y x] 1)]
            (swap! core/game-state assoc :board test-board)
            (is (false? (core/is-game-over?))
                (str "位置 [" safe-y " " x "] のぷよは安全")))))

      ;; 状態復元
      (reset! core/game-state initial-state))))

(run-tests)
