(ns puyo.core-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [puyo.core :as core]))

(deftest test-create-empty-board
  (testing "空のゲームボードの作成"
    (let [board (core/create-empty-board)]
      (is (= core/board-height (count board)) "ボードの高さが正しい")
      (is (= core/board-width (count (first board))) "ボードの幅が正しい")
      (is (every? zero? (flatten board)) "すべてのセルが0（空）で初期化されている"))))

(deftest test-board-dimensions
  (testing "ボードサイズの定数確認"
    (is (= 8 core/board-width) "ボード幅は8")
    (is (= 12 core/board-height) "ボード高さは12")))

(deftest test-colors-definition
  (testing "色の定義確認"
    (is (contains? core/colors 0) "空の色が定義されている")
    (is (contains? core/colors 1) "赤の色が定義されている")
    (is (contains? core/colors 2) "緑の色が定義されている")
    (is (contains? core/colors 3) "青の色が定義されている")
    (is (contains? core/colors 4) "黄の色が定義されている")
    (is (contains? core/colors 5) "紫の色が定義されている")))

(deftest test-game-state-initialization
  (testing "ゲーム状態の初期化"
    (core/init-game-state!)
    (let [state @core/game-state]
      (is (vector? (:board state)) "ボードがベクターである")
      (is (= core/board-height (count (:board state))) "ボードの行数が正しい")
      (is (= core/board-width (count (first (:board state)))) "ボードの列数が正しい")
      (is (nil? (:current-piece state)) "現在のピースがnil")
      (is (= 0 (:score state)) "スコアが0で初期化")
      (is (= 1 (:level state)) "レベルが1で初期化")
      (is (false? (:game-running state)) "ゲームが停止状態で初期化"))))

(deftest test-cell-size
  (testing "セルサイズの確認"
    (is (= 40 core/cell-size) "セルサイズが40ピクセル")))

(deftest test-color-values
  (testing "色の値の確認"
    (is (= "#ffffff" (core/colors 0)) "空は白色")
    (is (= "#ff0000" (core/colors 1)) "1は赤色")
    (is (= "#00ff00" (core/colors 2)) "2は緑色")
    (is (= "#0000ff" (core/colors 3)) "3は青色")
    (is (= "#ffff00" (core/colors 4)) "4は黄色")
    (is (= "#ff00ff" (core/colors 5)) "5は紫色")))

(deftest test-puyo-pair-creation
  (testing "組ぷよの作成"
    (let [pair (core/create-puyo-pair 1 2 3 0)]
      (is (map? pair) "組ぷよはマップである")
      (is (contains? pair :puyo1) "puyo1が含まれる")
      (is (contains? pair :puyo2) "puyo2が含まれる")
      (is (contains? pair :rotation) "回転状態が含まれる")
      (is (= 1 (get-in pair [:puyo1 :color])) "puyo1の色が正しい")
      (is (= 2 (get-in pair [:puyo2 :color])) "puyo2の色が正しい")
      (is (= 3 (get-in pair [:puyo1 :x])) "puyo1のx座標が正しい")
      (is (= 0 (get-in pair [:puyo1 :y])) "puyo1のy座標が正しい")
      (is (= 0 (:rotation pair)) "初期回転状態が0"))))

(deftest test-puyo-pair-rotation-positions
  (testing "組ぷよの回転時の相対位置計算"
    (testing "縦向き（rotation 0）"
      (let [positions (core/get-puyo-pair-positions 3 0 0)]
        (is (= [{:x 3 :y 0} {:x 3 :y 1}] positions) "縦向きの位置が正しい")))

    (testing "右向き（rotation 1）"
      (let [positions (core/get-puyo-pair-positions 3 0 1)]
        (is (= [{:x 3 :y 0} {:x 4 :y 0}] positions) "右向きの位置が正しい")))

    (testing "逆縦向き（rotation 2）"
      (let [positions (core/get-puyo-pair-positions 3 1 2)]
        (is (= [{:x 3 :y 1} {:x 3 :y 0}] positions) "逆縦向きの位置が正しい")))

    (testing "左向き（rotation 3）"
      (let [positions (core/get-puyo-pair-positions 4 0 3)]
        (is (= [{:x 4 :y 0} {:x 3 :y 0}] positions) "左向きの位置が正しい")))))

(deftest test-puyo-pair-rotation
  (testing "組ぷよの回転"
    (let [initial-pair (core/create-puyo-pair 1 2 3 0)
          rotated-pair (core/rotate-puyo-pair initial-pair)]
      (is (= 1 (:rotation rotated-pair)) "回転後の状態が1")
      (is (= 1 (get-in rotated-pair [:puyo1 :color])) "puyo1の色は変わらない")
      (is (= 2 (get-in rotated-pair [:puyo2 :color])) "puyo2の色は変わらない"))

    (testing "360度回転"
      (let [initial-pair (core/create-puyo-pair 1 2 3 0)]
        (is (= 0 (:rotation (-> initial-pair
                                core/rotate-puyo-pair
                                core/rotate-puyo-pair
                                core/rotate-puyo-pair
                                core/rotate-puyo-pair))) "4回転で元に戻る")))))

(deftest test-puyo-pair-validation
  (testing "組ぷよの色の妥当性"
    (is (thrown? js/Error (core/create-puyo-pair 0 1 3 0)) "puyo1に無効な色（0）を指定するとエラー")
    (is (thrown? js/Error (core/create-puyo-pair 1 6 3 0)) "puyo2に無効な色（6）を指定するとエラー")
    (is (thrown? js/Error (core/get-puyo-pair-positions 3 0 4)) "無効な回転状態（4）を指定するとエラー")))

;; テスト実行のエントリーポイント
(defn ^:export run-all-tests []
  (run-tests))

;; Node.js環境でのテスト実行
(when (exists? js/process)
  (set! *main-cli-fn* run-all-tests))
