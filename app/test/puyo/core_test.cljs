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

;; テスト実行のエントリーポイント
(defn ^:export run-all-tests []
  (run-tests))

;; Node.js環境でのテスト実行
(when (exists? js/process)
  (set! *main-cli-fn* run-all-tests))
