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
    (let [pair (core/create-puyo-pair 1 2 3 1)]
      (let [rotated (core/rotate-puyo-pair pair)]
        (is (= 1 (:rotation rotated)) "回転状態が1")
        (is (= 3 (get-in rotated [:puyo1 :x])) "puyo1のx座標")
        (is (= 4 (get-in rotated [:puyo2 :x])) "puyo2が右に移動")))))

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

(deftest validation-test
  (testing "バリデーション"
    (is (true? (core/valid-color? 1)) "有効な色")
    (is (false? (core/valid-color? 0)) "無効な色")
    (is (true? (core/valid-rotation? 0)) "有効な回転")
    (is (false? (core/valid-rotation? 4)) "無効な回転")
    (is (true? (core/valid-direction? :left)) "有効な方向")
    (is (false? (core/valid-direction? :up)) "無効な方向")))

(run-tests)
