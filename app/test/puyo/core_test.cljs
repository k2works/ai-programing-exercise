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

(deftest validation-test
  (testing "バリデーション"
    (is (true? (core/valid-color? 1)) "有効な色")
    (is (false? (core/valid-color? 0)) "無効な色")
    (is (true? (core/valid-rotation? 0)) "有効な回転")
    (is (false? (core/valid-rotation? 4)) "無効な回転")
    (is (true? (core/valid-direction? :left)) "有効な方向")
    (is (false? (core/valid-direction? :up)) "無効な方向")))

(run-tests)
