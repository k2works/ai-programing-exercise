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

(deftest validation-test
  (testing "バリデーション"
    (is (true? (core/valid-color? 1)) "有効な色")
    (is (false? (core/valid-color? 0)) "無効な色")
    (is (true? (core/valid-rotation? 0)) "有効な回転")
    (is (false? (core/valid-rotation? 4)) "無効な回転")
    (is (true? (core/valid-direction? :left)) "有効な方向")
    (is (false? (core/valid-direction? :up)) "無効な方向")))

(run-tests)
