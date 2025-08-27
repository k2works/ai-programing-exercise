(ns test-gravity
  (:require [puyo.core :as puyo]))

;; 重力テスト用の簡単なテストケース
(defn test-floating-puyo-gravity []
  (println "=== 浮いているぷよの重力テスト ===")
  
  ;; テスト用ボード状態を作成
  (let [test-board (-> (puyo/create-empty-board)
                       ;; 底に赤ぷよを配置
                       (puyo/place-puyo 3 11 :red)
                       ;; 空間を空けて上に青ぷよを配置（浮いた状態）
                       (puyo/place-puyo 3 9 :blue))]
    
    (println "テスト前のボード状態:")
    (doseq [row (range 12)]
      (let [line (apply str (map #(case (puyo/get-puyo-at test-board % row)
                                    :red "R"
                                    :blue "B"
                                    :green "G"
                                    :yellow "Y"
                                    " ") (range 8)))]
        (println (str row ": " line))))
    
    ;; 重力を適用
    (let [after-gravity (puyo/drop-floating-puyos test-board)]
      (println "\n重力適用後のボード状態:")
      (doseq [row (range 12)]
        (let [line (apply str (map #(case (puyo/get-puyo-at after-gravity % row)
                                      :red "R"
                                      :blue "B"
                                      :green "G"
                                      :yellow "Y"
                                      " ") (range 8)))]
          (println (str row ": " line))))
      
      ;; 青ぷよが落下したかチェック
      (let [blue-at-10 (= :blue (puyo/get-puyo-at after-gravity 3 10))
            blue-at-9 (= :blue (puyo/get-puyo-at after-gravity 3 9))]
        (if blue-at-10
          (println "\n✓ テスト成功: 青ぷよが正しく落下しました")
          (if blue-at-9
            (println "\n✗ テスト失敗: 青ぷよが落下していません")
            (println "\n? テスト不明: 青ぷよが見つかりません")))))))

;; テスト実行
(test-floating-puyo-gravity)
