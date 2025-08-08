(ns algorithm-clj.demos.string-demo
  "文字列アルゴリズムのデモプログラム"
 (:require [algorithm-clj.algorithms.string :as string-alg]))

(defn print-divider [title]
  (println "\n" (str "=" (apply str (repeat (count title) "=")) "="))
  (println (str " " title " "))
  (println (str "=" (apply str (repeat (count title) "=")) "=")))

(defn demo-string-search []
  (print-divider "文字列検索アルゴリズム")
  
  (let [text "Hello, World! This is a Hello World example."
        pattern "Hello"]
    
    (println "テキスト:" text)
    (println "パターン:" pattern)
    (println)
    
    ; 単純検索
    (let [result (string-alg/string-search-simple text pattern)]
      (println "単純検索結果:" result))
    
    ; KMP検索
    (let [result (string-alg/string-search-kmp text pattern)]
      (println "KMP検索結果:" result))
    
    ; Boyer-Moore検索
    (let [result (string-alg/string-search-boyer-moore text pattern)]
      (println "Boyer-Moore検索結果:" result))))

(defn demo-string-manipulation []
  (print-divider "文字列操作")
  
  (let [original "Hello, World!"]
    (println "元の文字列:" original)
    (println "長さ:" (string-alg/string-length original))
    (println "反転:" (string-alg/string-reverse original))
    (println "大文字:" (string-alg/string-uppercase original))
    (println "小文字:" (string-alg/string-lowercase original))
    (println "部分文字列(0-5):" (string-alg/substring original 0 5))

    ;; 文字列連結のデモ
    (println "文字列連結:" (string-alg/string-concat "Hello" ", " "World" "!"))))

(defn demo-string-analysis []
  (print-divider "文字列解析")
  
  ; 回文チェック
  (let [palindrome-tests ["racecar" "hello" "A man a plan a canal Panama"]]
    (println "回文チェック:")
    (doseq [test palindrome-tests]
      (println (str "  \"" test "\" -> " (string-alg/is-palindrome test)))))
  
  (println)
  
  ; アナグラムチェック
  (let [anagram-tests [["listen" "silent"] ["hello" "world"] ["anagram" "nagaram"]]]
    (println "アナグラムチェック:")
    (doseq [[s1 s2] anagram-tests]
      (println (str "  \"" s1 "\" と \"" s2 "\" -> " (string-alg/is-anagram s1 s2))))))

(defn demo-string-compression []
  (print-divider "文字列圧縮")
  
  (let [test-strings ["aabcccccaaa" "abcdef" "aaaa" "a"]]
    (println "ランレングス符号化:")
    (doseq [s test-strings]
      (let [encoded (string-alg/run-length-encode s)
            decoded (string-alg/run-length-decode encoded)]
        (println (str "  元: \"" s "\" -> 符号化: \"" encoded "\" -> 復号化: \"" decoded "\""))))))

(defn demo-pattern-matching []
  (print-divider "パターンマッチング")
  
  (let [text "hello world"
        patterns ["hello*" "h?llo" "*world" "h*d"]]
    (println "テキスト:" text)
    (println "ワイルドカードパターンマッチング:")
    (doseq [pattern patterns]
      (let [result (string-alg/pattern-match-wildcard text pattern)]
        (println (str "  パターン \"" pattern "\" -> " result))))))

(defn demo-edit-distance []
  (print-divider "編集距離")
  
  (let [pairs [["kitten" "sitting"]
               ["hello" "world"]
               ["algorithm" "altruistic"]
               ["same" "same"]]]
    (println "レーベンシュタイン距離:")
    (doseq [[s1 s2] pairs]
      (let [distance (string-alg/edit-distance s1 s2)]
        (println (str "  \"" s1 "\" と \"" s2 "\" の距離: " distance))))))

(defn run-all-demos []
  (println "===========================================")
  (println "    第7章: 文字列アルゴリズムデモ")
  (println "===========================================")
  
  (demo-string-search)
  (demo-string-manipulation)
  (demo-string-analysis)
  (demo-string-compression)
  (demo-pattern-matching)
  (demo-edit-distance)
  
  (println "\n===========================================")
  (println "           デモ完了")
  (println "==========================================="))

(defn -main []
  (run-all-demos))
