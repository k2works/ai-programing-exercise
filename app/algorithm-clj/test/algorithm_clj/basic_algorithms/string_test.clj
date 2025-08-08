(ns algorithm-clj.basic-algorithms.string-test
  (:require [clojure.test :refer :all]
            [algorithm-clj.basic-algorithms.string :refer :all]))

;; ============================
;; 第7章: 文字列アルゴリズム テスト
;; ============================

;; ============================
;; 文字列の基本操作テスト
;; ============================

(deftest test-string-length
  (testing "文字列の長さを求める"
    (is (= (string-length "") 0))
    (is (= (string-length "hello") 5))
    (is (= (string-length "こんにちは") 5))))

(deftest test-string-concat
  (testing "文字列の連結"
    (is (= (string-concat "hello" " " "world") "hello world"))
    (is (= (string-concat "a" "b" "c") "abc"))
    (is (= (string-concat "") ""))))

(deftest test-substring
  (testing "部分文字列の抽出"
    (is (= (substring "hello" 1 4) "ell"))
    (is (= (substring "programming" 0 4) "prog"))
    (is (= (substring "test" 2 4) "st"))))

;; ============================
;; 文字列検索テスト
;; ============================

(deftest test-string-search-simple
  (testing "単純な文字列検索"
    (is (= (string-search-simple "hello world" "world") 6))
    (is (= (string-search-simple "programming" "gram") 3))
    (is (= (string-search-simple "test" "xyz") -1))
    (is (= (string-search-simple "" "a") -1))
    (is (= (string-search-simple "hello" "") 0))))

(deftest test-string-search-kmp
  (testing "KMP法による文字列検索"
    (is (= (string-search-kmp "hello world" "world") 6))
    (is (= (string-search-kmp "programming" "gram") 3))
    (is (= (string-search-kmp "test" "xyz") -1))
    (is (= (string-search-kmp "ABABCABABA" "ABABA") 5))))

(deftest test-string-search-boyer-moore
  (testing "Boyer-Moore法による文字列検索"
    (is (= (string-search-boyer-moore "hello world" "world") 6))
    (is (= (string-search-boyer-moore "programming" "gram") 3))
    (is (= (string-search-boyer-moore "test" "xyz") -1))))

;; ============================
;; 文字列変換テスト
;; ============================

(deftest test-string-reverse
  (testing "文字列の反転"
    (is (= (string-reverse "hello") "olleh"))
    (is (= (string-reverse "a") "a"))
    (is (= (string-reverse "") ""))))

(deftest test-string-uppercase
  (testing "文字列を大文字に変換"
    (is (= (string-uppercase "hello") "HELLO"))
    (is (= (string-uppercase "Hello World") "HELLO WORLD"))
    (is (= (string-uppercase "") ""))))

(deftest test-string-lowercase
  (testing "文字列を小文字に変換"
    (is (= (string-lowercase "HELLO") "hello"))
    (is (= (string-lowercase "Hello World") "hello world"))
    (is (= (string-lowercase "") ""))))

;; ============================
;; 文字列解析テスト
;; ============================

(deftest test-is-palindrome
  (testing "回文判定"
    (is (= (is-palindrome "racecar") true))
    (is (= (is-palindrome "hello") false))
    (is (= (is-palindrome "a") true))
    (is (= (is-palindrome "") true))
    (is (= (is-palindrome "madam") true))))

(deftest test-is-anagram
  (testing "アナグラム判定"
    (is (= (is-anagram "listen" "silent") true))
    (is (= (is-anagram "hello" "world") false))
    (is (= (is-anagram "evil" "vile") true))
    (is (= (is-anagram "a" "a") true))
    (is (= (is-anagram "" "") true))))

;; ============================
;; 文字列圧縮テスト
;; ============================

(deftest test-run-length-encode
  (testing "ランレングス符号化"
    (is (= (run-length-encode "aabcccccaaa") "a2b1c5a3"))
    (is (= (run-length-encode "abcdef") "a1b1c1d1e1f1"))
    (is (= (run-length-encode "") ""))
    (is (= (run-length-encode "aaaa") "a4"))))

(deftest test-run-length-decode
  (testing "ランレングス復号化"
    (is (= (run-length-decode "a2b1c5a3") "aabcccccaaa"))
    (is (= (run-length-decode "a1b1c1d1e1f1") "abcdef"))
    (is (= (run-length-decode "") ""))
    (is (= (run-length-decode "a4") "aaaa"))))

;; ============================
;; 文字列パターンマッチングテスト
;; ============================

(deftest test-pattern-match-wildcard
  (testing "ワイルドカードパターンマッチング"
    (is (= (pattern-match-wildcard "hello" "h*o") true))
    (is (= (pattern-match-wildcard "test" "t?st") true))
    (is (= (pattern-match-wildcard "hello" "h?llo") true))
    (is (= (pattern-match-wildcard "hello" "h?l") false))))

;; ============================
;; 文字列編集距離テスト
;; ============================

(deftest test-edit-distance
  (testing "レーベンシュタイン距離"
    (is (= (edit-distance "kitten" "sitting") 3))
    (is (= (edit-distance "hello" "hello") 0))
    (is (= (edit-distance "" "abc") 3))
    (is (= (edit-distance "abc" "") 3))))
