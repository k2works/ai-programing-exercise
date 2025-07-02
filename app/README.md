# FizzBuzz - Clojure Implementation

このプロジェクトは、テスト駆動開発（TDD）のプロセスに従ってClojureで実装されたFizzBuzzプログラムです。

## 仕様

1から100までの数をプリントするプログラムです。
- 3の倍数のときは数の代わりに「Fizz」と出力
- 5の倍数のときは数の代わりに「Buzz」と出力  
- 3と5両方の倍数の場合には「FizzBuzz」と出力

## 開発手法

このプロジェクトは以下のTDDサイクルに従って開発されました：

1. **RED**: 失敗するテストを書く
2. **GREEN**: テストが通る最小限の実装を行う
3. **REFACTOR**: コードを改善する

## TODOリスト（完了済み）

- ✅ 数を文字列にして返す
  - ✅ 1を渡したら文字列"1"を返す
  - ✅ 2を渡したら文字列"2"を返す
- ✅ 3の倍数のときは数の代わりに「Fizz」と返す
- ✅ 5の倍数のときは「Buzz」と返す
- ✅ 3と5両方の倍数の場合には「FizzBuzz」と返す
- ✅ 1から100までの数
- ✅ プリントする

## プロジェクト構成

```
app/
├── deps.edn          # プロジェクトの依存関係
├── src/
│   └── fizzbuzz/
│       └── core.clj  # メインのプロダクトコード
└── test/
    └── fizzbuzz/
        └── core_test.clj  # テストコード
```

## 実行方法

### FizzBuzzプログラムの実行

```bash
cd app
clojure -M -e "(require 'fizzbuzz.core) (fizzbuzz.core/-main)"
```

### テストの実行

```bash
cd app
clojure -M -e "(require 'fizzbuzz.core-test) (clojure.test/run-tests 'fizzbuzz.core-test)"
```

## API

### `fizzbuzz [n]`

単一の数値に対してFizzBuzz変換を実行します。

```clojure
(fizzbuzz 1)   ; => "1"
(fizzbuzz 3)   ; => "Fizz"
(fizzbuzz 5)   ; => "Buzz"
(fizzbuzz 15)  ; => "FizzBuzz"
```

### `fizzbuzz-list [start end]`

指定された範囲の数値に対してFizzBuzz変換を実行し、リストで返します。

```clojure
(fizzbuzz-list 1 5)  ; => ("1" "2" "Fizz" "4" "Buzz")
```

### `print-fizzbuzz [start end]`

指定された範囲の数値に対してFizzBuzz変換を実行し、結果を出力します。

```clojure
(print-fizzbuzz 1 5)
; 1
; 2
; Fizz
; 4
; Buzz
```

## 学習ポイント

このプロジェクトを通じて以下を学ぶことができます：

1. **テスト駆動開発（TDD）の実践**
   - RED-GREEN-REFACTORサイクル
   - テストファーストアプローチ

2. **Clojureの基本概念**
   - 関数型プログラミング
   - 条件分岐（`cond`）
   - シーケンス操作（`map`, `range`）
   - 名前空間の管理

3. **ソフトウェア設計の原則**
   - 単一責任の原則
   - 段階的な機能拡張
   - リファクタリング

## 依存関係

- Clojure 1.11.1
- clojure.test（標準ライブラリ）
