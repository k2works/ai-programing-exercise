# FizzBuzz in Clojure

テスト駆動開発（TDD）を用いてClojureで実装したFizzBuzzプログラムです。関数型プログラミングの特性を活かした実装となっています。

## 概要

このプロジェクトは「テスト駆動開発から始めるRuby入門」の内容をClojureで実装したものです。以下の特徴があります：

- **テスト駆動開発（TDD）**: レッド・グリーン・リファクタリングのサイクルを遵守
- **関数型プログラミング**: Clojureの関数型の特性を活用
- **高品質コード**: 100%のテストカバレッジとコード品質ツールの活用
- **ソフトウェア開発の三種の神器**: バージョン管理・テスティング・自動化を完備

## FizzBuzzルール

1から100までの数をプリントするプログラムで、以下のルールに従います：

- 3の倍数の場合は「Fizz」
- 5の倍数の場合は「Buzz」  
- 3と5の両方の倍数（15の倍数）の場合は「FizzBuzz」
- それ以外は数値をそのまま文字列として返す

## 実装アプローチ

### 2つの実装方式

1. **条件分岐による実装** (`fizzbuzz-rule`)
   - `cond`文を使った明示的な条件分岐
   - `partial`と`every-pred`を活用した関数合成

2. **文字列結合による実装** (`fizzbuzz-rule-v2`)
   - 条件に応じて文字列を結合する関数型アプローチ
   - より柔軟な拡張性を持つ設計

### 関数型プログラミングの特性

- **純粋関数**: 副作用のない関数設計
- **高階関数**: `map`, `partial`, `every-pred`の活用
- **遅延評価**: 無限シーケンスによる効率的な処理
- **イミュータビリティ**: データの不変性を保持

## セットアップ

### 前提条件

- Java 8以上
- Leiningen 2.0以上

### 依存関係のインストール

```bash
lein deps
```

## 使用方法

### REPLでの実行

```bash
lein repl
```

```clojure
;; 単一の数値を変換
(generate 15)  ; => "FizzBuzz"
(generate 3)   ; => "Fizz"
(generate 5)   ; => "Buzz"
(generate 7)   ; => "7"

;; 範囲指定でシーケンス生成
(fizzbuzz-sequence 15)     ; => ["1" "2" "Fizz" "4" "Buzz" ...]
(fizzbuzz-sequence 10 15)  ; => ["Buzz" "11" "Fizz" "13" "14" "FizzBuzz"]

;; 無限シーケンス（遅延評価）
(take 10 (fizzbuzz-lazy-seq))  ; => ("1" "2" "Fizz" "4" "Buzz" ...)

;; 1から100までプリント
(print-fizzbuzz)
```

### コマンドラインでの実行

```bash
# すべてのタスクを確認
make help

# テスト実行
make test

# 全チェック（lint + test + coverage）
make check

# コードフォーマット
make format

# カバレッジ測定
make coverage
```

## 開発

### テスト実行

```bash
lein test
# または
make test
```

### コード品質チェック

```bash
# 静的解析
lein lint
make lint

# フォーマットチェック
lein cljfmt check

# フォーマット自動修正
lein format
make format

# コードカバレッジ
lein coverage
make coverage
```

### 継続的テスト

ファイル変更を監視してテストを自動実行（`entr`が必要）:

```bash
make watch
```

## プロジェクト構造

```
fizzbuzz/
├── src/fizzbuzz/
│   └── core.clj           # メイン実装
├── test/fizzbuzz/
│   └── core_test.clj      # テストコード
├── project.clj            # プロジェクト設定とタスク定義
├── Makefile              # タスクランナー
└── README.md             # このファイル
```

## 開発プロセス

このプロジェクトは以下の開発プロセスに従って作成されました：

1. **TODOリスト作成**: 要件を小さなタスクに分解
2. **テストファースト**: 実装前にテストを作成
3. **レッド・グリーン・リファクタリング**: TDDサイクルの実践
4. **仮実装→三角測量**: 段階的な実装の一般化
5. **継続的リファクタリング**: コード品質の継続的改善

## コード品質

- **テストカバレッジ**: 100%
- **静的解析**: エラーなし
- **コードフォーマット**: 標準準拠
- **関数型設計**: 副作用なし、イミュータブル

## ライセンス

Copyright © 2025

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
