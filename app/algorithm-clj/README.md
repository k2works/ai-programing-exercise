# algorithm-clj

アルゴリズムから始めるClojure入門プロジェクトです。

## 概要

このプロジェクトは、Clojureを使ってアルゴリズムの基本概念を学ぶための学習プロジェクトです。
テスト駆動開発（TDD）を実践しながら、様々なアルゴリズムを実装していきます。

## 環境構築

このプロジェクトにはLeiningenが必要です。

### セットアップ

```bash
# 依存関係のインストール
lein deps

# テストの実行
lein test

# REPLの開始
lein repl
```

## 使用方法

```bash
# アプリケーションの実行
lein run

# デモプログラムの実行（第1章の全関数を試す）
lein run -m algorithm-clj.demo

# 特定のテストの実行
lein test algorithm-clj.basic-algorithms.core-test
```

## 実装済みアルゴリズム

### 第1章: 基本的なアルゴリズム

- **3値の最大値**: `max3` - 3つの整数値の中から最大値を求める
- **3値の中央値**: `med3` - 3つの整数値の中央値を求める
- **符号判定**: `judge-sign` - 整数値の符号（正・負・ゼロ）を判定
- **1からnまでの総和**: `sum-1-to-n-while`, `sum-1-to-n-for` - 1からnまでの整数の総和を計算
- **記号文字の交互表示**: `alternative-1`, `alternative-2` - '+'と'-'を交互に表示
- **長方形の辺の長さを列挙**: `rectangle` - 指定された面積の長方形の辺の組み合わせを列挙
- **九九の表**: `multiplication-table` - 九九の表を表示
- **直角三角形の表示**: `triangle-lb` - 左下が直角の二等辺三角形を表示

## プロジェクト構造

```
src/algorithm_clj/
├── core.clj                      # メインアプリケーション
├── demo.clj                      # デモプログラム
├── basic_algorithms/
│   └── core.clj                  # 第1章: 基本的なアルゴリズム
└── algorithms/
    └── sorting.clj               # ソートアルゴリズム（今後実装予定）

test/algorithm_clj/
├── core_test.clj                 # メインのテスト
├── basic_algorithms/
│   └── core_test.clj             # 第1章のテスト
└── algorithms/
    └── sorting_test.clj          # ソートアルゴリズムのテスト
```

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2025 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
