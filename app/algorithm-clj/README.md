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

# 第2章 配列のデモプログラムの実行
lein run -m algorithm-clj.array-demo

# 第3章 探索アルゴリズムのデモプログラムの実行
lein run -m algorithm-clj.search-demo

# 第4章 スタックとキューのデモプログラムの実行
lein run -m algorithm-clj.stack-queue-demo

# 第5章 再帰アルゴリズムのデモプログラムの実行
lein run -m algorithm-clj.recursion-demo

# 特定のテストの実行
lein test algorithm-clj.basic-algorithms.core-test
lein test algorithm-clj.basic-algorithms.array-test
lein test algorithm-clj.basic-algorithms.search-test
lein test algorithm-clj.basic-algorithms.stack-queue-test
lein test algorithm-clj.basic-algorithms.recursion-test
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

### 第2章: 配列

- **5人の点数から合計と平均**: `calculate-scores-individual` - 個別引数での計算
- **配列の要素の最大値**: `max-of` - シーケンスの最大値を求める
- **配列の要素の並びを反転**: `reverse-vector` - ベクターの要素を反転
- **基数変換**: `card-conv` - 10進数を指定された基数に変換
- **素数の列挙**: `prime1`, `prime2`, `prime3` - 3つの異なる効率レベルの素数列挙アルゴリズム

### 第3章: 探索アルゴリズム

- **線形探索**: `ssearch-while`, `ssearch-for`, `ssearch-sentinel` - 3つの実装方式による線形探索
- **二分探索**: `bsearch` - ソート済み配列での効率的な探索
- **ハッシュ法（チェイン法）**: `ChainedHash` - 連鎖リストによる衝突解決
- **ハッシュ法（オープンアドレス法）**: `OpenHash` - 線形探査による衝突解決

### 第4章: スタックとキュー

- **固定長スタック**: `FixedStack` - 配列ベースのLIFO構造、プッシュ・ポップ・ピーク操作
- **ArrayDeque スタック**: `ArrayDeque` - Java標準ライブラリを使った効率的なスタック実装
- **固定長キュー**: `FixedQueue` - リングバッファを使ったFIFO構造、エンキュー・デキュー・ピーク操作  
- **ArrayDeque キュー**: `ArrayDeque` - Java標準ライブラリを使った効率的なキュー実装

### 第5章: 再帰アルゴリズム

- **階乗**: `factorial` - 再帰とループによる階乗計算
- **ユークリッドの互除法**: `gcd` - 最大公約数の計算
- **再帰関数**: `recure`, `recure-iterative` - 再帰の仕組みを理解するための例
- **ハノイの塔**: `move-hanoi`, `count-hanoi-moves` - 円盤の移動と移動回数の計算
- **8王妃問題**: `solve-8-queens`, `solve-n-queens` - バックトラッキングによるn王妃問題の解法
- **王妃の配置**: `put-queens-all-combinations`, `put-queens-row-col-constraint` - 制約条件を考慮した王妃の配置

## プロジェクト構造

```
src/algorithm_clj/
├── core.clj                      # メインアプリケーション
├── demo.clj                      # 第1章デモプログラム
├── array_demo.clj                # 第2章デモプログラム
├── search_demo.clj               # 第3章デモプログラム
├── stack_queue_demo.clj          # 第4章デモプログラム
├── recursion_demo.clj            # 第5章デモプログラム
├── debug_prime.clj               # 素数デバッグユーティリティ
├── basic_algorithms/
│   ├── core.clj                  # 第1章: 基本的なアルゴリズム
│   ├── array.clj                 # 第2章: 配列
│   ├── search.clj                # 第3章: 探索アルゴリズム
│   ├── stack_queue.clj           # 第4章: スタックとキュー
│   └── recursion.clj             # 第5章: 再帰アルゴリズム
└── algorithms/
    └── sorting.clj               # ソートアルゴリズム（今後実装予定）

test/algorithm_clj/
├── core_test.clj                 # メインのテスト
├── basic_algorithms/
│   ├── core_test.clj             # 第1章のテスト
│   ├── array_test.clj            # 第2章のテスト
│   ├── search_test.clj           # 第3章のテスト
│   ├── stack_queue_test.clj      # 第4章のテスト
│   └── recursion_test.clj        # 第5章のテスト
└── algorithms/
    └── sorting_test.clj          # ソートアルゴリズムのテスト
```

## テスト結果

現在のテスト状況:
- **総テスト数**: 43テスト
- **総アサーション数**: 305アサーション  
- **テスト結果**: 全テスト成功 ✅ (100%成功)

### 実装の特徴

- **関数型プログラミング**: Clojureの不変データ構造を活用
- **テスト駆動開発**: 各アルゴリズムに対する包括的なテストケース
- **効率的な実装**: loop/recurによる最適化された反復処理
- **データ構造活用**: レコードとアトムを使ったハッシュテーブル実装

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
