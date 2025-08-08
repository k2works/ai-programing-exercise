# アルゴリズムから始めるHaskell入門

Haskellを使ったアルゴリズム学習のためのプロジェクトです。テスト駆動開発（TDD）を実践しながら、Haskellの基本的な文法とアルゴリズムを学習します。

## 特徴

- **テスト駆動開発**: hspecを使用したテストファーストのアプローチ
- **静的コード解析**: HLintによるコード品質チェック
- **自動フォーマット**: Ormoluによるコード整形
- **継続的インテグレーション**: GitHub Actionsによる自動テスト

## セットアップ

### 必要なツール

- [Stack](https://docs.haskellstack.org/en/stable/README/)

### インストール

```bash
# 依存関係のインストール
make setup

# プロジェクトのビルド
make build
```

## 使用方法

### テストの実行

```bash
make test
```

### 静的コード解析

```bash
make lint
```

### コードフォーマット

```bash
make format
```

### カバレッジ付きテスト

```bash
make coverage
```

### 循環複雑度解析

```bash
# 基本的な複雑度解析
make complexity

# 詳細レポート（リスク分析・推奨事項付き）
make complexity-report
```

### アプリケーションの実行

```bash
# アルゴリズムから始めるHaskell入門

## 概要

このプロジェクトは、Haskellを使ってアルゴリズムを学ぶための教材プロジェクトです。テスト駆動開発（TDD）の手法を用いて、基本的なアルゴリズムから始めて段階的に学習を進めていきます。

## プロジェクトの特徴

- **テスト駆動開発（TDD）**: まずテストを書き、そのテストが通るようにコードを実装
- **関数型プログラミング**: Haskellの強力な型システムと関数型の概念を活用
- **品質管理**: 静的解析、コードカバレッジ、循環複雑度測定を含む総合的な品質管理
- **CI/CD**: GitHub Actionsによる自動化されたビルドとテスト

## 実装済みアルゴリズム

### 第1章: 基本的なアルゴリズム

1. **3値の最大値** (`max3`)
   - 3つの値から最大値を求める
   - Haskellの標準ライブラリ`max`を活用

2. **3値の中央値** (`med3`)
   - 3つの値の中央値を求める
   - ガード構文を使った条件分岐

3. **符号判定** (`judgeSign`)
   - 整数値の符号を判定
   - 型多相とガード構文の組み合わせ

4. **1からnまでの総和** (`sum1ToN`)
   - リスト内包表記と`sum`関数の活用
   - 範囲演算子`[1..n]`の使用

5. **記号文字の交互表示**
   - `alternative1`: `cycle`関数を使った無限リスト
   - `alternative2`: リスト内包表記とパリティ判定

6. **長方形の辺の長さ列挙** (`rectangle`)
   - 約数の効率的な探索
   - 平方根までの探索で計算量削減

7. **九九の表** (`multiplicationTable`)
   - ネストしたリスト内包表記
   - `printf`を使ったフォーマット出力

8. **直角三角形表示** (`triangleLb`)
   - `replicate`関数の活用
   - `unlines`による改行連結

### 既存実装

- **FizzBuzz**: テスト駆動開発の基本例として実装済み

## 開発環境

### 必要なツール

- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskellビルドツール)
- GHC (Glasgow Haskell Compiler)
- HLint (静的解析ツール)

### インストール

```bash
# Stackのインストール (Ubuntu/Debian)
curl -sSL https://get.haskellstack.org/ | sh

# 依存関係のインストール
stack setup
stack build
```

## 使い方

### プロジェクトのビルド

```bash
make build
```

### テストの実行

```bash
make test
```

### アプリケーションの実行

```bash
make run
```

実行すると、実装済みの全アルゴリズムのデモンストレーションが表示されます。

### 品質チェック

```bash
# 静的解析
make lint

# コードカバレッジ
make coverage

# 循環複雑度測定  
make complexity

# 全体的な品質チェック
make check
```

### 開発用コマンド

```bash
# ファイル変更時の自動再実行
make watch

# 対話的実行環境
make repl

# ヘルプ表示
make help
```

## 品質メトリクス

### 現在の品質状況

- **テストカバレッジ**: 99% (143/144 expressions)
- **循環複雑度**: 
  - 総合: 60
  - 平均: 8.57
  - 最大: 23
- **静的解析**: HLintによるチェック適用済み

### コード品質の特徴

- **型安全性**: Haskellの強力な型システムによる実行時エラーの防止
- **純粋関数**: 副作用のない関数による予測可能な動作
- **宣言的コード**: 問題の「何を」に焦点を当てた読みやすいコード
- **再利用性**: 高階関数とジェネリクスによる柔軟なコード

## プロジェクト構造

```
.
├── app/
│   └── Main.hs                 # メインアプリケーション
├── src/
│   ├── BasicAlgorithms.hs     # 基本アルゴリズム実装
│   ├── FizzBuzz.hs            # FizzBuzz実装
│   └── Lib.hs                 # ライブラリモジュール
├── test/
│   ├── BasicAlgorithmsSpec.hs # 基本アルゴリズムテスト
│   ├── FizzBuzzSpec.hs        # FizzBuzzテスト
│   └── Spec.hs                # テスト設定
├── complexity-analyzer.hs      # 循環複雑度分析ツール
├── complexity-report.hs       # 詳細複雑度レポート
├── Makefile                   # ビルド自動化
├── package.yaml               # プロジェクト設定
└── stack.yaml                 # Stack設定
```

## 学習の進め方

1. **基礎から学ぶ**: まずは第1章の基本アルゴリズムから開始
2. **テストを読む**: 各アルゴリズムのテストケースを確認して仕様を理解
3. **実装を解析**: Haskellの関数型プログラミングの特徴を理解
4. **品質を測定**: 各種メトリクスでコード品質を確認

## 技術スタック

- **言語**: Haskell (GHC 9.10.2)
- **ビルドツール**: Stack
- **テストフレームワーク**: Hspec + QuickCheck
- **静的解析**: HLint + Ormolu
- **CI/CD**: GitHub Actions
- **品質測定**: HPC (コードカバレッジ) + カスタム複雑度分析

## 参考文献

- "Algorithm Design with Haskell" - Richard Bird, Jeremy Gibbons
- 『すごいHaskellたのしく学ぼう！』 - Miran Lipovača
- 『テスト駆動開発』 - Kent Beck
- [Haskell公式ドキュメント](https://www.haskell.org/documentation/)

## ライセンス

BSD3
```

## プロジェクト構成

```
├── app/                # 実行可能ファイル
├── src/                # ライブラリソースコード
├── test/               # テストファイル
├── package.yaml        # パッケージ設定
├── stack.yaml          # Stack設定
├── Makefile            # タスクランナー
└── .github/workflows/  # CI/CD設定
```

## 学習トピック

- [ ] FizzBuzz実装（TDD実践）
- [ ] ソート アルゴリズム
- [ ] 探索アルゴリズム
- [ ] データ構造（リスト、ツリー、グラフ）
- [ ] 関数型プログラミングパターン

## ライセンス

BSD3
