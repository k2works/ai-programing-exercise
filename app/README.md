# FizzBuzz ClojureScript Application

このプロジェクトは、ClojureScriptとshadow-cljsを使用して構築されたFizzBuzzアプリケーションです。
テスト駆動開発（TDD）のアプローチに従って開発されています。

## 前提条件

- Node.js (v14以降)
- npm または yarn

## セットアップ

1. 依存関係をインストール:
```bash
npm install
```

## 開発

### 開発サーバーの起動

```bash
npm run watch
```

これにより、shadow-cljsの開発サーバーが起動し、ファイルの変更を監視して自動的にコンパイルを行います。

### ブラウザでの確認

`public/index.html` をブラウザで開くか、ローカルサーバーを使用してアクセスしてください。

### テストの実行

```bash
npm test
```

## ビルド

### 開発ビルド

```bash
npm run build
```

### 本番ビルド

```bash
npm run release
```

## プロジェクト構造

```
app/
├── package.json          # Node.js依存関係とスクリプト
├── shadow-cljs.edn       # shadow-cljs設定
├── public/
│   └── index.html        # メインHTMLファイル
├── src/
│   └── fizzbuzz/
│       └── core.cljs     # メインアプリケーションコード
└── test/
    └── fizzbuzz/
        └── core_test.cljs # テストコード
```

## 機能

- **FizzBuzz生成**: 指定された範囲でFizzBuzzを生成
- **インタラクティブUI**: ブラウザ上で範囲を指定して結果を表示
- **カラーコーディング**: Fizz、Buzz、FizzBuzz、数字それぞれに異なる色を適用
- **テスト駆動開発**: 包括的なテストスイート

## TDDアプローチ

このアプリケーションは以下のTDDサイクルに従って開発されました：

1. **RED**: 失敗するテストを書く
2. **GREEN**: テストを通す最小限の実装
3. **REFACTOR**: コードの改善

### 実装されたテストケース

- 基本的な数値から文字列変換
- 3の倍数での"Fizz"変換
- 5の倍数での"Buzz"変換  
- 15の倍数での"FizzBuzz"変換
- リスト生成機能

## 技術スタック

- **ClojureScript**: Clojureのブラウザ版、関数型プログラミング言語
- **shadow-cljs**: ClojureScript用の高機能ビルドツール
- **cljs.test**: ClojureScript標準のテスティングフレームワーク

## 学習ポイント

このプロジェクトを通じて以下を学習できます：

- ClojureScriptの基本的な文法と概念
- shadow-cljsを使った開発環境の構築
- テスト駆動開発の実践
- 関数型プログラミングのアプローチ
- ブラウザでのClojureScript実行
