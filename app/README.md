# テスト駆動開発から始めるRust入門

## ソフトウェア開発の三種の神器を準備する

このプロジェクトは「テスト駆動開発から始めるRuby入門」をRustで再実装したものです。
ソフトウェア開発の三種の神器（バージョン管理、テスティング、自動化）をRustの環境で実践します。

### プロジェクト構成

```
.
├── Cargo.toml         # プロジェクト設定と依存関係
├── Makefile           # タスクランナー
├── rustfmt.toml       # コードフォーマット設定
├── tarpaulin.toml     # コードカバレッジ設定
├── scripts/           # 自動化スクリプト
│   └── watch.sh       # 自動監視スクリプト
└── src/               # ソースコード
    ├── lib.rs         # FizzBuzz実装とテスト
    └── main.rs        # CLIインターフェイス
```

### 前提条件

- Rust（最新の安定版）
- Cargo（Rustのパッケージマネージャ）
- 必要に応じて以下のツール：
  - cargo-watch：ファイル変更監視
  - cargo-tarpaulin：コードカバレッジ測定
  - rustfmt：コードフォーマッタ
  - clippy：静的コード解析

### セットアップ

```bash
# 必要なツールのインストール
cargo install cargo-watch
cargo install cargo-tarpaulin
rustup component add clippy
rustup component add rustfmt
```

### 使い方

#### FizzBuzzプログラムの実行

```bash
# 単一の数値でFizzBuzzを実行
cargo run -- generate 15  # "FizzBuzz"を表示

# 1から100までのFizzBuzzリストを表示
cargo run -- list
```

#### 開発タスク

```bash
# Makefileを使用したタスク実行
make build      # ビルド
make test       # テスト実行
make format     # コードフォーマット
make lint       # 静的解析
make coverage   # カバレッジレポート生成
make watch      # 自動監視モード（変更を検知して自動テスト）
make clean      # クリーンアップ
make doc        # ドキュメント生成
```

### テスト駆動開発

このプロジェクトはテスト駆動開発（TDD）の原則に従って実装されています。
「Red-Green-Refactor」のサイクルを実践し、テストファーストのアプローチで機能を開発しています。

### 自動化

ファイル変更を監視して自動的にテスト、静的解析、カバレッジを実行するためには：

```bash
make watch
```

### 学習リソース

- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [テスト駆動開発](https://www.amazon.co.jp/dp/4274217884)
