# FizzBuzz Rust Implementation

Rustで実装したFizzBuzzアプリケーション。テスト駆動開発とオブジェクト指向設計の実践例です。

## 概要

このプロジェクトは「テスト駆動開発から始めるRust入門3 ~6時間でオブジェクト指向のエッセンスを体験する~」の実装例です。段階的にオブジェクト指向プログラミングの要素を取り入れながら、保守性の高いFizzBuzzアプリケーションを構築しています。

## 特徴

- 🦀 **Rust言語**: 安全性とパフォーマンスを両立
- 🧪 **テスト駆動開発**: 40個以上のテストケースによる品質保証
- 🏗️ **レイヤードアーキテクチャ**: ドメインとアプリケーション層の分離
- 🎯 **デザインパターン**: Command、Strategy、Value Objectパターンの活用
- ⚡ **型安全性**: Rustの型システムによるコンパイル時エラー検出
- 🔧 **モジュール設計**: 明確な責任分離と再利用可能な設計

## アーキテクチャ

```
src/
├── lib.rs              # ライブラリルート
├── main.rs             # CLIエントリーポイント
├── application/        # アプリケーション層
│   ├── mod.rs
│   ├── fizz_buzz_value_command.rs
│   └── fizz_buzz_list_command.rs
└── domain/             # ドメイン層
    ├── mod.rs
    ├── model/          # ドメインモデル
    │   ├── mod.rs
    │   ├── fizz_buzz_value.rs
    │   └── fizz_buzz_list.rs
    └── type/           # FizzBuzzタイプ実装
        ├── mod.rs
        ├── fizz_buzz_type.rs
        ├── fizz_buzz_type_01.rs
        ├── fizz_buzz_type_02.rs
        └── fizz_buzz_type_03.rs
```

### 層の責任

- **Application Layer**: ユースケースの実装とコマンドパターンによる処理制御
- **Domain Layer**: ビジネスロジックと値オブジェクトの実装

## 使用方法

### 前提条件

- Rust 1.70以上
- Cargo

### ビルド

```bash
cargo build
```

### テスト実行

```bash
# 全テストの実行
cargo test

# 詳細出力でテスト実行
cargo test -- --nocapture

# 特定のテストファイルを実行
cargo test fizz_buzz_tests
```

### 開発タスク（cargo-make使用）

このプロジェクトでは `cargo-make` を使用して開発タスクを自動化しています。

#### セットアップ

```bash
# cargo-makeのインストール
cargo install cargo-make

# 必要な開発ツールのインストール
cargo install cargo-tarpaulin  # カバレッジレポート用
cargo install cargo-watch      # ファイル監視用
```

#### 基本タスク

```bash
# 開発モード（ファイル変更を監視して自動でテスト・フォーマット・Lint実行）
cargo make

# または明示的に
cargo make watch

# 全てのチェックを実行（フォーマット確認、Lint、テスト）
cargo make check-all

# コードの自動修正（フォーマット + Lint）
cargo make fix
```

#### 個別タスク

```bash
# テスト実行
cargo make test

# コードフォーマット
cargo make format

# フォーマットチェック（CIで使用）
cargo make format-check

# Lint（警告をエラーとして扱う）
cargo make lint

# カバレッジレポート生成
cargo make coverage
```

#### 推奨開発フロー

```bash
# 1. 開発開始時（ファイル監視モードで自動チェック）
cargo make watch

# 2. コミット前の最終チェック
cargo make check-all

# 3. コード品質の修正
cargo make fix
```

### アプリケーション実行

```bash
# ライブラリとして使用
cargo run --bin fizz_buzz

# 特定の数値でFizzBuzz判定
# プログラム内でFizzBuzzValueCommandを使用
```

### ライブラリとしての使用例

```rust
use fizz_buzz::{FizzBuzzValueCommand, FizzBuzzListCommand};

fn main() {
    // 単一値の生成
    let command = FizzBuzzValueCommand::new(1);
    match command.execute(15) {
        Ok(value) => println!("{}: {}", value.number, value.value),
        Err(e) => eprintln!("Error: {}", e),
    }

    // リストの生成
    let list_command = FizzBuzzListCommand::new(1, 100);
    match list_command.execute() {
        Ok(list) => {
            for item in &list.value {
                println!("{}: {}", item.number, item.value);
            }
        }
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

## 実装されたパターン

### 1. Commandパターン
- `FizzBuzzValueCommand`: 単一値の生成コマンド
- `FizzBuzzListCommand`: リスト生成コマンド

### 2. Strategyパターン
- `FizzBuzzType01`: 基本的なif-else実装
- `FizzBuzzType02`: 文字列結合による実装
- `FizzBuzzType03`: 分離された判定ロジック

### 3. Value Objectパターン
- `FizzBuzzValue`: 数値と文字列のペア
- `FizzBuzzList`: FizzBuzzValueのコレクション

## テスト構成

### 単体テスト（Unit Tests）
- 各モジュール内にテストコードを配置
- ドメインロジックの詳細なテストケース
- エラーハンドリングのテスト

### 統合テスト（Integration Tests）
- `tests/`ディレクトリ内のend-to-endテスト
- アプリケーション全体の動作確認

### テストカバレッジ

```bash
# カバレッジレポート生成（tarpaulinが必要）
cargo install cargo-tarpaulin
cargo tarpaulin --out Html

# cargo-makeを使用する場合
cargo make coverage
```

カバレッジレポートは `tarpaulin-report.html` として生成されます。

## 開発の段階

このプロジェクトは以下の段階で開発されました：

1. **Phase 1**: 基本的なFizzBuzz実装（手続き型）
2. **Phase 2**: オブジェクト指向化（構造体、トレイト）
3. **Phase 3**: デザインパターンの導入
4. **Phase 4**: モジュール分割とアーキテクチャ設計
5. **Phase 5**: エラーハンドリングと品質向上

## 学習ポイント

### Rustでのオブジェクト指向プログラミング

- **トレイト（Trait）**: 継承の代わりにコンポジションを促進
- **所有権システム**: メモリ安全性の自動保証
- **Result型**: 例外安全なエラーハンドリング
- **パターンマッチング**: 安全な分岐処理

### テスト駆動開発

- Red-Green-Refactorサイクルの実践
- 段階的な機能追加
- リファクタリングによる設計改善

## 関連記事

詳細な実装過程は以下の記事で解説しています：
- [テスト駆動開発から始めるRust入門3 ~6時間でオブジェクト指向のエッセンスを体験する~](../docs/wiki/記事/テスト駆動開発から始めるRust入門3.md)

## ライセンス

MIT License

## 貢献

プルリクエストやIssueは歓迎します。以下のガイドラインに従ってください：

### 開発環境のセットアップ

```bash
# 必要なツールのインストール
cargo install cargo-make cargo-tarpaulin cargo-watch

# リポジトリのクローン
git clone https://github.com/k2works/ai-programing-exercise.git
cd ai-programing-exercise/app

# 開発開始
cargo make watch
```

### コントリビューションの手順

1. **開発開始**: `cargo make watch` でファイル監視モードを起動
2. **コード変更**: テスト駆動開発でコードを修正
3. **品質チェック**: `cargo make check-all` で全体をチェック
4. **自動修正**: `cargo make fix` でフォーマットとLintを適用
5. **最終確認**: テストが通ることを確認

### 品質基準

- テストが通ることを確認（`cargo make test`）
- コードフォーマットを適用（`cargo make format`）
- Clippyの警告を解決（`cargo make lint`）
- カバレッジレポートを確認（`cargo make coverage`）

## 参考資料

- [Rust公式ドキュメント](https://doc.rust-lang.org/)
- [テスト駆動開発](https://www.amazon.co.jp/dp/4274217884)
- [オブジェクト指向入門](https://www.amazon.co.jp/dp/4798111112)
