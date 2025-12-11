# 会計システム - Rust + SQLx + Testcontainers

TDD でデータベース設計を進める会計システムの実装プロジェクトです。

## 技術スタック

- **言語**: Rust
- **Web フレームワーク**: Axum (高性能な非同期 Web フレームワーク)
- **アーキテクチャ**: ヘキサゴナルアーキテクチャ (Ports & Adapters)
- **データベースライブラリ**: SQLx (非同期、コンパイル時クエリ検証)
- **データベース**: PostgreSQL 16
- **テストフレームワーク**: Rust標準 + Testcontainers
- **非同期ランタイム**: Tokio
- **API ドキュメント**: OpenAPI 3.0 + Swagger UI
- **開発環境**: Docker Compose

## 前提条件

以下のツールがインストールされていることを確認してください。

- Rust (最新の安定版)
- Docker & Docker Compose
- SQLx CLI
- Just (タスクランナー) - オプショナル
- jq (JSON パーサー) - API テスト用、オプショナル

## セットアップ

### 1. 依存関係のインストール

```bash
# SQLx CLIのインストール
cargo install sqlx-cli --no-default-features --features postgres

# プロジェクトの依存関係をインストール
cargo build
```

### 2. データベース環境の起動

```bash
# PostgreSQLコンテナを起動
docker-compose up -d postgres

# Adminer（データベース管理ツール）を含むすべてのサービスを起動
docker-compose up -d
```

### 3. マイグレーションの実行

```bash
# マイグレーションを実行
sqlx migrate run
```

## テスト

```bash
# すべてのテストを実行
cargo test

# テストの詳細な出力を確認
cargo test -- --nocapture
```

**注意**: テストは Testcontainers を使用して、テストごとに独立した PostgreSQL コンテナを起動します。Docker が実行されている必要があります。

## API サーバー

### API サーバーの起動

#### 開発モード（推奨）

```bash
# just を使用する場合
just api-run

# または cargo を直接使用
cargo run --bin accounting-system
```

#### ウォッチモード（ファイル変更時に自動再起動）

```bash
# just を使用する場合
just api-dev

# または cargo-watch を使用
cargo watch -x 'run --bin accounting-system'
```

#### リリースモード

```bash
# just を使用する場合
just api-run-release

# または cargo を直接使用
cargo run --release --bin accounting-system
```

サーバーは `http://localhost:3000` で起動します。

### Swagger UI（API ドキュメント）

API サーバーが起動している状態で、以下の URL にアクセスできます：

- **Swagger UI**: http://localhost:3000/swagger-ui
- **OpenAPI JSON**: http://localhost:3000/api-docs/openapi.json

#### Swagger UI の使用方法

```bash
# API サーバーを起動
just api-run

# 別のターミナルで Swagger UI をブラウザで開く
just swagger-ui

# または Swagger UI を開く（URL とともに表示）
just swagger-open

# OpenAPI JSON 仕様を確認
just swagger-json
```

Swagger UI では以下のことができます：

- 📚 全 API エンドポイントの一覧表示
- 📝 各エンドポイントの詳細ドキュメント（パラメータ、レスポンス）
- 🧪 ブラウザから直接 API をテスト実行
- 💾 リクエスト/レスポンスのサンプル表示

### API エンドポイント

#### 勘定科目 API

- `GET /api/v1/accounts` - 全勘定科目の取得
- `GET /api/v1/accounts/:code` - 特定の勘定科目を取得
- `POST /api/v1/accounts` - 勘定科目の作成

#### 財務諸表 API

- `GET /api/v1/financial/balance-sheet?as_of_date=YYYY-MM-DD` - 貸借対照表の取得
- `GET /api/v1/financial/income-statement?start_date=YYYY-MM-DD&end_date=YYYY-MM-DD` - 損益計算書の取得
- `GET /api/v1/financial/ratios?as_of_date=YYYY-MM-DD&period_start=YYYY-MM-DD&period_end=YYYY-MM-DD` - 財務指標の取得

### API の使用例

```bash
# 全勘定科目を取得
just api-test-get-accounts

# 特定の勘定科目を取得（例: コード "1110"）
just api-test-get-account 1110

# 勘定科目を作成
just api-test-create-account

# 貸借対照表を取得（例: 2024-12-31 時点）
just api-test-balance-sheet 2024-12-31

# 損益計算書を取得（例: 2024-01-01 から 2024-12-31）
just api-test-income-statement 2024-01-01 2024-12-31

# 財務指標を取得（例: 2024-12-31 時点、期間 2024-01-01 から 2024-12-31）
just api-test-ratios 2024-12-31 2024-01-01 2024-12-31
```

または、curl を直接使用：

```bash
# 全勘定科目を取得
curl http://localhost:3000/api/v1/accounts | jq '.'

# 貸借対照表を取得
curl "http://localhost:3000/api/v1/financial/balance-sheet?as_of_date=2024-12-31" | jq '.'
```

## データベース管理

### Adminer へのアクセス

ブラウザで http://localhost:8080 にアクセスします。

- **システム**: PostgreSQL
- **サーバ**: postgres
- **ユーザ名**: postgres
- **パスワード**: postgres
- **データベース**: financial_accounting

### 便利なコマンド

```bash
# コンテナの停止
docker-compose stop

# コンテナの停止と削除
docker-compose down

# コンテナの停止、削除、ボリュームも削除
docker-compose down -v

# ログの確認
docker-compose logs -f postgres

# コンテナの状態確認
docker-compose ps

# PostgreSQLコンテナに接続
docker-compose exec postgres psql -U postgres -d financial_accounting
```

## プロジェクト構成

```
accounting-system/
├── Cargo.toml                      # プロジェクト設定と依存関係
├── justfile                        # タスクランナー設定
├── .env                            # 環境変数（データベース接続情報）
├── .env.example                    # 環境変数のサンプル
├── docker-compose.yml              # Docker Compose設定
├── docker/                         # Dockerコンテナ初期化スクリプト
│   ├── postgres/
│   │   └── init/
│   └── mysql/
│       ├── init/
│       └── conf.d/
├── migrations/                     # SQLxマイグレーションファイル
├── src/
│   ├── main.rs                    # API サーバーのエントリーポイント
│   ├── lib.rs                     # ライブラリのエントリーポイント
│   ├── application/               # Application 層
│   │   ├── ports/                 # Ports（入力・出力ポート）
│   │   │   ├── input/             # Input Ports（ユースケース）
│   │   │   └── output/            # Output Ports（リポジトリ）
│   │   ├── services/              # Application Services
│   │   ├── balance/               # 残高管理サービス
│   │   └── financial/             # 財務諸表サービス
│   ├── domain/                    # Domain 層
│   │   ├── account.rs             # 勘定科目エンティティ
│   │   ├── journal.rs             # 仕訳エンティティ
│   │   ├── balance/               # 残高ドメインモデル
│   │   └── financial/             # 財務諸表ドメインモデル
│   ├── infrastructure/            # Infrastructure 層
│   │   ├── web/                   # Web インターフェース（Input Adapters）
│   │   │   ├── handlers/          # HTTP ハンドラ
│   │   │   └── dtos/              # DTOs
│   │   └── persistence/           # データ永続化（Output Adapters）
│   │       └── repositories/      # リポジトリ実装
│   └── repositories/              # 既存のリポジトリ
├── tests/                         # 統合テスト
└── README.md                      # このファイル
```

### アーキテクチャ

このプロジェクトは**ヘキサゴナルアーキテクチャ**（Ports & Adapters）を採用しています：

- **Domain 層**: ビジネスロジックとドメインモデル
- **Application 層**: ユースケースとアプリケーションサービス
  - **Input Ports**: 外部からアプリケーションへの入力インターフェース
  - **Output Ports**: アプリケーションから外部への出力インターフェース
- **Infrastructure 層**: 技術的な実装詳細
  - **Input Adapters**: HTTP ハンドラ、CLI など
  - **Output Adapters**: データベースリポジトリ、外部 API クライアントなど

## 開発のワークフロー

1. **マイグレーションファイルの作成**
   ```bash
   sqlx migrate add <migration_name>
   ```

2. **マイグレーションの実行**
   ```bash
   sqlx migrate run
   ```

3. **テストの作成と実行**
   - `src/lib.rs` にテストを追加
   - `cargo test` でテストを実行

4. **コードフォーマット**
   ```bash
   cargo fmt
   ```

5. **リンターの実行**
   ```bash
   cargo clippy
   ```

## 実装状況

### 完了した章

- ✅ **第0章**: プロジェクトの初期化
  - SQLx のセットアップ
  - SQLx マイグレーションのセットアップ
  - Docker Compose のセットアップ
  - テスト環境のセットアップ（Testcontainers）

- ✅ **第1章**: 勘定科目マスタ
  - テーブル設計
  - CRUD 操作
  - 制約とバリデーション

- ✅ **第2章**: 仕訳と残高管理
  - 仕訳テーブル設計
  - 日次・月次残高管理
  - 自動仕訳機能

- ✅ **第3章**: 税務管理
  - 課税取引コードマスタ
  - 消費税計算

- ✅ **第4章**: 財務諸表生成
  - 貸借対照表（Balance Sheet）
  - 損益計算書（Income Statement）
  - 財務指標（Financial Ratios）
  - 財務分析レポート

- ✅ **第5章**: ヘキサゴナルアーキテクチャによる API 実装
  - Ports & Adapters パターン
  - 勘定科目 API
  - 財務諸表 API
  - RESTful エンドポイント

### Just コマンド一覧

開発を効率化するため、以下の Just コマンドを提供しています：

```bash
# 開発環境
just setup              # 初回セットアップ
just db-up              # データベース起動
just db-down            # データベース停止
just db-reset           # データベースリセット
just migrate            # マイグレーション実行

# API サーバー
just api-run            # API サーバー起動
just api-dev            # API サーバー起動（ウォッチモード）
just api-run-release    # API サーバー起動（リリースビルド）

# Swagger UI
just swagger-ui         # Swagger UI をブラウザで開く
just swagger-open       # Swagger UI を開く（URL 表示付き）
just swagger-json       # OpenAPI JSON 仕様を取得

# API テスト
just api-test-get-accounts              # 全勘定科目取得
just api-test-get-account <code>        # 特定の勘定科目取得
just api-test-create-account            # 勘定科目作成
just api-test-balance-sheet <date>      # 貸借対照表取得
just api-test-income-statement <start> <end>  # 損益計算書取得
just api-test-ratios <date> <start> <end>     # 財務指標取得

# 品質チェック
just test               # テスト実行
just fmt                # フォーマット適用
just lint               # リンターチェック
just ci                 # CI チェック（fmt + lint + test）

# ビルド
just build              # デバッグビルド
just build-release      # リリースビルド
just clean              # ビルド成果物削除
```
