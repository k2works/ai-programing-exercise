# 財務会計システム - Rust + SQLx + Testcontainers

TDD でデータベース設計を進める財務会計システムの実装プロジェクトです。

## 技術スタック

- **言語**: Rust
- **データベースライブラリ**: SQLx (非同期、コンパイル時クエリ検証)
- **データベース**: PostgreSQL 16
- **テストフレームワーク**: Rust標準 + Testcontainers
- **非同期ランタイム**: Tokio
- **開発環境**: Docker Compose

## 前提条件

以下のツールがインストールされていることを確認してください。

- Rust (最新の安定版)
- Docker & Docker Compose
- SQLx CLI

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
financial-accounting-system/
├── Cargo.toml              # プロジェクト設定と依存関係
├── .env                    # 環境変数（データベース接続情報）
├── .env.example            # 環境変数のサンプル
├── docker-compose.yml      # Docker Compose設定
├── docker/                 # Dockerコンテナ初期化スクリプト
│   ├── postgres/
│   │   └── init/
│   └── mysql/
│       ├── init/
│       └── conf.d/
├── migrations/             # SQLxマイグレーションファイル
├── src/
│   └── lib.rs             # ライブラリのエントリーポイント
└── README.md              # このファイル
```

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

## 第0章完了

- ✅ プロジェクトの初期化
- ✅ SQLx のセットアップ
- ✅ SQLx マイグレーションのセットアップ
- ✅ Docker Compose のセットアップ
- ✅ テスト環境のセットアップ（Testcontainers）

次は第1章で実際のテーブル定義とテストを TDD で進めていきます。
