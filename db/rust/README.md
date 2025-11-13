# 販売管理システム データベース（Rust + SQLx版）

実践データベース設計：TDDで育てる販売管理システム

## 概要

テスト駆動開発（TDD）の原則をデータベース設計に応用し、小さな要求から始めて販売管理システムのデータベースを段階的に育てていくプロジェクトです。

## 環境構築

### 前提条件

- Rust (最新の安定版を推奨)
- Cargo (Rustに同梱)
- Docker & Docker Compose（推奨）
- Git

**または**

- PostgreSQL (v14以上推奨) または MySQL (v8.0以上推奨)

### プロジェクトのセットアップ

1. 依存関係のインストール

```bash
cargo build
```

2. 環境変数の設定

```bash
cp .env.example .env
```

`.env` ファイルを編集して、データベース接続情報を設定してください。

3. SQLx CLIのインストール

PostgreSQLを使用する場合：

```bash
cargo install sqlx-cli --no-default-features --features postgres
```

MySQLを使用する場合：

```bash
cargo install sqlx-cli --no-default-features --features mysql
```

### Dockerを使用する場合

1. Dockerコンテナの起動

PostgreSQLを使用する場合：

```bash
docker-compose up -d postgres
```

MySQLを使用する場合：

```bash
docker-compose up -d mysql
```

Adminerも同時に起動する場合：

```bash
docker-compose up -d
```

2. データベースの作成

```bash
sqlx database create
```

3. マイグレーションの実行

```bash
sqlx migrate run
```

### テストの実行

```bash
cargo test
```

## 開発ワークフロー

1. **要求の明確化**: テストコードで要求を具体化
2. **テスト作成（レッド）**: 失敗するテストを書く
3. **マイグレーション作成**: SQLファイルでテーブル定義を記述
4. **マイグレーション実行**: `sqlx migrate run`
5. **実装（グリーン）**: テストがパスする最小限のコードを書く
6. **リファクタリング**: 設計を改善する
7. **コミット**: Gitでバージョン管理

## マイグレーション管理

### 新しいマイグレーションの作成

```bash
sqlx migrate add <migration_name>
```

### マイグレーションの実行

```bash
sqlx migrate run
```

### マイグレーションのロールバック

```bash
sqlx migrate revert
```

## データベース管理ツール

### Adminer

ブラウザからデータベースに接続できます。

```
http://localhost:8080
```

### SchemaSpy

データベーススキーマを可視化します。

```bash
docker-compose run --rm schemaspy
```

生成されたドキュメントは `docs/schemaspy` ディレクトリに出力されます。

## ライセンス

MIT License

## 参考資料

- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [SQLx Documentation](https://docs.rs/sqlx/latest/sqlx/)
- [PostgreSQL Documentation](https://www.postgresql.org/docs/)
- [MySQL Documentation](https://dev.mysql.com/doc/)
