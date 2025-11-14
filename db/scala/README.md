# 実践データベース設計：TDDで育てる販売管理システム（Scala版）

## 概要

本プロジェクトは、テスト駆動開発（TDD）の原則をデータベース設計に応用し、
小さな要求から始めて販売管理システムのデータベースを段階的に育てていく、
極めて実践的なプロセスを追体験するためのものです。

## 技術スタック

- **Scala**: 3.3.3
- **sbt**: 1.9.7
- **ScalikeJDBC**: 4.2.1（データベースアクセス）
- **Flyway**: 10.4.1（データベースマイグレーション）
- **PostgreSQL**: 15-alpine（データベース）
- **Testcontainers**: 0.41.0（テスト環境）
- **ScalaTest**: 3.2.18（テストフレームワーク）

## セットアップ

### 前提条件

以下のツールがインストールされていることを確認してください。

- Scala 3.3.3以上
- sbt 1.9.0以上
- Java Development Kit (JDK) 11以上
- Docker & Docker Compose
- just (コマンドランナー、オプション)

### 初期セットアップ

```bash
# プロジェクトディレクトリに移動
cd db/scala

# 依存関係のダウンロード
sbt update

# Dockerコンテナ起動（開発用）
docker compose up -d

# マイグレーション実行
sbt migrate
```

または just を使用：

```bash
just setup
```

## 開発コマンド

### テスト

```bash
# テスト実行（testcontainersが自動でPostgreSQLコンテナを起動）
sbt test

# ファイル監視モードでテスト
sbt "~ test"

# または
just test
just watch
```

### コード品質

```bash
# コードフォーマット
sbt format

# フォーマットチェック
sbt formatCheck

# 静的解析
sbt lint

# 全品質チェック
sbt check

# カバレッジレポート生成
sbt coverage

# または
just format
just lint
just check
just coverage
```

### データベース

```bash
# Dockerコンテナ起動（開発用）
docker compose up -d

# マイグレーション実行
sbt migrate

# マイグレーション情報表示
sbt migrateInfo

# マイグレーション検証
sbt migrateValidate

# または
just docker-up
just migrate
just migrate-info
```

### CI/CD

```bash
# CI用完全チェック
sbt ci

# または
just ci
```

## プロジェクト構成

```
db/scala/
├── build.sbt                    # sbtビルド定義
├── project/
│   ├── build.properties        # sbtバージョン
│   └── plugins.sbt             # sbtプラグイン
├── src/
│   ├── main/
│   │   ├── scala/
│   │   │   └── com/example/db/
│   │   │       ├── DatabaseConfig.scala      # DB接続設定
│   │   │       └── FlywayMigration.scala     # マイグレーション管理
│   │   └── resources/
│   │       ├── application.conf              # アプリケーション設定
│   │       └── db/migration/
│   │           └── V1__Create_schema_version_table.sql
│   └── test/
│       └── scala/
│           └── com/example/db/
│               ├── DatabaseSpec.scala         # テスト基底クラス
│               ├── DatabaseConfigSpec.scala   # DB接続テスト
│               └── FlywayMigrationSpec.scala  # マイグレーションテスト
├── docker-compose.yml           # Docker設定
├── docker/
│   └── postgres/
│       └── init/
│           └── 01_init.sql     # DB初期化スクリプト
├── .gitignore
├── .scalafmt.conf              # フォーマット設定
├── justfile                    # タスク定義
└── README.md

```

## 開発ワークフロー

### TDD サイクル

1. **Red（失敗するテスト）**: 要求をテストコードで表現
2. **Green（最小限の実装）**: テストをパスする最小限のコードを書く
3. **Refactor（設計の改善）**: コードをきれいにする

### 典型的な開発フロー

```bash
# 1. 開発環境のセットアップ
just setup

# 2. ターミナル1: ファイル監視でテスト自動実行
sbt "~ test"

# 3. ターミナル2: コーディング
# コードを編集・保存すると自動でテストが実行される

# 4. コミット前のチェック
sbt check

# 5. コミット
git add .
git commit -m "feat: 新機能追加"
```

## Testcontainersについて

このプロジェクトでは、テストに **Testcontainers** を使用しています。

### 利点

- ✅ **完全な分離**: 各テストスイートが独立したDBインスタンスを持つ
- ✅ **本番環境に近い**: 実際のPostgreSQLを使用（モックではない）
- ✅ **クリーンな状態**: 毎回新しいDBでテスト開始
- ✅ **並列実行可能**: 複数のテストスイートを同時実行可能
- ✅ **CI/CD対応**: Docker環境があればどこでも実行可能

### 使い分け

| 用途 | ツール | 特徴 |
|------|--------|------|
| **自動テスト** | Testcontainers | テストごとに自動起動/停止 |
| **開発・手動テスト** | Docker Compose | 手動で起動/停止 |
| **マイグレーション確認** | Docker Compose | 永続化されたデータで確認 |

## ライセンス

MIT License

## 参考資料

- [ScalikeJDBC公式ドキュメント](http://scalikejdbc.org/)
- [Flyway公式サイト](https://flywaydb.org/)
- [Testcontainers公式サイト](https://www.testcontainers.org/)
- [Scala公式サイト](https://www.scala-lang.org/)
- [just コマンドランナー](https://github.com/casey/just)
