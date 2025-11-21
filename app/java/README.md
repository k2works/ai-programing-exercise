# 財務会計システム - TDDで育てるデータベース設計（Java版）

TDD（テスト駆動開発）でデータベース設計を進める、実践的な財務会計システムの実装です。

## 概要

このプロジェクトは、以下の機能を提供する財務会計システムです：

- 勘定科目管理
- 仕訳入力
- 元帳管理
- 残高試算表
- 決算書作成

## 技術スタック

- **Java**: 21 LTS
- **フレームワーク**: Spring Boot 3.2.0
- **ビルドツール**: Gradle 8.11.1
- **データベース**: PostgreSQL 16 / MySQL 8.0
- **マイグレーション**: Flyway
- **ORM**: MyBatis 3.0.3
- **テスティング**: JUnit 5, Testcontainers
- **品質管理**: Checkstyle, PMD, SpotBugs, JaCoCo

## 前提条件

以下のツールがインストールされていることを確認してください：

- Java 21 以上
- Docker & Docker Compose
- Git

## セットアップ

### 1. リポジトリのクローン

```bash
git clone <repository-url>
cd app/java
```

### 2. 環境変数の設定

```bash
cp .env.example .env
```

必要に応じて `.env` ファイルを編集してください。

### 3. データベースの起動

```bash
# PostgreSQL を起動
docker-compose up -d postgres

# または MySQL を起動
docker-compose up -d mysql

# Adminer（データベース管理UI）も含めてすべて起動
docker-compose up -d
```

### 4. ビルドとテスト

```bash
# ビルド
./gradlew build

# テスト実行
./gradlew test

# テストカバレッジレポート生成
./gradlew jacocoTestReport
```

### 5. アプリケーションの起動

```bash
./gradlew bootRun
```

## 開発ワークフロー

### テスト駆動開発（TDD）

このプロジェクトはTDDサイクルに従って開発されています：

1. **Red（失敗）**: 失敗するテストを書く
2. **Green（成功）**: テストを通す最小限のコードを実装
3. **Refactor（改善）**: コードをリファクタリング

### データベースマイグレーション

Flywayを使用してデータベースのバージョン管理を行います。

```bash
# マイグレーション実行
./gradlew flywayMigrate

# マイグレーション情報確認
./gradlew flywayInfo

# マイグレーションのクリーンアップ（開発環境のみ）
./gradlew flywayClean
```

マイグレーションファイルは `app/src/main/resources/db/migration` に配置します。

### コード品質チェック

```bash
# Checkstyle 実行
./gradlew checkstyleMain checkstyleTest

# PMD 実行
./gradlew pmdMain pmdTest

# SpotBugs 実行
./gradlew spotbugsMain spotbugsTest

# すべての品質チェックを実行
./gradlew check
```

## データベース管理

### Adminer

ブラウザで http://localhost:8080 にアクセスすると、Adminer（データベース管理UI）が利用できます。

**PostgreSQL 接続情報:**
- システム: PostgreSQL
- サーバ: postgres
- ユーザ名: postgres
- パスワード: postgres
- データベース: accounting_system

**MySQL 接続情報:**
- システム: MySQL
- サーバ: mysql
- ユーザ名: user
- パスワード: password
- データベース: accounting_system

### CLI接続

```bash
# PostgreSQL に接続
docker-compose exec postgres psql -U postgres -d accounting_system

# MySQL に接続
docker-compose exec mysql mysql -u user -p accounting_system
```

## プロジェクト構造

```
app/java/
├── app/
│   ├── src/
│   │   ├── main/
│   │   │   ├── java/
│   │   │   │   └── com/example/accounting/
│   │   │   │       └── App.java
│   │   │   └── resources/
│   │   │       ├── application.properties
│   │   │       └── db/migration/
│   │   │           └── V1__create_accounts_table.sql
│   │   └── test/
│   │       ├── java/
│   │       │   └── com/example/accounting/
│   │       │       ├── TestDatabaseConfig.java
│   │       │       └── DatabaseConnectionTest.java
│   │       └── resources/
│   │           └── application-test.properties
│   └── build.gradle
├── config/
│   ├── checkstyle/
│   │   └── checkstyle.xml
│   └── pmd/
│       └── ruleset.xml
├── docker/
│   ├── postgres/
│   │   └── init/
│   │       └── 01-init.sql
│   └── mysql/
│       ├── init/
│       │   └── 01-init.sql
│       └── conf.d/
│           └── my.cnf
├── settings.gradle
├── docker-compose.yml
├── .env.example
└── README.md
```

## テスト

### 単体テスト

```bash
./gradlew test
```

### 統合テスト（Testcontainers）

Testcontainersを使用して、実際のPostgreSQLコンテナでテストを実行します：

```bash
./gradlew test --tests DatabaseConnectionTest
```

### テストカバレッジ

```bash
# カバレッジレポート生成
./gradlew jacocoTestReport

# レポート確認（HTMLファイルをブラウザで開く）
# app/build/reports/jacoco/test/html/index.html
```

## トラブルシューティング

### Docker コンテナが起動しない

```bash
# コンテナの状態確認
docker-compose ps

# ログ確認
docker-compose logs postgres
docker-compose logs mysql

# コンテナの再起動
docker-compose restart
```

### テストが失敗する

```bash
# ビルドキャッシュをクリア
./gradlew clean

# 依存関係を再取得
./gradlew build --refresh-dependencies
```

### Flyway マイグレーションエラー

```bash
# マイグレーション情報確認
./gradlew flywayInfo

# 開発環境でマイグレーションをクリーンアップ（注意：全データ削除）
./gradlew flywayClean
./gradlew flywayMigrate
```

## ライセンス

MIT License

## 参考資料

- [Spring Boot Documentation](https://spring.io/projects/spring-boot)
- [MyBatis Documentation](https://mybatis.org/mybatis-3/)
- [Flyway Documentation](https://flywaydb.org/documentation/)
- [Testcontainers Documentation](https://www.testcontainers.org/)
