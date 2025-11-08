# 販売管理システム（Java版）

TDDでデータベース設計を行う販売管理システムの実装プロジェクトです。

## 第0章：環境構築

このプロジェクトは「実践データベース設計：TDDで育てる販売管理システム（Java版）」の第0章「環境構築」を実装したものです。

### 技術スタック

- **Java**: 21 (LTS)
- **フレームワーク**: Spring Boot 3.2.0
- **ビルドツール**: Gradle 8.x
- **ORM/SQL**: MyBatis 3.0.3
- **データベース**: PostgreSQL 16 / MySQL 8.0
- **マイグレーション**: Flyway
- **テスト**: JUnit 5, Testcontainers
- **静的解析**: Checkstyle, PMD, SpotBugs
- **コードカバレッジ**: JaCoCo

### プロジェクト構造

```
db/java/
├── app/
│   ├── src/
│   │   ├── main/
│   │   │   ├── java/com/example/sales/
│   │   │   │   └── App.java
│   │   │   └── resources/
│   │   │       ├── application.properties
│   │   │       └── db/migration/
│   │   │           └── V1__Initial_setup.sql
│   │   └── test/
│   │       ├── java/com/example/sales/
│   │       │   ├── AbstractDatabaseTest.java
│   │       │   └── DatabaseConnectionTest.java
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
│   ├── mysql/
│   │   ├── init/
│   │   └── conf.d/
│   └── schemaspy/
│       └── Dockerfile
├── docker-compose.yml
├── .env.example
└── .gitignore
```

## セットアップ

### 前提条件

以下のツールがインストールされていることを確認してください：

- Java 17以上（推奨：Java 21 LTS）
- Gradle 8.x以上（Gradle Wrapper使用）
- Docker & Docker Compose（推奨）
- Git

### 環境構築手順

#### 1. 環境変数の設定

```bash
cp .env.example .env
```

`.env` ファイルを編集して、必要に応じてデータベース設定を変更してください。

#### 2. データベースコンテナの起動

```bash
# PostgreSQL を起動
docker-compose up -d postgres

# または MySQL を起動
docker-compose up -d mysql

# すべてのサービスを起動（PostgreSQL + MySQL + Adminer）
docker-compose up -d
```

#### 3. 依存関係のインストールとビルド

```bash
./gradlew build
```

#### 4. テスト実行

```bash
./gradlew test
```

## 開発

### よく使うコマンド

**重要**: すべてのGradleコマンドは、プロジェクトのルートディレクトリ (`db/java`) から実行してください。

#### ビルドとテスト

```bash
# ビルド
./gradlew build

# クリーンビルド
./gradlew clean build

# テスト実行
./gradlew test

# Spring Bootアプリケーション起動
./gradlew bootRun
```

#### 品質チェック

```bash
# 全体チェック（静的解析 + テスト）
./gradlew checkAll

# 静的コード解析
./gradlew checkstyleMain pmdMain spotbugsMain

# カバレッジレポート表示
./gradlew showCoverage
```

#### データベース

**重要**: Windows/WSL2 環境では `./gradlew flywayMigrate` が localhost 接続でタイムアウトします。以下の方法を使用してください。

```bash
# Flywayマイグレーション実行（推奨）
./migrate.sh

# または、個別にSQLファイルを実行
for file in app/src/main/resources/db/migration/V*.sql; do
  docker exec -i sales-management-postgres psql -U postgres -d sales_management < "$file"
done
```

以下の Gradle タスクは Linux/Mac 環境では動作する可能性があります：

```bash
# Flywayマイグレーション実行（Gradleタスク）
./gradlew flywayMigrate --no-configuration-cache

# Flyway情報表示
./gradlew flywayInfo --no-configuration-cache
```

#### その他

```bash
# セットアップガイド表示
./gradlew setup

# 利用可能なタスク一覧
./gradlew tasks

# 依存関係表示
./gradlew dependencies
```

### データベース管理

#### Adminer（Webベースのデータベース管理ツール）

- URL: http://localhost:8080
- **PostgreSQL の場合:**
  - システム: PostgreSQL
  - サーバ: postgres
  - ユーザ名: postgres
  - パスワード: postgres
  - データベース: sales_management
- **MySQL の場合:**
  - システム: MySQL
  - サーバ: mysql
  - ユーザ名: user
  - パスワード: password
  - データベース: sales_management

#### SchemaSpy（スキーマ可視化ツール）

```bash
# スキーマドキュメントを生成
docker-compose run --rm schemaspy

# 生成されたドキュメントを表示
docker-compose up -d schemaspy-viewer
```

- URL: http://localhost:8081

### Docker Compose の便利なコマンド

```bash
# コンテナの停止
docker-compose stop

# コンテナの停止と削除
docker-compose down

# コンテナの停止、削除、ボリュームも削除
docker-compose down -v

# ログの確認
docker-compose logs -f postgres
docker-compose logs -f mysql

# コンテナの状態確認
docker-compose ps

# コンテナに接続
docker-compose exec postgres psql -U postgres -d sales_management
docker-compose exec mysql mysql -u user -p sales_management
```

## テスト

### テスト環境

- **Testcontainers** を使用して、テスト実行時に自動的にDockerコンテナ上のデータベースを起動します
- テストごとにクリーンな状態のデータベースが利用できます
- 開発環境のデータベースと完全に分離されています

### テストの書き方

テストクラスは `AbstractDatabaseTest` を継承することで、自動的にTestcontainersの機能が利用できます。

```java
class MyTest extends AbstractDatabaseTest {
    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Test
    void testDatabase() {
        // テストコード
    }
}
```

## コード品質

### 静的コード解析

このプロジェクトでは、以下の静的コード解析ツールを使用しています：

- **Checkstyle**: コーディング規約のチェック
- **PMD**: コード品質と潜在的な問題の検出
- **SpotBugs**: バグパターンの検出
- **JaCoCo**: コードカバレッジの測定

### 品質基準

- **循環的複雑度**: メソッドレベル 7以下
- **認知的複雑度**: 4以下
- **メソッド長**: 50行以下
- **ファイル長**: 500行以下

## トラブルシューティング

### Gradleタスクが見つからない

**エラー**: `Task 'wrapper' not found in project ':app'`

**原因**: タスクがルートプロジェクトレベルでのみ利用可能なため。

**解決策**:
```bash
# プロジェクトのルートディレクトリに移動
cd db/java

# コマンドを実行
./gradlew tasks
```

### IDEからの実行

IntelliJ IDEA や VSCode などのIDEから Gradle タスクを実行する場合：

1. **Gradle Tool Window** を開く
2. **ルートプロジェクト** (`sales-management-db`) を選択
3. 実行したいタスクを選択

または、IDEのターミナルから：
```bash
# プロジェクトルートに移動
cd db/java

# タスクを実行
./gradlew build
```

### Testcontainersの問題

**エラー**: Docker関連のエラー

**解決策**:
1. Dockerが起動していることを確認
2. Dockerに十分なメモリが割り当てられているか確認（推奨: 4GB以上）
3. ネットワーク接続を確認（初回実行時はイメージのダウンロードが必要）

### 依存関係の問題

**エラー**: 依存関係の解決に失敗

**解決策**:
```bash
# Gradleキャッシュをクリア
./gradlew clean --refresh-dependencies

# 再ビルド
./gradlew build
```

### Flyway マイグレーションの問題

**エラー**: `Unable to obtain connection from database` または接続タイムアウト

**原因**: Windows/WSL2 環境では Gradle から localhost:5432 への接続がタイムアウトする問題があります。

**解決策**:
```bash
# migrate.sh スクリプトを使用（推奨）
./migrate.sh

# または、手動で SQL ファイルを実行
for file in app/src/main/resources/db/migration/V*.sql; do
  docker exec -i sales-management-postgres psql -U postgres -d sales_management < "$file"
done
```

**注意**:
- この問題は Windows/WSL2 環境特有です
- Linux/Mac 環境では `./gradlew flywayMigrate` が動作する可能性があります
- テスト実行時は Testcontainers を使用するため、この問題は発生しません

## 次のステップ

第0章の環境構築が完了しました。次は第1章「最初の要求『部門と従業員』」で、実際のデータベース設計を開始します。

## ライセンス

このプロジェクトは学習目的で作成されています。
