# Scala 財務会計システム - TDD でデータベース設計

Scala 3、ScalikeJDBC、Flyway を使用した財務会計システムのデータベース設計プロジェクトです。

## 前提条件

- **Scala 3.3.3 以上**
- **sbt 1.9.0 以上**
- **JDK 11 以上**
- **Docker Desktop 4.30.0**（推奨）または互換性のあるバージョン
- **Docker Compose**

## プロジェクト構成

```
app/scala/
├── build.sbt                           # sbt ビルド設定
├── project/
│   ├── build.properties               # sbt バージョン
│   └── plugins.sbt                    # sbt プラグイン
├── docker-compose.yml                  # Docker Compose 設定
├── docker/
│   └── postgres/
│       └── init/
│           └── 01_init.sql            # DB 初期化 SQL
├── src/
│   ├── main/
│   │   ├── resources/
│   │   │   ├── application.conf       # アプリケーション設定
│   │   │   └── db/migration/
│   │   │       └── V1__*.sql          # Flyway マイグレーション
│   │   └── scala/com/example/db/
│   │       ├── DatabaseConfig.scala   # DB 接続設定
│   │       └── FlywayMigration.scala  # マイグレーション管理
│   └── test/
│       ├── resources/
│       │   └── logback-test.xml       # テスト用ログ設定
│       └── scala/com/example/db/
│           ├── DatabaseConfigSpec.scala
│           ├── DatabaseSpec.scala      # テスト基底クラス
│           └── FlywayMigrationSpec.scala
└── .scalafmt.conf                      # コードフォーマット設定
```

## 開発環境のセットアップ

### Docker コンテナの起動

```bash
# コンテナをバックグラウンドで起動
docker compose up -d

# 起動確認
docker compose ps
```

### 依存関係のダウンロード

```bash
sbt update
```

### テストの実行

```bash
# Docker が起動していればテストは Testcontainers で自動実行
sbt test
```

### マイグレーションの実行

```bash
# マイグレーション実行
sbt migrate

# マイグレーション情報の確認
sbt migrateInfo

# マイグレーション検証
sbt migrateValidate
```

## 接続情報

### 開発用 PostgreSQL

- **Host**: localhost
- **Port**: 5432
- **Database**: accounting_system
- **User**: postgres
- **Password**: postgres

### pgAdmin（データベース管理 UI）

- **URL**: http://localhost:5050
- **Email**: admin@example.com
- **Password**: admin

pgAdmin でサーバーを追加する際:
- **Host**: `postgres`（Docker ネットワーク内）
- **Port**: 5432
- **Username**: postgres
- **Password**: postgres

## コマンド一覧

| コマンド | 説明 |
|----------|------|
| `sbt compile` | コンパイル |
| `sbt test` | テスト実行 |
| `sbt migrate` | マイグレーション実行 |
| `sbt migrateInfo` | マイグレーション情報表示 |
| `sbt scalafmt` | コードフォーマット |
| `sbt scalafmtCheck` | フォーマットチェック |
| `docker compose up -d` | コンテナ起動 |
| `docker compose down` | コンテナ停止 |
| `docker compose down -v` | コンテナ & データ削除 |

## 技術スタック

- **Scala 3.3.3**: プログラミング言語
- **ScalikeJDBC 4.2.1**: データベースアクセスライブラリ
- **Flyway 10.4.1**: マイグレーションツール
- **PostgreSQL 15**: データベース
- **Testcontainers**: Docker ベースのテスト環境
- **ScalaTest 3.2.18**: テストフレームワーク
- **Akka HTTP 10.5.3**: HTTP フレームワーク（将来使用）

## Docker Desktop のセットアップ

### 推奨バージョン

**Docker Desktop 4.30.0**（Docker Engine 26.1.1）を推奨します。

Docker Desktop 29.x 以降では、Testcontainers との互換性問題が発生するため、4.30.0 へのダウングレードを推奨します。

### Docker Desktop 4.30.0 のインストール

1. **現在の Docker Desktop をアンインストール**（既にインストール済みの場合）:
   ```powershell
   winget uninstall Docker.DockerDesktop
   ```

2. **Docker Desktop 4.30.0 をダウンロード**:
   ```
   https://desktop.docker.com/win/main/amd64/149282/Docker%20Desktop%20Installer.exe
   ```

3. **インストール**: ダウンロードした `Docker Desktop Installer.exe` を実行

4. **自動更新を無効化**:
   - Docker Desktop → Settings → Software updates
   - 「Automatically check for updates」のチェックを外す

5. **バージョン確認**:
   ```bash
   docker --version
   # Docker version 26.1.1 と表示されれば OK
   ```

### Testcontainers の設定（Windows）

ホームディレクトリに `.testcontainers.properties` を作成:

```properties
#Testcontainers configuration for Docker Desktop
docker.host=tcp://localhost:2375
ryuk.container.privileged=true
testcontainers.reuse.enable=true
```

Docker Desktop で TCP socket を有効化:
- Docker Desktop → Settings → General
- 「Expose daemon on tcp://localhost:2375 without TLS」を有効化

### トラブルシューティング

Docker 接続エラーが発生する場合:
- Docker Desktop が起動していることを確認
- `docker info` コマンドで接続を確認
- Docker Desktop を再起動
- Docker Desktop 29.x を使用している場合は 4.30.0 へダウングレード
