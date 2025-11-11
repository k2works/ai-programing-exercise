# 販売管理システム（F# 版）

テスト駆動開発（TDD）で育てる販売管理システムのデータベース設計プロジェクト（F# 版）

## 技術スタック

- **言語**: F# 9.0 (.NET 9.0)
- **ORM**: Dapper 2.1.35（マイクロ ORM）
- **マイグレーション**: FluentMigrator 6.2.0
- **データベース**: PostgreSQL 16 / MySQL 8.0
- **テスト**: xUnit 2.9.3 + FsUnit 6.0.1 + Testcontainers 4.1.0
- **コード品質**: FSharpLint
- **ビルド自動化**: Cake 5.1.0

## プロジェクト構造

```
db/fsharp/
├── SalesManagement.sln               # ソリューションファイル
├── build.cake                         # Cake ビルドスクリプト
├── fsharplint.json                    # FSharpLint 設定
├── docker-compose.yml                 # Docker Compose 設定
├── .env.example                       # 環境変数テンプレート
│
├── SalesManagement.Domain/            # ドメインモデル層
│   ├── SalesManagement.Domain.fsproj
│   └── Library.fs
│
├── SalesManagement.Infrastructure/    # データアクセス層
│   ├── SalesManagement.Infrastructure.fsproj
│   ├── appsettings.json              # 開発用設定
│   ├── Program.fs                     # マイグレーション実行
│   ├── MigrationRunner.fs             # マイグレーションロジック
│   └── Migrations/                    # マイグレーションファイル
│       └── Migration_20250106_001_InitialSetup.fs
│
├── SalesManagement.Tests/             # テスト層
│   ├── SalesManagement.Tests.fsproj
│   ├── appsettings.Test.json         # テスト用設定
│   ├── DatabaseTestBase.fs            # テスト基底クラス
│   ├── IntegrationTests/
│   │   └── DatabaseConnectionTests.fs
│   └── Tests.fs
│
└── docker/                            # Docker 設定
    ├── postgres/init/01-init.sql
    └── mysql/
        ├── init/01-init.sql
        └── conf.d/my.cnf
```

## セットアップ

### 前提条件

- .NET SDK 9.0 以上
- Docker & Docker Compose（推奨）
- Git

### 1. リポジトリのクローン

```bash
git clone <repository-url>
cd db/fsharp
```

### 2. 環境変数の設定

```bash
cp .env.example .env
```

`.env` ファイルを編集してデータベース設定を変更できます。

### 3. Docker コンテナの起動

```bash
# PostgreSQL を起動
docker-compose up -d postgres

# または MySQL を起動
docker-compose up -d mysql

# すべてのサービス（PostgreSQL + MySQL + Adminer）を起動
docker-compose up -d
```

### 4. パッケージの復元とビルド

```bash
dotnet restore
dotnet build
```

### 5. マイグレーションの実行

```bash
cd SalesManagement.Infrastructure
dotnet run
cd ..
```

## 開発

### ビルド

```bash
dotnet build
```

### テストの実行

```bash
# すべてのテスト
dotnet test

# カバレッジ付きテスト
dotnet test --collect:"XPlat Code Coverage"
```

### マイグレーションの実行

```bash
cd SalesManagement.Infrastructure
dotnet run
cd ..
```

### FSharpLint の実行

```bash
# グローバルインストール（初回のみ）
dotnet tool install -g dotnet-fsharplint

# Lint 実行
dotnet fsharplint lint SalesManagement.sln
```

## Cake タスク

Cake を使用して各種タスクを実行できます：

```bash
# デフォルト（ビルド + テスト）
dotnet cake

# 特定のタスク実行
dotnet cake --target=Clean           # クリーンアップ
dotnet cake --target=Build           # ビルド
dotnet cake --target=Test            # テスト実行
dotnet cake --target=Lint            # FSharpLint 実行
dotnet cake --target=Test-Coverage   # カバレッジ測定
dotnet cake --target=Coverage-Report # レポート生成
dotnet cake --target=CI              # CI 用完全チェック
dotnet cake --target=Migrate         # マイグレーション実行
dotnet cake --target=Docker-Up       # Docker 起動
dotnet cake --target=Docker-Down     # Docker 停止

# シェルラッパー（Unix/Linux/macOS）
./cake.sh Build
./cake.sh Test
```

## データベース管理

### Adminer（Web UI）

Docker Compose でサービスを起動すると、Adminer が利用可能になります：

- URL: http://localhost:8080

**PostgreSQL の場合:**
- システム: PostgreSQL
- サーバ: postgres
- ユーザ名: postgres
- パスワード: postgres
- データベース: sales_management_fsharp

**MySQL の場合:**
- システム: MySQL
- サーバ: mysql
- ユーザ名: user
- パスワード: password
- データベース: sales_management_fsharp

### Docker Compose コマンド

```bash
# コンテナの起動
docker-compose up -d

# コンテナの停止
docker-compose stop

# コンテナの停止と削除
docker-compose down

# ボリュームも削除
docker-compose down -v

# ログの確認
docker-compose logs -f postgres
docker-compose logs -f mysql

# コンテナに接続
docker-compose exec postgres psql -U postgres -d sales_management_fsharp
docker-compose exec mysql mysql -u user -p sales_management_fsharp
```

## テスト

このプロジェクトは Testcontainers を使用して、Docker コンテナ上でデータベーステストを実行します。

### テストの特徴

- **独立性**: 各テストクラスが独自のデータベースコンテナを使用
- **環境の一貫性**: 開発者のローカル環境、CI/CD 環境で同じデータベースバージョンを使用
- **クリーンな状態**: テスト実行後にコンテナを破棄し、次回は新しいコンテナで実行
- **並列実行**: 複数のテストが異なるコンテナで並列実行可能

### テストの実行

```bash
# すべてのテスト
dotnet test

# 詳細表示
dotnet test -v n

# カバレッジレポート生成
dotnet cake --target=Coverage-Report
# レポート: coverage/index.html
```

## アーキテクチャ

### レイヤー構成

```
┌─────────────────────────────────────┐
│       SalesManagement.Tests         │  テスト層
│  (xUnit + FsUnit + Testcontainers)  │
└─────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────┐
│   SalesManagement.Infrastructure    │  データアクセス層
│    (Dapper + FluentMigrator)        │
└─────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────┐
│      SalesManagement.Domain         │  ドメインモデル層
│         (F# Record Types)           │
└─────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────┐
│    PostgreSQL / MySQL Database      │  データベース
└─────────────────────────────────────┘
```

### F# の特徴を活かした設計

- **型システムの安全性**: ドメインモデルを型で表現し、コンパイル時にエラーを検出
- **不変性（Immutability）**: データの変更を明示的に扱い、予期しない副作用を防止
- **パイプライン処理**: データ変換を読みやすく表現
- **パターンマッチング**: 状態遷移やビジネスルールを網羅的に処理
- **簡潔な表現**: 定型コードが少なく、本質的なロジックに集中

## CI/CD

### GitHub Actions の例

```yaml
name: CI

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-dotnet@v3
        with:
          dotnet-version: '9.0.x'
      - name: Run CI checks
        run: dotnet cake --target=CI
```

## トラブルシューティング

### ビルドエラー

```bash
# クリーンビルド
dotnet clean
dotnet build
```

### テスト失敗

```bash
# Docker が起動しているか確認
docker ps

# Testcontainers のログを確認
dotnet test -v n
```

### マイグレーションエラー

```bash
# 接続文字列を確認
cat SalesManagement.Infrastructure/appsettings.json

# データベースが起動しているか確認
docker-compose ps
```

## 参考資料

- [F# 公式ドキュメント](https://learn.microsoft.com/ja-jp/dotnet/fsharp/)
- [Dapper](https://github.com/DapperLib/Dapper)
- [FluentMigrator](https://fluentmigrator.github.io/)
- [Testcontainers](https://dotnet.testcontainers.org/)
- [xUnit](https://xunit.net/)
- [FsUnit](https://fsprojects.github.io/FsUnit/)
- [Cake](https://cakebuild.net/)

## ライセンス

MIT License
