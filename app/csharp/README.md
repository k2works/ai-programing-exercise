# Production Management System

生産管理システムの C# 実装です。Clean Architecture に基づいた設計で、REST API、gRPC API、WPF クライアントを提供します。

## アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│                      Presentation Layer                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │  REST API   │  │  gRPC API   │  │    WPF Client       │  │
│  │  (Port 5000)│  │  (Port 5001)│  │  (Desktop App)      │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                     Application Layer                        │
│              Use Cases / Application Services                │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                       Domain Layer                           │
│                 Entities / Value Objects                     │
└─────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────┐
│                   Infrastructure Layer                       │
│         Repositories / gRPC Services / REST Controllers      │
└─────────────────────────────────────────────────────────────┘
                              │
                        ┌───────────┐
                        │ PostgreSQL│
                        └───────────┘
```

## プロジェクト構成

```
app/csharp/
├── src/
│   ├── ProductionManagement.Domain/        # ドメイン層（エンティティ、値オブジェクト）
│   ├── ProductionManagement.Application/   # アプリケーション層（ユースケース）
│   ├── ProductionManagement.Infrastructure/# インフラ層（リポジトリ、gRPC サービス）
│   ├── ProductionManagement.Api/           # REST API（ポート 5000）
│   ├── ProductionManagement.Grpc.Api/      # gRPC API（ポート 5001）
│   ├── ProductionManagement.Grpc.Protos/   # Protocol Buffers 定義
│   ├── ProductionManagement.Web/           # Blazor Web アプリケーション
│   └── ProductionManagement.WPF/           # WPF デスクトップクライアント
├── tests/
│   ├── ProductionManagement.Tests/         # 単体テスト
│   └── ProductionManagement.IntegrationTests/ # 統合テスト
├── tools/
│   ├── ProductionManagement.Migrator/      # データベースマイグレーション
│   └── ProductionManagement.Seeder/        # シードデータ投入
├── docker/                                 # Docker 関連ファイル
├── docker-compose.yml                      # Docker Compose 設定
└── ProductionManagement.sln                # ソリューションファイル
```

## 技術スタック

| カテゴリ | 技術 |
|---------|------|
| フレームワーク | .NET 9.0 |
| Web API | ASP.NET Core Minimal API |
| gRPC | Grpc.AspNetCore |
| デスクトップ | WPF + Material Design |
| ORM | Dapper |
| データベース | PostgreSQL 16 |
| マイグレーション | FluentMigrator |
| コード分析 | StyleCop, SonarAnalyzer |
| テスト | xUnit, Moq |

## 前提条件

- [.NET 9.0 SDK](https://dotnet.microsoft.com/download/dotnet/9.0)
- [Docker Desktop](https://www.docker.com/products/docker-desktop/) （データベース用）
- [Visual Studio 2022](https://visualstudio.microsoft.com/) または [Rider](https://www.jetbrains.com/rider/)

## セットアップ

### 1. データベースの起動

```bash
cd app/csharp
docker-compose up -d
```

PostgreSQL が `localhost:5432` で起動します。

- ユーザー: `postgres`
- パスワード: `postgres`
- データベース: `production_management`

### 2. マイグレーションの実行

```bash
dotnet run --project tools/ProductionManagement.Migrator
```

### 3. シードデータの投入（オプション）

```bash
dotnet run --project tools/ProductionManagement.Seeder
```

## アプリケーションの実行

### REST API（ポート 5000）

```bash
dotnet run --project src/ProductionManagement.Api
```

Swagger UI: http://localhost:5000/swagger

### gRPC API（ポート 5001）

```bash
dotnet run --project src/ProductionManagement.Grpc.Api
```

### WPF クライアント

```bash
dotnet run --project src/ProductionManagement.WPF
```

### 複数プロジェクトの同時実行

```bash
# REST API と gRPC API を同時に起動
dotnet run --project src/ProductionManagement.Api &
dotnet run --project src/ProductionManagement.Grpc.Api &
```

## テストの実行

```bash
# 全テスト実行
dotnet test

# 単体テストのみ
dotnet test tests/ProductionManagement.Tests

# 統合テストのみ
dotnet test tests/ProductionManagement.IntegrationTests
```

## 主な機能

| 機能 | 説明 |
|------|------|
| 品目管理 | 製品、部品、原材料の登録・管理 |
| BOM 管理 | 部品表の構成管理、正展開・逆展開 |
| 在庫管理 | 在庫照会、入出庫処理 |
| 発注管理 | 発注書作成、発注状況管理 |
| 製造指示 | 作業指示の作成・進捗管理 |
| MRP | 所要量計算、調達計画立案 |
| 取引先管理 | 仕入先・得意先の登録・管理 |

## gRPC サービス

| サービス | 説明 |
|----------|------|
| ItemService | 品目の CRUD 操作 |
| BomService | BOM 展開（Server Streaming） |
| MrpService | MRP 計算実行 |
| PurchaseOrderService | 発注管理 |

## 開発ガイドライン

- コードスタイル: StyleCop + SonarAnalyzer によるコード品質チェック
- アーキテクチャ: Clean Architecture に従った依存関係
- テスト: TDD サイクルに基づく開発

## ライセンス

このプロジェクトは学習・演習目的で作成されています。
