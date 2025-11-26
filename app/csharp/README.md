# AccountingSystem (C#)

財務会計システムの C# 実装です。TDD（テスト駆動開発）アプローチで開発しています。

## 技術スタック

- **.NET 9.0**
- **ASP.NET Core Web API** - REST API フレームワーク
- **Dapper** - 軽量 ORM
- **FluentMigrator** - データベースマイグレーション
- **PostgreSQL / MySQL** - データベース
- **Swagger/OpenAPI** - API ドキュメント
- **Testcontainers** - 統合テスト用コンテナ
- **xUnit** - テストフレームワーク
- **FluentAssertions** - アサーションライブラリ
- **Cake** - ビルド自動化

## プロジェクト構成

ヘキサゴナルアーキテクチャ（Ports and Adapters）に基づくマルチプロジェクト構成です。

```
AccountingSystem.sln
├── AccountingSystem.Domain/         # ドメイン層（依存なし）
│   ├── Entities/                    # エンティティ
│   ├── Events/                      # ドメインイベント
│   └── Models/                      # ドメインモデル
│       └── Financial/               # 財務モデル
├── AccountingSystem.Application/    # アプリケーション層（→ Domain）
│   ├── Exceptions/                  # カスタム例外
│   ├── Ports/                       # ポート
│   │   ├── In/                      # 入力ポート（サービスインターフェース）
│   │   └── Out/                     # 出力ポート（リポジトリインターフェース）
│   └── Services/                    # アプリケーションサービス
├── AccountingSystem.Infrastructure/ # インフラストラクチャ層（→ Domain, Application）
│   ├── EventHandlers/               # ドメインイベントハンドラー
│   ├── Migrations/                  # FluentMigrator マイグレーション
│   ├── Persistence/                 # 永続化
│   │   └── Repositories/            # リポジトリ実装
│   └── Web/                         # Web アダプター
│       ├── Controllers/             # API コントローラー
│       ├── Dtos/                    # データ転送オブジェクト
│       └── Middleware/              # ミドルウェア
├── AccountingSystem.Api/            # エントリポイント（→ 全プロジェクト）
│   └── Program.cs                   # アプリケーション起動
└── AccountingSystem.Tests/          # テスト
    ├── Application/Services/        # サービステスト
    ├── Domain/                      # ドメインテスト
    ├── Infrastructure/              # インフラテスト
    │   ├── EventHandlers/           # イベントハンドラーテスト
    │   └── Repositories/            # リポジトリテスト
    └── Integration/                 # API 統合テスト
```

## 前提条件

- .NET 9.0 SDK
- Docker（テスト実行時に必要）

## セットアップ

```bash
# パッケージ復元
dotnet restore

# ビルド
dotnet build

# テスト実行
dotnet test
```

## API サーバーの起動

```bash
cd AccountingSystem.Api

# 開発モードで起動
dotnet run

# Swagger UI: http://localhost:5057/swagger
```

## データベースマイグレーション

FluentMigrator を使用してスキーマを管理しています。

### CLI でマイグレーション実行

```bash
cd AccountingSystem.Api
dotnet run -- migrate
```

### マイグレーションファイル

| バージョン | 説明 |
|-----------|------|
| 20250121000 | 初期セットアップ |
| 20250121001 | 勘定科目マスタテーブル作成 |
| 20250121002 | 実務項目追加 |
| 20250121003 | 勘定科目制約追加 |
| 20250121004 | 勘定科目インデックス追加 |
| 20250121005 | 課税取引コード追加 |
| 20250121006 | 勘定科目構成マスタ作成 |
| 20250121007 | 税取引テーブル作成 |
| 20250121008 | 3層構造仕訳テーブル作成 |
| 20250121010 | 自動仕訳テーブル作成 |
| 20250121011 | 複式簿記チェック制約追加 |
| 20250121012 | 日次勘定残高テーブル作成 |
| 20250121013 | 月次勘定残高テーブル作成 |
| 20250121014 | 元帳ビュー作成 |
| 20250121015 | 監査ログテーブル作成 |
| 20250121016 | イベントストアテーブル作成 |
| 20250121017 | スナップショットテーブル作成 |
| 20250121018 | 仕訳 Read Model テーブル作成 |

## Cake ビルドスクリプト

```bash
# デフォルト（ビルド + テスト）
./cake.sh

# クリーンアップ
./cake.sh Clean

# テストのみ
./cake.sh Test

# カバレッジ付きテスト
./cake.sh Test-Coverage

# CI 用完全チェック
./cake.sh CI
```

## Docker 開発環境

```bash
# コンテナ起動
docker-compose up -d

# コンテナ停止
docker-compose down
```

| サービス | ポート | 用途 |
|---------|--------|------|
| PostgreSQL | 5432 | メインデータベース |
| MySQL | 3306 | 代替データベース |
| Adminer | 8080 | DB 管理 UI |

## API エンドポイント

### 勘定科目 API

| メソッド | エンドポイント | 説明 |
|---------|---------------|------|
| GET | /api/accounts | 勘定科目一覧取得 |
| GET | /api/accounts/{code} | 勘定科目取得 |
| POST | /api/accounts | 勘定科目作成 |
| PUT | /api/accounts/{code} | 勘定科目更新 |
| DELETE | /api/accounts/{code} | 勘定科目削除 |

### 仕訳 API

| メソッド | エンドポイント | 説明 |
|---------|---------------|------|
| GET | /api/journals/{slipNumber} | 仕訳取得 |
| POST | /api/journals | 仕訳作成 |
| POST | /api/journals/validate-balance | 貸借バランス検証 |

### 財務諸表 API

| メソッド | エンドポイント | 説明 |
|---------|---------------|------|
| GET | /api/financial-statements/balance-sheet | 貸借対照表取得 |

## テスト

テストは Testcontainers を使用して実際の PostgreSQL コンテナで実行します。

```bash
# 全テスト実行
dotnet test

# 特定のテストクラスのみ
dotnet test --filter "FullyQualifiedName~AccountTest"
```

### テストカテゴリ

| カテゴリ | 説明 |
|---------|------|
| Domain | ドメインモデル・イベントテスト |
| Infrastructure/Repositories | リポジトリ統合テスト |
| Infrastructure/EventHandlers | イベントハンドラーテスト |
| Application/Services | サービス層テスト |
| Integration | API 統合テスト |

## 開発手法

TDD（テスト駆動開発）サイクルに従って開発しています：

1. **Red** - 失敗するテストを書く
2. **Green** - テストを通す最小限のコードを書く
3. **Refactor** - コードを改善する

## ライセンス

このプロジェクトは学習目的で作成されています。
