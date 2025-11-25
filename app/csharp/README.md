# AccountingSystem (C#)

財務会計システムの C# 実装です。TDD（テスト駆動開発）アプローチで開発しています。

## 技術スタック

- **.NET 9.0**
- **Dapper** - 軽量 ORM
- **FluentMigrator** - データベースマイグレーション
- **PostgreSQL / MySQL** - データベース
- **Testcontainers** - 統合テスト用コンテナ
- **xUnit** - テストフレームワーク
- **FluentAssertions** - アサーションライブラリ
- **Cake** - ビルド自動化

## プロジェクト構成

```
AccountingSystem.sln
├── AccountingSystem.Domain/       # ドメインモデル
├── AccountingSystem.Infrastructure/ # データベース、マイグレーション
└── AccountingSystem.Tests/        # テスト
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

## データベースマイグレーション

FluentMigrator を使用してスキーマを管理しています。

### マイグレーションファイル

| バージョン | 説明 |
|-----------|------|
| 20250121000 | 初期セットアップ |
| 20250121001 | 勘定科目マスタテーブル作成 |
| 20250121002 | 実務項目追加 |

### 勘定科目マスタ（勘定科目マスタ）

| カラム | 型 | 説明 |
|--------|-----|------|
| 勘定科目ID | INT | 主キー（自動採番） |
| 勘定科目コード | VARCHAR(20) | 勘定科目コード（ユニーク） |
| 勘定科目名 | VARCHAR(100) | 勘定科目名 |
| 勘定科目種別 | ENUM | 資産/負債/純資産/収益/費用 |
| 残高 | DECIMAL(15,2) | 残高 |
| BSPL区分 | CHAR(1) | B:貸借対照表, P:損益計算書 |
| 取引要素区分 | CHAR(1) | 1:資産, 2:負債, 3:純資産, 4:収益, 5:費用 |
| 費用区分 | CHAR(1) | 費用の詳細分類 |
| 合計科目 | BOOLEAN | 集計科目フラグ |
| 表示順序 | INT | 財務諸表での表示順 |
| 集計対象 | BOOLEAN | 集計対象フラグ |
| 勘定科目カナ | VARCHAR(40) | 検索用カナ |
| 作成日時 | TIMESTAMP | 作成日時 |
| 更新日時 | TIMESTAMP | 更新日時 |

## テスト

テストは Testcontainers を使用して実際の PostgreSQL コンテナで実行します。

```bash
# 全テスト実行
dotnet test

# 特定のテストクラスのみ
dotnet test --filter "FullyQualifiedName~AccountTest"
```

### テストクラス

| クラス | テスト数 | 説明 |
|--------|---------|------|
| AccountTest | 6 | 勘定科目 CRUD テスト |
| AccountRefactoringTest | 3 | 実務項目テスト |
| AccountValidationTests | 2 | ドメインバリデーション |
| DatabaseConnectionTests | 2 | DB 接続テスト |

## 開発手法

TDD（テスト駆動開発）サイクルに従って開発しています：

1. **Red** - 失敗するテストを書く
2. **Green** - テストを通す最小限のコードを書く
3. **Refactor** - コードを改善する

## ライセンス

このプロジェクトは学習目的で作成されています。
