# Script Lab for Financial Accounting Microservice

Excel Script Lab から F# マイクロサービス API を操作するためのツールセット。

## 概要

このディレクトリには以下のファイルが含まれています：

- `openapi.yaml` - API 仕様書（OpenAPI 3.0）
- `generate-openapi.ps1` - OpenAPI スキーマ生成スクリプト
- `journals-bulk-operations.yaml` - Script Lab 仕訳一括操作スニペット

## 前提条件

- .NET 9.0 SDK
- PowerShell 7.0 以上
- Excel (Script Lab アドイン)

## OpenAPI スキーマの生成

### 方法 1: PowerShell スクリプト（推奨）

```powershell
cd app/fsharp-ms/script-lab
./generate-openapi.ps1
```

このスクリプトは以下を自動的に行います：

1. プロジェクトをビルド
2. Swashbuckle CLI でスキーマを抽出
3. YamlDotNet で YAML に変換

### 方法 2: API 実行中に取得

```powershell
# API を起動
cd ../src/FinancialAccounting.Api
dotnet run

# 別ターミナルで Swagger JSON を取得
Invoke-RestMethod http://localhost:5115/swagger/v1/swagger.json | ConvertTo-Json -Depth 100 > openapi.json
```

## Script Lab の使い方

### セットアップ

1. Excel を開く
2. Script Lab アドインをインストール（Office アドインストアから）
3. Script Lab を開く → Import → YAML ファイルをインポート

### journals-bulk-operations.yaml

仕訳の一括取得・登録を行うスニペット。

**機能：**

- **仕訳取得**: 指定した会計年度の仕訳をシートに展開
- **勘定科目取得**: 登録済みの勘定科目一覧を取得
- **仕訳登録**: シート上のデータを仕訳として API に登録

**使い方：**

1. スニペットをインポート
2. API ベース URL を環境に合わせて変更（デフォルト: `http://localhost:5115`）
3. 会計年度を入力して「仕訳取得」をクリック

## API エンドポイント

### 仕訳 (Journals)

| メソッド | パス                          | 説明                 |
| -------- | ----------------------------- | -------------------- |
| GET      | /api/journals?fiscalYear={yr} | 会計年度で仕訳を取得 |
| POST     | /api/journals                 | 仕訳を作成           |
| GET      | /api/journals/{id}            | ID で仕訳を取得      |

### 勘定科目 (Accounts)

| メソッド | パス                        | 説明                     |
| -------- | --------------------------- | ------------------------ |
| GET      | /api/accounts               | 全勘定科目を取得         |
| POST     | /api/accounts               | 勘定科目を作成           |
| GET      | /api/accounts/{id}          | ID で勘定科目を取得      |
| GET      | /api/accounts/code/{code}   | コードで勘定科目を取得   |
| GET      | /api/accounts/type/{type}   | 種別で勘定科目を取得     |
| PUT      | /api/accounts/{id}          | 勘定科目を更新           |
| DELETE   | /api/accounts/{id}          | 勘定科目を削除           |

## 開発環境での API 起動

```powershell
# PostgreSQL を起動（Docker）
cd app/fsharp-ms
docker-compose up -d postgres

# FinancialAccounting.Api を起動
cd src/FinancialAccounting.Api
dotnet run

# Swagger UI を開く
# http://localhost:5115/swagger
```

## トラブルシューティング

### CORS エラー

Script Lab から API にアクセスする際に CORS エラーが発生する場合は、API に CORS 設定を追加してください。

```fsharp
// Program.fs に追加
builder.Services.AddCors(fun options ->
    options.AddDefaultPolicy(fun builder ->
        builder.AllowAnyOrigin()
               .AllowAnyMethod()
               .AllowAnyHeader() |> ignore
    )
)

// app.UseAuthorization() の前に追加
app.UseCors() |> ignore
```

### 接続エラー

- API が起動しているか確認
- ポート番号が正しいか確認（デフォルト: 5115）
- ファイアウォール設定を確認
