# 生産管理システム API リファレンス

## 概要

生産管理システムの REST API ドキュメントです。このシステムは、品目管理、BOM（部品構成表）、在庫管理、MRP（資材所要量計画）、発注、作業指示などの機能を提供します。

## API 仕様ドキュメント

### Swagger UI

APIサーバー起動中に、以下のURLでSwagger UIを利用できます:

```
http://localhost:3000/docs
```

### OpenAPI 仕様ファイル

OpenAPI 3.0 仕様は以下の形式で提供されています:

- **YAML形式**: [`api-docs/openapi.yaml`](./api-docs/openapi.yaml)
- **JSON形式**: [`api-docs/openapi.json`](./api-docs/openapi.json)

#### OpenAPI 仕様の更新方法

1. APIサーバーを起動:
   ```bash
   npm run dev:api
   ```

2. 別のターミナルで仕様をエクスポート:
   ```bash
   npm run api:export-spec
   ```

## HTTP Client を使用したAPIテスト

### JetBrains IDE (IntelliJ IDEA / WebStorm)

[`api-requests.http`](./api-requests.http) ファイルには、すべてのAPIエンドポイントのサンプルリクエストが含まれています。

#### 使用方法

1. IntelliJ IDEAまたはWebStormで `api-requests.http` を開く
2. APIサーバーを起動: `npm run dev:api`
3. リクエストの横にある ▶️ ボタンをクリックして実行

#### ファイル構成

- **ヘルスチェック**: サーバーの稼働状況確認
- **品目マスタ API**: 製品・部品マスタの CRUD 操作
- **在庫照会 API**: 在庫情報の照会・更新
- **BOM API**: 部品構成表の管理
- **MRP API**: 資材所要量計画の実行
- **発注 API**: 購買オーダの管理
- **作業指示 API**: 製造作業指示の管理

### VS Code REST Client

VS Code を使用している場合は、[REST Client](https://marketplace.visualstudio.com/items?itemName=humao.rest-client) 拡張機能をインストールすることで、同じ `.http` ファイルを使用できます。

## 主要なAPIエンドポイント

### 品目管理 (Item Management)

| メソッド | エンドポイント | 説明 |
|---------|--------------|------|
| GET | `/items` | 品目一覧取得 |
| GET | `/items?itemCode={code}` | 品目検索（品目コード） |
| GET | `/items?category={category}` | 品目検索（カテゴリ） |
| GET | `/items/{itemCode}` | 品目詳細取得 |
| POST | `/items` | 品目作成 |
| PUT | `/items/{itemCode}` | 品目更新 |
| DELETE | `/items/{itemCode}` | 品目削除 |

### 在庫照会 (Inventory Inquiry)

| メソッド | エンドポイント | 説明 |
|---------|--------------|------|
| GET | `/inventory` | 在庫一覧取得 |
| GET | `/inventory?itemCode={code}` | 在庫検索（品目コード） |
| GET | `/inventory?locationCode={code}` | 在庫検索（ロケーション） |
| GET | `/inventory/{itemCode}/{locationCode}` | 在庫詳細取得 |
| POST | `/inventory` | 在庫作成 |
| PATCH | `/inventory/{itemCode}/{locationCode}/adjust` | 在庫数量調整 |

### BOM（部品構成表）

| メソッド | エンドポイント | 説明 |
|---------|--------------|------|
| GET | `/bom` | BOM一覧取得 |
| GET | `/bom?parentItemCode={code}` | BOM検索（親品目） |
| GET | `/bom?childItemCode={code}` | BOM検索（子品目） |
| GET | `/bom/{parentItemCode}/{childItemCode}` | BOM詳細取得 |
| POST | `/bom` | BOM作成 |
| PUT | `/bom/{parentItemCode}/{childItemCode}` | BOM更新 |
| DELETE | `/bom/{parentItemCode}/{childItemCode}` | BOM削除 |

### MRP（資材所要量計画）

| メソッド | エンドポイント | 説明 |
|---------|--------------|------|
| POST | `/mrp/execute` | MRP実行 |

### 発注 (Purchase Orders)

| メソッド | エンドポイント | 説明 |
|---------|--------------|------|
| GET | `/purchase-orders` | 発注一覧取得 |
| GET | `/purchase-orders?orderNumber={number}` | 発注検索（発注番号） |
| GET | `/purchase-orders?itemCode={code}` | 発注検索（品目コード） |
| GET | `/purchase-orders?status={status}` | 発注検索（ステータス） |
| GET | `/purchase-orders/{orderNumber}` | 発注詳細取得 |
| POST | `/purchase-orders` | 発注作成 |
| PATCH | `/purchase-orders/{orderNumber}/status` | 発注ステータス更新 |

### 作業指示 (Work Orders)

| メソッド | エンドポイント | 説明 |
|---------|--------------|------|
| GET | `/work-orders` | 作業指示一覧取得 |
| GET | `/work-orders?workOrderNumber={number}` | 作業指示検索（作業指示番号） |
| GET | `/work-orders?itemCode={code}` | 作業指示検索（品目コード） |
| GET | `/work-orders?status={status}` | 作業指示検索（ステータス） |
| GET | `/work-orders/{workOrderNumber}` | 作業指示詳細取得 |
| POST | `/work-orders` | 作業指示作成 |
| PATCH | `/work-orders/{workOrderNumber}/status` | 作業指示ステータス更新 |
| PATCH | `/work-orders/{workOrderNumber}/report` | 作業実績報告 |

## 認証

現在のバージョンでは認証機能は実装されていません。将来的に追加される予定です。

## エラーハンドリング

APIは以下のHTTPステータスコードを返します:

| ステータスコード | 説明 |
|---------------|------|
| 200 | 成功 |
| 201 | 作成成功 |
| 400 | リクエストエラー |
| 404 | リソースが見つからない |
| 500 | サーバーエラー |

エラーレスポンスの形式:

```json
{
  "error": "Error Type",
  "message": "詳細なエラーメッセージ"
}
```

## 開発環境

### 前提条件

- Node.js 22.x
- PostgreSQL (Docker Composeで自動起動)

### セットアップ

1. 依存関係のインストール:
   ```bash
   npm install
   ```

2. Prisma クライアントの生成:
   ```bash
   npm run prisma:generate
   ```

3. データベースマイグレーション:
   ```bash
   npm run prisma:migrate
   ```

4. APIサーバーの起動:
   ```bash
   npm run dev:api
   ```

### テスト

```bash
# 全テスト実行
npm test

# ウォッチモード
npm run test:watch

# カバレッジレポート
npm run test:coverage
```

## ライセンス

Private

## サポート

問題が発生した場合は、GitHubのIssueで報告してください。
