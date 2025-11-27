# Script Lab API クライアント (Microservices)

マイクロサービス版会計システムの REST API を Excel Script Lab から利用するための TypeScript クライアントです。

## 前提条件

- Microsoft Excel（デスクトップ版または Excel Online）
- Script Lab アドイン（Excel アドインストアからインストール）
- マイクロサービス API が起動していること

## API サーバーの起動

### Docker Compose で全サービス起動（推奨）

```bash
cd app/csharp-ms
docker compose up -d
```

### 個別起動

```bash
cd app/csharp-ms

# インフラストラクチャ
docker compose up -d financial-db management-db rabbitmq

# 財務会計サービス
dotnet run --project FinancialAccounting/FinancialAccounting.Api

# API Gateway（別ターミナル）
dotnet run --project ApiGateway/ApiGateway
```

API Gateway は `http://localhost:8080` で起動します。

## Script Lab のセットアップ

### 1. Script Lab アドインのインストール

1. Excel を開く
2. `挿入` タブ → `アドインを取得` をクリック
3. 「Script Lab」を検索してインストール
4. `ホーム` タブに Script Lab のアイコンが追加される

### 2. YAML ファイルのインポート

1. Script Lab の `Code` ボタンをクリック
2. メニューから `Import` を選択
3. YAML ファイルの内容をコピーして貼り付け
4. `Import` をクリック

## 提供ツール

### 仕訳入力ツール (`journal-entry.yaml`)

仕訳の一覧取得、入力、登録を Excel から実行します。

**機能**:
- 仕訳一覧を取得（年度別）
- 仕訳入力シートを作成
- 仕訳を登録（貸借チェック付き）

**使用方法**:

**仕訳一覧取得**:
1. 「決算期（年度）」に年度を入力（例: 2024）
2. 「仕訳一覧を取得」ボタンをクリック
3. 「仕訳一覧_{年度}」シートに仕訳一覧が出力される

**仕訳入力**:
1. 「入力シート作成」ボタンをクリック
2. 「仕訳入力」シートが作成される
3. 仕訳ヘッダー（日付、決算期、摘要）を入力
4. 仕訳明細（勘定科目コード、金額）を入力
5. 貸借差額が 0 であることを確認
6. 「仕訳登録」ボタンをクリック

**出力されるシート**:

| シート名 | 説明 |
|---------|------|
| 仕訳一覧_{年度} | 指定年度の仕訳一覧 |
| 仕訳入力 | 仕訳入力用シート |

## トラブルシューティング

### CORS エラーが発生する場合

API サーバーが CORS を許可していることを確認してください。開発環境では `DevelopmentPolicy` が適用されます。

### API に接続できない場合

1. API サーバーが起動しているか確認
2. ファイアウォールの設定を確認
3. API Gateway の URL が正しいか確認（デフォルト: `http://localhost:8080`）

### データが見つからない場合

Seed データが投入されているか確認してください。サービス起動時に FluentMigrator が自動でマイグレーションを実行します。

## API エンドポイント

| メソッド | パス | 説明 |
|---------|------|------|
| GET | `/api/journals?fiscalYear=2024` | 仕訳一覧（年度別） |
| GET | `/api/journals/{id}` | 仕訳詳細 |
| POST | `/api/journals` | 仕訳登録 |
| GET | `/api/financial-analysis/{fiscalYear}` | 財務分析 |

## ファイル構成

```
script-lab/
├── README.md              # このファイル
└── journal-entry.yaml     # 仕訳入力ツール
```

## モノリス版との違い

| 項目 | モノリス版 (`app/csharp`) | マイクロサービス版 (`app/csharp-ms`) |
|------|--------------------------|-------------------------------------|
| API URL | `http://localhost:5000` | `http://localhost:8080` |
| 仕訳一覧 | `/api/v1/journals` | `/api/journals` |
| 仕訳登録 | 詳細なリクエスト形式 | シンプルなリクエスト形式 |
| 勘定科目 | `/api/v1/accounts` | （未実装） |
