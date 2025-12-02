# Script Lab API クライアント

財務会計システム（Scala 実装）の REST API を Excel Script Lab から利用するための TypeScript クライアントです。

## 前提条件

- Microsoft Excel（デスクトップ版または Excel Online）
- Script Lab アドイン（Excel アドインストアからインストール）
- 財務会計システム API が起動していること

## API サーバーの起動

```bash
cd app/scala
docker compose up -d
sbt migrate
sbt seed
just api
```

API は `http://localhost:8080` で起動します。

Swagger UI: `http://localhost:8080/swagger-ui/`

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

### 1. 財務分析ツール (`financial-analysis.yaml`)

財務分析 API から D 社の財務指標データを取得し、Excel シートに出力します。

**機能**:
- 単年度分析: 指定した年度の財務分析データを取得
- 期間比較分析: 複数年度のデータを横並びで比較

**使用方法**:
1. YAML をインポート
2. 「決算期（年度）」に年度を入力（例: 2021）
3. 「分析実行」ボタンをクリック
4. シートに収益性・効率性・安全性指標が出力される

**出力される指標**:

| カテゴリ | 指標 |
|---------|------|
| 収益性 | 売上高総利益率、売上高営業利益率、売上高経常利益率、売上高販管費比率 |
| 効率性 | 総資本回転率、有形固定資産回転率 |
| 安全性 | 流動比率、負債比率、自己資本比率 |

### 2. 勘定科目管理ツール (`account-manager.yaml`)

勘定科目の CRUD 操作を Excel から実行します。

**機能**:
- 勘定科目一覧を取得
- 登録用シートを作成
- 勘定科目を一括登録
- 選択した勘定科目を削除

**使用方法**:

**一覧取得**:
1. 「勘定科目一覧を取得」ボタンをクリック
2. 「勘定科目一覧」シートに勘定科目の一覧が出力される

**一括登録**:
1. 「登録シート作成」ボタンをクリック
2. 「勘定科目登録」シートの A2 行以降に登録データを入力
3. 「一括登録」ボタンをクリック

**削除**:
1. 「勘定科目一覧」シートで削除したい行を選択
2. 「選択行を削除」ボタンをクリック

### 3. 仕訳入力ツール (`journal-entry.yaml`)

仕訳の一覧取得、入力、登録を Excel から実行します。

**機能**:
- 仕訳一覧を取得（期間指定）
- 仕訳入力シートを作成
- 仕訳を登録（貸借チェック付き）
- 勘定科目参照シートを表示

**使用方法**:

**仕訳一覧取得**:
1. 「開始日」「終了日」に期間を入力
2. 「仕訳一覧を取得」ボタンをクリック
3. 「仕訳一覧」シートに仕訳一覧が出力される

**仕訳入力**:
1. 「入力シート作成」ボタンをクリック
2. 「仕訳入力」シートが作成される
3. 仕訳ヘッダー（日付、摘要）を入力
4. 仕訳明細（勘定科目コード、金額）を入力
5. 貸借差額が 0 であることを確認
6. 「仕訳登録」ボタンをクリック

**勘定科目参照**:
1. 「勘定科目一覧を表示」ボタンをクリック
2. 「勘定科目参照」シートに勘定科目コードと名称が表示される

### 4. イベントソーシング版仕訳入力ツール (`journal-entry-es.yaml`)

イベントソーシング版の仕訳入力ツールです。

**API エンドポイント**: `/api/journal-entries`

**機能**:
- 仕訳の作成（イベントストアに保存）
- 仕訳の承認
- 仕訳の却下
- 仕訳の削除（論理削除）
- 仕訳の取得（イベント再生）
- イベント履歴の取得
- タイムトラベル（特定時点の状態取得）

## トラブルシューティング

### CORS エラーが発生する場合

Scala API サーバーが CORS を許可していることを確認してください。開発環境では Script Lab からのアクセスが許可されています。

### API に接続できない場合

1. API サーバーが起動しているか確認: `just api`
2. ファイアウォールの設定を確認
3. API の URL が正しいか確認（デフォルト: `http://localhost:8080`）

### データが見つからない場合

Seed データが投入されているか確認してください。

```bash
cd app/scala
just seed
```

## API エンドポイント

### 標準 API

| メソッド | パス | 説明 |
|---------|------|------|
| GET | `/api/v1/financial-analysis/{fiscalYear}` | 単年度の財務分析 |
| GET | `/api/v1/financial-analysis/compare?fromYear=X&toYear=Y` | 期間の財務分析比較 |
| GET | `/api/accounts` | 勘定科目一覧 |
| POST | `/api/accounts` | 勘定科目作成 |
| GET | `/api/accounts/{code}` | 勘定科目詳細 |
| PUT | `/api/accounts/{code}` | 勘定科目更新 |
| DELETE | `/api/accounts/{code}` | 勘定科目削除 |
| GET | `/api/journals?from=X&to=Y` | 仕訳一覧（期間指定） |
| POST | `/api/journals` | 仕訳登録 |
| GET | `/api/journals/{journalNo}` | 仕訳取得 |
| DELETE | `/api/journals/{journalNo}` | 仕訳削除 |

### イベントソーシング API

| メソッド | パス | 説明 |
|---------|------|------|
| POST | `/api/journal-entries` | 仕訳作成 |
| GET | `/api/journal-entries/{id}` | 仕訳取得（イベント再生） |
| POST | `/api/journal-entries/{id}/approve` | 仕訳承認 |
| POST | `/api/journal-entries/{id}/reject` | 仕訳却下 |
| DELETE | `/api/journal-entries/{id}` | 仕訳削除（論理削除） |
| GET | `/api/journal-entries/{id}/history` | イベント履歴取得 |
| GET | `/api/journal-entries/{id}/at?asOf=X` | タイムトラベル |

## ファイル構成

```
script-lab/
├── README.md                   # このファイル
├── financial-analysis.yaml     # 財務分析ツール
├── account-manager.yaml        # 勘定科目管理ツール
├── journal-entry.yaml          # 仕訳入力ツール（標準版）
└── journal-entry-es.yaml       # 仕訳入力ツール（イベントソーシング版）
```
