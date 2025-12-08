# Script Lab API クライアント

財務会計システム (F# 版) の REST API を Excel Script Lab から利用するための TypeScript クライアントです。

## 前提条件

- Microsoft Excel（デスクトップ版または Excel Online）
- Script Lab アドイン（Excel アドインストアからインストール）
- 財務会計システム API が起動していること

## API サーバーの起動

```bash
cd app/fsharp
docker-compose up -d
dotnet run --project AccountingSystem.Api
```

API は `http://localhost:5212` で起動します。

Swagger UI: `http://localhost:5212/swagger`

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

すべてのツールは Web UI 上のログエリアに実行状況を出力します。コンソールを開く必要はありません。

### 財務分析ツール (`financial-analysis.yaml`)

財務分析 API から D 社の財務データ・指標を取得し、Excel シートに出力します。

**機能**:
- 財務データ取得: 指定した年度の財務データ（B/S・P/L）を取得
- 財務指標取得: 指定した年度の財務指標を取得
- 収益性分析: 収益性指標と分析コメントを取得
- 効率性分析: 効率性指標と分析コメントを取得
- 安全性分析: 安全性指標と分析コメントを取得
- 期間比較分析: 複数年度のデータを横並びで比較

**使用方法**:
1. YAML をインポート
2. 「決算期（年度）」に年度を入力（例: 2021）
3. 各ボタンをクリックして分析を実行
4. シートに財務データ・指標が出力される
5. 実行ログエリアで処理状況を確認

**出力される指標**:

| カテゴリ | 指標 |
|---------|------|
| 収益性 | 売上高総利益率、売上高営業利益率、売上高経常利益率、売上高販管費比率、ROA、ROE |
| 効率性 | 総資本回転率、売上債権回転率、棚卸資産回転率、有形固定資産回転率 |
| 安全性 | 流動比率、当座比率、固定比率、固定長期適合率、負債比率、自己資本比率 |

### 勘定科目一括操作ツール (`accounts-bulk-operations.yaml`)

勘定科目の一覧取得・一括登録・一括更新・一括削除を行います。

**機能**:
- 勘定科目一覧取得: 全勘定科目を Excel シートに出力
- 一括登録: Excel シートから勘定科目を一括登録
- 一括更新: Excel シートから勘定科目を一括更新
- 一括削除: Excel シートから勘定科目を一括削除

**使用方法**:
1. YAML をインポート
2. 「勘定科目一覧取得」でデータを取得
3. シート上でデータを編集
4. 「一括登録」「一括更新」「一括削除」で API に反映
5. 実行ログエリアで処理状況を確認

**Excel シートのフォーマット**:

| 列 | 項目 |
|----|------|
| A | 勘定科目コード |
| B | 勘定科目名 |
| C | 勘定科目区分 (Assets/Liabilities/Equity/Revenue/Expense) |
| D | 有効フラグ (TRUE/FALSE) |

### 仕訳一括操作ツール (`journals-bulk-operations.yaml`)

仕訳の一覧取得・登録を行います。

**機能**:
- 仕訳一覧取得: 指定期間の仕訳を Excel シートに出力（明細含む）
- 仕訳登録: Excel シートから仕訳を登録

**使用方法**:
1. YAML をインポート
2. 期間を指定して「仕訳一覧取得」
3. シートに仕訳ヘッダーと明細が出力される
4. 新規仕訳はシートで作成後「仕訳登録」で API に登録
5. 実行ログエリアで処理状況を確認

**Excel シートのフォーマット（ヘッダー）**:

| 列 | 項目 |
|----|------|
| A | 伝票番号 |
| B | 起票日 |
| C | 入力日 |
| D | 決算仕訳フラグ |
| E | 単振フラグ |
| F | 伝票区分 |
| G | 定期計上フラグ |
| H | 社員コード |
| I | 部門コード |
| J | 赤伝フラグ |
| K | 赤黒伝票番号 |

### 仕訳イベントソーシング API ツール

イベントソーシングパターンに基づく仕訳の CRUD 操作、承認、タイムトラベル、スナップショット管理を行います。

#### 基本版 (`journal-entry-event-sourcing.yaml`)

基本的なイベントソーシング API クライアントです。

**機能**:
- 全仕訳取得: すべての仕訳を Excel シートに出力
- 単一仕訳取得: 仕訳 ID を指定して詳細を取得
- タイムトラベル: 特定時点の仕訳状態を取得
- 仕訳作成: Excel シートから仕訳を登録
- 仕訳承認: 仕訳を承認済み状態に更新
- 仕訳削除: 論理削除を実行

**出力シート**: 仕訳一覧(ES)、仕訳詳細(ES)、タイムトラベル(ES)、仕訳入力(ES)

#### イベント発行版 (`journal-entry-event-sourcing-with-events.yaml`)

イベント保存後に Read Model 更新と監査ログ記録を自動実行するバージョンです。

**機能**:
- 基本版のすべての機能
- スナップショット作成: 集約の現在状態を保存
- スナップショット削除: 保存されたスナップショットを削除

**出力シート**: 仕訳一覧(イベント版)、仕訳詳細(イベント版)、タイムトラベル(イベント版)、仕訳入力(イベント版)

#### スナップショット最適化版 (`journal-entry-event-sourcing-with-snapshot.yaml`)

スナップショットを活用した高速な集約復元を実現するバージョンです。

**機能**:
- 基本版のすべての機能
- スナップショット作成: 集約の現在状態を保存
- スナップショット削除: 保存されたスナップショットを削除

**出力シート**: 仕訳一覧(SS版)、仕訳詳細(SS版)、タイムトラベル(SS版)、仕訳入力(SS版)

**使用方法（共通）**:
1. YAML をインポート
2. API ベース URL を設定（デフォルト: `http://localhost:5000`）
3. 「全仕訳取得」で仕訳一覧を取得
4. 「入力シート準備」で新規仕訳用シートを作成
5. シートにデータを入力後「仕訳登録」で API に登録
6. 実行ログエリアで処理状況を確認

**Excel シートのフォーマット（仕訳入力）**:

| 列 | 項目 |
|----|------|
| A | 仕訳ID (UUID) |
| B | 仕訳日 |
| C | 摘要 |
| D | 勘定科目コード |
| E | 借方/貸方 (D/C) |
| F | 金額 |

## トラブルシューティング

### CORS エラーが発生する場合

API サーバーが CORS を許可していることを確認してください。開発環境では `DevelopmentPolicy` が適用され、すべてのオリジンからのアクセスが許可されます。

### API に接続できない場合

1. API サーバーが起動しているか確認
2. ファイアウォールの設定を確認
3. API の URL が正しいか確認（デフォルト: `http://localhost:5212`）

### `confirm()` や `alert()` が動作しない場合

Script Lab 環境では `confirm()` や `alert()` がブロックされる場合があります。
すべてのツールは Web UI 上のログエリアに実行状況を出力するため、コンソールを開く必要はありません。

### データが見つからない場合

Seed データが投入されているか確認してください。API 起動時に DatabaseSeeder が自動実行され、D 社のシードデータが投入されます。

```bash
# API の Swagger UI でデータを確認
open http://localhost:5212/swagger
```

## API エンドポイント

### 財務分析 API

| メソッド | パス | 説明 |
|---------|------|------|
| GET | `/api/v1/financial-analysis/data/{fiscalYear}` | 財務データ取得 |
| GET | `/api/v1/financial-analysis/ratios/{fiscalYear}` | 財務指標取得 |
| POST | `/api/v1/financial-analysis/compare` | 複数年度比較 |
| GET | `/api/v1/financial-analysis/profitability/{fiscalYear}` | 収益性分析 |
| GET | `/api/v1/financial-analysis/efficiency/{fiscalYear}` | 効率性分析 |
| GET | `/api/v1/financial-analysis/safety/{fiscalYear}` | 安全性分析 |

### 勘定科目 API

| メソッド | パス | 説明 |
|---------|------|------|
| GET | `/api/v1/accounts` | 勘定科目一覧 |
| GET | `/api/v1/accounts/{accountCode}` | 勘定科目詳細 |
| GET | `/api/v1/accounts/type/{accountType}` | 種別別勘定科目 |
| POST | `/api/v1/accounts` | 勘定科目作成 |

### 仕訳 API

| メソッド | パス | 説明 |
|---------|------|------|
| GET | `/api/v1/journals` | 仕訳一覧 |
| GET | `/api/v1/journals/{voucherNumber}` | 仕訳詳細 |
| POST | `/api/v1/journals` | 仕訳登録 |

### 財務諸表 API

| メソッド | パス | 説明 |
|---------|------|------|
| GET | `/api/v1/financial-statements/balance-sheet` | 貸借対照表 |
| GET | `/api/v1/financial-statements/income-statement` | 損益計算書 |
| GET | `/api/v1/financial-statements/ratios` | 財務比率 |

### イベントソーシング API（基本版）

| メソッド | パス | 説明 |
|---------|------|------|
| GET | `/api/v1/journal-entries` | 仕訳一覧 |
| GET | `/api/v1/journal-entries/{id}` | 仕訳取得 |
| POST | `/api/v1/journal-entries` | 仕訳作成 |
| POST | `/api/v1/journal-entries/{id}/approve` | 仕訳承認 |
| DELETE | `/api/v1/journal-entries/{id}` | 仕訳削除 |
| GET | `/api/v1/journal-entries/{id}/at` | タイムトラベル |

### イベントソーシング API（イベント発行版）

| メソッド | パス | 説明 |
|---------|------|------|
| GET | `/api/v1/journal-entries-with-events` | 仕訳一覧 |
| GET | `/api/v1/journal-entries-with-events/{id}` | 仕訳取得 |
| POST | `/api/v1/journal-entries-with-events` | 仕訳作成（Read Model 更新 + 監査ログ記録） |
| POST | `/api/v1/journal-entries-with-events/{id}/approve` | 仕訳承認（Read Model 更新 + 監査ログ記録） |
| DELETE | `/api/v1/journal-entries-with-events/{id}` | 仕訳削除（Read Model 更新 + 監査ログ記録） |
| GET | `/api/v1/journal-entries-with-events/{id}/at` | タイムトラベル |
| POST | `/api/v1/journal-entries-with-events/{id}/snapshot` | スナップショット作成 |
| DELETE | `/api/v1/journal-entries-with-events/{id}/snapshot` | スナップショット削除 |

### イベントソーシング API（スナップショット最適化版）

| メソッド | パス | 説明 |
|---------|------|------|
| GET | `/api/v1/journal-entries-with-snapshot` | 仕訳一覧 |
| GET | `/api/v1/journal-entries-with-snapshot/{id}` | 仕訳取得（スナップショット活用） |
| POST | `/api/v1/journal-entries-with-snapshot` | 仕訳作成 |
| POST | `/api/v1/journal-entries-with-snapshot/{id}/approve` | 仕訳承認 |
| DELETE | `/api/v1/journal-entries-with-snapshot/{id}` | 仕訳削除 |
| GET | `/api/v1/journal-entries-with-snapshot/{id}/at` | タイムトラベル |
| POST | `/api/v1/journal-entries-with-snapshot/{id}/snapshot` | スナップショット作成 |
| DELETE | `/api/v1/journal-entries-with-snapshot/{id}/snapshot` | スナップショット削除 |

## ファイル構成

```
script-lab/
├── README.md                                        # このファイル
├── financial-analysis.yaml                          # 財務分析ツール
├── accounts-bulk-operations.yaml                    # 勘定科目一括操作ツール
├── journals-bulk-operations.yaml                    # 仕訳一括操作ツール
├── journal-entry-event-sourcing.yaml                # イベントソーシング API（基本版）
├── journal-entry-event-sourcing-with-events.yaml    # イベントソーシング API（イベント発行版）
└── journal-entry-event-sourcing-with-snapshot.yaml  # イベントソーシング API（スナップショット最適化版）
```
