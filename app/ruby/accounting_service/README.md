# Ruby Accounting Service

第9章「境界付けられたコンテキスト（Bounded Context）」の実装サンプルです。

## アーキテクチャ概要

マイクロサービスアーキテクチャに基づいた会計システムです。

```
├── financial_accounting/     # 財務会計サービス (port: 3000)
├── management_accounting/    # 管理会計サービス (port: 3001)
└── docker-compose.yml        # Docker Compose 設定
```

## サービス構成

### 1. 財務会計サービス (Financial Accounting)
- **ポート**: 3000
- **データベース**: PostgreSQL (port: 5432)
- **機能**:
  - 仕訳管理（作成、取得、一覧表示）
  - REST API エンドポイント
  - ヘキサゴナルアーキテクチャ実装

### 2. 管理会計サービス (Management Accounting)
- **ポート**: 3001
- **データベース**: PostgreSQL (port: 5433)
- **機能**:
  - 財務分析（財務比率の計算）
  - 腐敗防止層（Anti-Corruption Layer）
  - 財務会計サービスとのHTTP通信

## Docker Compose による起動

### 前提条件
- Docker
- Docker Compose

### 起動方法

```bash
# サービスをビルドして起動
docker-compose up --build

# バックグラウンドで起動
docker-compose up -d

# ログを確認
docker-compose logs -f

# 特定のサービスのログを確認
docker-compose logs -f financial-accounting
docker-compose logs -f management-accounting
```

### サービスの停止

```bash
# すべてのサービスを停止
docker-compose down

# ボリュームも削除（データベースのデータも削除）
docker-compose down -v
```

### データベースのリセット

```bash
# 財務会計サービスのデータベースをリセット
docker-compose exec financial-accounting bin/rails db:reset

# 管理会計サービスのデータベースをリセット
docker-compose exec management-accounting bin/rails db:reset
```

## API エンドポイント

### 財務会計サービス (http://localhost:3000)

#### 仕訳一覧取得
```bash
curl http://localhost:3000/api/journals
```

#### 会計年度で絞り込み
```bash
curl http://localhost:3000/api/journals?fiscal_year=2023
```

#### 仕訳詳細取得
```bash
curl http://localhost:3000/api/journals/{journal_id}
```

#### 仕訳作成
```bash
curl -X POST http://localhost:3000/api/journals \
  -H "Content-Type: application/json" \
  -d '{
    "journal_date": "2023-04-15",
    "description": "売上計上",
    "fiscal_year": 2023,
    "entries": [
      {
        "account_code": "1120",
        "debit_amount": 100000,
        "credit_amount": 0,
        "description": "売掛金"
      },
      {
        "account_code": "4110",
        "debit_amount": 0,
        "credit_amount": 100000,
        "description": "売上高"
      }
    ]
  }'
```

### 管理会計サービス (http://localhost:3001)

（今後実装予定）

## 開発環境

### ローカルでの実行（Docker なし）

#### 財務会計サービス

```bash
cd financial_accounting

# データベースのセットアップ
bin/rails db:create db:migrate db:seed

# サーバー起動
bin/rails server -p 3000
```

#### 管理会計サービス

```bash
cd management_accounting

# データベースのセットアップ
bin/rails db:create db:migrate

# サーバー起動
bin/rails server -p 3001
```

### テストの実行

```bash
# 財務会計サービス
cd financial_accounting
bundle exec rspec

# 管理会計サービス
cd management_accounting
bundle exec rspec
```

## トラブルシューティング

### ポートが既に使用されている場合

```bash
# 使用中のポートを確認
lsof -i :3000
lsof -i :3001
lsof -i :5432
lsof -i :5433

# プロセスを停止
kill -9 <PID>
```

### データベース接続エラー

```bash
# PostgreSQL コンテナが起動しているか確認
docker-compose ps

# PostgreSQL コンテナのログを確認
docker-compose logs postgres-financial
docker-compose logs postgres-management

# データベースを再作成
docker-compose down -v
docker-compose up -d
```

### Bundler エラー

```bash
# コンテナを再ビルド
docker-compose build --no-cache

# bundle install を再実行
docker-compose exec financial-accounting bundle install
docker-compose exec management-accounting bundle install
```

## 参考資料

- 記事: `docs/wiki/記事/データベース/実践データベース設計/財務会計/Ruby.md` 第9章
- ヘキサゴナルアーキテクチャ: Ports and Adapters Pattern
- ドメイン駆動設計（DDD）: Bounded Context, Aggregate Root
