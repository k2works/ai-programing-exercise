# 財務会計システム - マイクロサービスアーキテクチャ

第8章：境界付けられたコンテキストに基づいたマイクロサービス実装

## クイックリファレンス

### 接続情報

| サービス | ポート | URL / 接続文字列 | 認証情報 |
|---------|--------|-----------------|---------|
| **API Gateway** | 8080 | http://localhost:8080 | - |
| **財務会計 API** | 3001 | http://localhost:8080/api/fa/ | - |
| **管理会計 API** | 3002 | http://localhost:8080/api/ma/ | - |
| **財務会計 DB** | 5432 | postgresql://localhost:5432/financial_accounting | fa_user / fa_password |
| **管理会計 DB** | 5433 | postgresql://localhost:5433/management_accounting | ma_user / ma_password |
| **RabbitMQ AMQP** | 5672 | amqp://localhost:5672 | admin / admin |
| **RabbitMQ 管理画面** | 15672 | http://localhost:15672 | admin / admin |

### よく使うコマンド

```bash
# サービス起動
docker-compose -f docker-compose.microservices.yml up -d

# ログ確認
docker-compose -f docker-compose.microservices.yml logs -f

# テスト実行
cd management-accounting-service && npm test

# データベース接続
psql postgresql://fa_user:fa_password@localhost:5432/financial_accounting

# サービス停止
docker-compose -f docker-compose.microservices.yml down
```

## アーキテクチャ概要

このシステムは、DDDの境界付けられたコンテキストに基づいて、以下の2つのマイクロサービスに分割されています：

### 財務会計サービス（Financial Accounting Service）
- **責務**: 会計基準に基づく正確な財務記録と報告
- **ポート**: 3001
- **データベース**: PostgreSQL (ポート 5432)
- **主要機能**:
  - 勘定科目管理
  - 仕訳管理
  - 残高計算
  - 財務諸表生成
  - 監査ログ

### 管理会計サービス（Management Accounting Service）
- **責務**: 経営判断のための財務分析と意思決定支援
- **ポート**: 3002
- **データベース**: PostgreSQL (ポート 5433)
- **主要機能**:
  - 財務分析
  - 財務比率計算
  - トレンド分析
  - 複数期間比較
  - **腐敗防止層（ACL）**: 財務会計サービスとの統合
  - **イベント駆動**: RabbitMQ 経由でのデータ同期

### API Gateway（Nginx）
- **ポート**: 8080
- **役割**:
  - 統一されたエントリーポイント
  - ルーティング
  - レート制限
  - CORS設定

### メッセージブローカー（RabbitMQ）
- **AMQPポート**: 5672
- **管理UIポート**: 15672
- **役割**:
  - サービス間の非同期通信
  - イベント駆動アーキテクチャ

## ディレクトリ構造

```
.
├── app/
│   └── typescript-ms/                      # マイクロサービス（TypeScript）
│       ├── financial-accounting-service/   # 財務会計サービス
│       │   ├── src/
│       │   │   ├── domain/                 # ドメイン層
│       │   │   ├── application/            # アプリケーション層
│       │   │   ├── infrastructure/         # インフラストラクチャ層
│       │   │   ├── server.ts              # エントリーポイント
│       │   │   └── config.ts              # 設定
│       │   ├── prisma/
│       │   │   └── schema.prisma          # データベーススキーマ
│       │   ├── Dockerfile
│       │   ├── package.json
│       │   └── tsconfig.json
│       │
│       ├── management-accounting-service/  # 管理会計サービス
│       │   ├── src/
│       │   │   ├── domain/                 # ドメイン層
│       │   │   ├── application/            # アプリケーション層
│       │   │   ├── infrastructure/         # インフラストラクチャ層
│       │   │   ├── server.ts              # エントリーポイント
│       │   │   └── config.ts              # 設定
│       │   ├── prisma/
│       │   │   └── schema.prisma          # データベーススキーマ
│       │   ├── Dockerfile
│       │   ├── package.json
│       │   └── tsconfig.json
│       │
│       ├── nginx/                          # API Gateway
│       │   ├── nginx.conf
│       │   └── Dockerfile
│       │
│       ├── shared/                         # 共有コード
│       │   └── events/                     # ドメインイベント
│       │       └── DomainEvent.ts
│       │
│       └── tests/                          # 統合テスト
│           └── integration/
│
└── docker-compose.microservices.yml        # マルチサービス統合
```

## アーキテクチャパターン

### 腐敗防止層（Anti-Corruption Layer: ACL）

管理会計サービスは、財務会計サービスとの統合において ACL パターンを実装しています。

#### ACL の構成

```
財務会計サービス
    ↓ (JournalCreated イベント)
[RabbitMQ]
    ↓
管理会計サービス
    ↓
[EventSubscriber]
    ↓
[JournalCreatedHandler]
    ↓
[FinancialAccountingEventTranslator] ← 腐敗防止層
    ↓ (外部 DTO → 内部モデル)
[JournalCacheRepository]
    ↓
[PostgreSQL]
```

#### ACL の責務

1. **外部 DTO の定義**: 財務会計サービスのイベント構造を隔離
2. **イベント翻訳**: 外部モデル → 内部ドメインモデルへの変換
3. **バリデーション**: データ整合性の検証（貸借一致チェックなど）
4. **HTTP アダプター**: 同期通信用の抽象化層

**実装ファイル**:
- `src/domain/models/external/FinancialAccountingEvent.ts`
- `src/application/translators/FinancialAccountingEventTranslator.ts`
- `src/infrastructure/adapters/FinancialAccountingAdapter.ts`
- `src/application/services/FinancialAccountingService.ts`

### イベント駆動アーキテクチャ

サービス間の通信は RabbitMQ を使用したイベント駆動アーキテクチャで実装されています。

#### RabbitMQ 設定

- **Exchange**: `financial-accounting-events` (Topic)
- **Queue**: `management-accounting-queue`
- **Routing Key**: `journal.created`
- **メッセージ永続化**: 有効
- **手動 ACK**: 有効

#### イベントフロー

1. 財務会計サービスが仕訳を作成
2. `JournalCreated` イベントを RabbitMQ にパブリッシュ
3. 管理会計サービスの EventSubscriber がイベントを受信
4. ACL を通じて内部モデルに変換
5. JournalCache テーブルに保存

## テスト戦略

### テストピラミッド

```
        /\
       /E2E\        ← マルチサービス統合テスト (7 tests)
      /------\
     /統合テスト\    ← ハンドラー・リポジトリ (10 tests)
    /----------\
   /  単体テスト  \  ← ACL 翻訳層 (10 tests)
  /--------------\
```

### TestContainers による統合テスト

実際の PostgreSQL と RabbitMQ コンテナを使用したテストを実装：

```bash
# 全テストを実行
cd management-accounting-service
npm test

# 特定のテストのみ実行
npm test -- tests/unit/translators/FinancialAccountingEventTranslator.test.ts
npm test -- tests/integration/handlers/JournalCreatedHandler.test.ts
npm test -- tests/e2e/multi-service.test.ts
```

**テスト実行結果**:
```
Test Files: 4 passed (4)
Tests: 27 passed (27)
Duration: ~51s
```

**テスト内容**:
- ✅ ACL 翻訳ロジック（10 テスト）
- ✅ リポジトリ永続化（5 テスト）
- ✅ イベントハンドラー（5 テスト）
- ✅ マルチサービス E2E（7 テスト）

詳細は [実装サマリー](management-accounting-service/docs/IMPLEMENTATION_SUMMARY.md) を参照。

## セットアップ

### 前提条件

- Docker & Docker Compose
- Node.js 20+ (ローカル開発用)

### 1. サービスのビルドと起動

```bash
# app/typescript-ms ディレクトリに移動
cd app/typescript-ms

# すべてのサービスをビルドして起動
docker-compose -f docker-compose.microservices.yml up --build -d

# ログを確認
docker-compose -f docker-compose.microservices.yml logs -f

# 特定のサービスのログを確認
docker-compose -f docker-compose.microservices.yml logs -f financial-accounting
docker-compose -f docker-compose.microservices.yml logs -f management-accounting
```

### 2. データベースマイグレーション

```bash
# 財務会計サービスのマイグレーション
docker-compose -f docker-compose.microservices.yml exec financial-accounting npm run prisma:migrate

# 管理会計サービスのマイグレーション
docker-compose -f docker-compose.microservices.yml exec management-accounting npm run prisma:migrate
```

### 3. ヘルスチェック

```bash
# API Gateway
curl http://localhost:8080/health

# 財務会計サービス
curl http://localhost:8080/api/fa/health

# 管理会計サービス
curl http://localhost:8080/api/ma/health

# RabbitMQ 管理画面
open http://localhost:15672  # admin/admin でログイン
```

## API エンドポイント

### 財務会計サービス (`/api/fa/`)

- `GET /api/fa/` - サービス情報
- `GET /api/fa/health` - ヘルスチェック
- `GET /api/fa/accounts` - 勘定科目一覧
- `POST /api/fa/accounts` - 勘定科目作成
- `GET /api/fa/journals` - 仕訳一覧
- `POST /api/fa/journals` - 仕訳作成
- `GET /api/fa/audit-logs` - 監査ログ

### 管理会計サービス (`/api/ma/`)

- `GET /api/ma/` - サービス情報
- `GET /api/ma/health` - ヘルスチェック
- `GET /api/ma/financial-analysis/:fiscalYear` - 財務分析
- `GET /api/ma/financial-analysis/compare?years=2021,2022,2023` - 複数期間比較

## イベント駆動アーキテクチャの動作確認

### 1. 財務会計サービスで仕訳を作成

**注意**: `accountType` は以下のいずれかを指定してください：
- `資産` - 資産科目
- `負債` - 負債科目
- `純資産` - 純資産科目
- `収益` - 収益科目
- `費用` - 費用科目

```bash
# 勘定科目を作成
curl -X POST http://localhost:8080/api/fa/accounts \
  -H "Content-Type: application/json" \
  -d '{
    "accountCode": "1010",
    "accountName": "現金",
    "accountType": "資産",
    "fiscalYear": 2024
  }'

# 仕訳を作成（JournalCreated イベントが発行される）
curl -X POST http://localhost:8080/api/fa/journals \
  -H "Content-Type: application/json" \
  -d '{
    "fiscalYear": 2024,
    "journalDate": "2024-01-15",
    "entries": [
      {
        "accountCode": "1010",
        "debitAmount": 10000,
        "creditAmount": 0,
        "description": "現金受取"
      },
      {
        "accountCode": "1010",
        "debitAmount": 0,
        "creditAmount": 10000,
        "description": "現金支払"
      }
    ]
  }'
```

### 2. 管理会計サービスでイベント受信を確認

```bash
# RabbitMQ 管理画面でメッセージ確認
open http://localhost:15672

# 管理会計サービスのログでイベント処理を確認
docker-compose -f docker-compose.microservices.yml logs -f management-accounting

# 期待されるログ出力:
# 📥 Event received: journal.created
# 🔄 Processing Financial Accounting event
# ✅ Journal cache saved: journalId=1, fiscalYear=2024
```

### 3. データベースで同期を確認

```bash
# 管理会計データベースに接続
docker-compose -f docker-compose.microservices.yml exec management-accounting-db psql -U ma_user -d management_accounting

# JournalCache テーブルを確認
SELECT * FROM "JournalCache";

# 期待される結果:
# journalId | fiscalYear | journalDate | totalDebitAmount | totalCreditAmount | receivedAt
# ----------+------------+-------------+------------------+-------------------+------------
# 1         | 2024       | 2024-01-15  | 10000           | 10000             | [timestamp]
```

## 開発

### ローカル開発環境

各サービスをローカルで開発する場合：

```bash
# 財務会計サービス
cd app/typescript-ms/financial-accounting-service
npm install
npm run dev

# 管理会計サービス
cd app/typescript-ms/management-accounting-service
npm install
npm run dev
```

### データベース接続

#### 財務会計サービス

**本番・開発環境**:
```bash
# 接続文字列
postgresql://fa_user:fa_password@localhost:5432/financial_accounting

# 環境変数
FINANCIAL_ACCOUNTING_DATABASE_URL="postgresql://fa_user:fa_password@localhost:5432/financial_accounting"

# 接続情報
ホスト: localhost
ポート: 5432
データベース名: financial_accounting
ユーザー名: fa_user
パスワード: fa_password
```

**Docker Compose 環境**:
```bash
# コンテナ内から接続
FINANCIAL_ACCOUNTING_DATABASE_URL="postgresql://fa_user:fa_password@financial-accounting-db:5432/financial_accounting"

# ホストから接続（ポートフォワーディング）
FINANCIAL_ACCOUNTING_DATABASE_URL="postgresql://fa_user:fa_password@localhost:5432/financial_accounting"
```

#### 管理会計サービス

**本番・開発環境**:
```bash
# 接続文字列
postgresql://ma_user:ma_password@localhost:5433/management_accounting

# 環境変数
MANAGEMENT_ACCOUNTING_DATABASE_URL="postgresql://ma_user:ma_password@localhost:5433/management_accounting"

# 接続情報
ホスト: localhost
ポート: 5433
データベース名: management_accounting
ユーザー名: ma_user
パスワード: ma_password
```

**Docker Compose 環境**:
```bash
# コンテナ内から接続
MANAGEMENT_ACCOUNTING_DATABASE_URL="postgresql://ma_user:ma_password@management-accounting-db:5432/management_accounting"

# ホストから接続（ポートフォワーディング）
MANAGEMENT_ACCOUNTING_DATABASE_URL="postgresql://ma_user:ma_password@localhost:5433/management_accounting"
```

#### TestContainers（テスト環境）

テスト実行時は動的にコンテナが起動され、接続情報は自動的に設定されます：

```typescript
// tests/setup/test-containers.ts で自動設定
const postgresContainer = await new PostgreSqlContainer('postgres:16-alpine')
  .withDatabase('test_management_accounting')
  .withUsername('test_user')
  .withPassword('test_password')
  .start()

// 環境変数に自動設定される
process.env.MANAGEMENT_ACCOUNTING_DATABASE_URL = postgresContainer.getConnectionUri()
// 例: postgresql://test_user:test_password@localhost:xxxxx/test_management_accounting
```

#### データベースクライアントでの接続

**psql を使用した接続**:

```bash
# 財務会計データベース
psql postgresql://fa_user:fa_password@localhost:5432/financial_accounting

# 管理会計データベース
psql postgresql://ma_user:ma_password@localhost:5433/management_accounting

# Docker 経由での接続
docker-compose -f docker-compose.microservices.yml exec financial-accounting-db \
  psql -U fa_user -d financial_accounting

docker-compose -f docker-compose.microservices.yml exec management-accounting-db \
  psql -U ma_user -d management_accounting
```

**GUI クライアント（DBeaver, pgAdmin など）での接続**:

財務会計データベース:
- Host: localhost
- Port: 5432
- Database: financial_accounting
- Username: fa_user
- Password: fa_password

管理会計データベース:
- Host: localhost
- Port: 5433
- Database: management_accounting
- Username: ma_user
- Password: ma_password

### RabbitMQ 接続

#### 本番・開発環境

**AMQP 接続（アプリケーション用）**:
```bash
# 接続文字列
amqp://admin:admin@localhost:5672

# 環境変数
RABBITMQ_URL="amqp://admin:admin@localhost:5672"

# 接続情報
ホスト: localhost
ポート: 5672 (AMQP)
ユーザー名: admin
パスワード: admin
Virtual Host: / (デフォルト)
```

**管理画面（ブラウザアクセス）**:
```bash
# URL
http://localhost:15672

# ログイン情報
ユーザー名: admin
パスワード: admin

# アクセス方法
open http://localhost:15672
```

#### Docker Compose 環境

```bash
# コンテナ内から接続
RABBITMQ_URL="amqp://admin:admin@rabbitmq:5672"

# ホストから接続（ポートフォワーディング）
RABBITMQ_URL="amqp://admin:admin@localhost:5672"

# 管理画面
# http://localhost:15672 でアクセス可能
```

#### TestContainers（テスト環境）

```typescript
// tests/setup/test-containers.ts で自動設定
const rabbitmqContainer = await new RabbitMQContainer('rabbitmq:3-management-alpine')
  .withExposedPorts(5672, 15672)
  .start()

// 環境変数に自動設定される
process.env.RABBITMQ_URL = rabbitmqContainer.getAmqpUrl()
// 例: amqp://localhost:xxxxx
```

#### Exchange と Queue の設定

**財務会計サービス（Publisher）**:
```typescript
Exchange Name: financial-accounting-events
Exchange Type: topic
Durable: true
Routing Keys:
  - journal.created
  - journal.updated
  - journal.deleted
```

**管理会計サービス（Subscriber）**:
```typescript
Exchange Name: financial-accounting-events
Queue Name: management-accounting-queue
Routing Key: journal.created
Durable: true
Auto ACK: false (手動 ACK)
```

#### RabbitMQ 管理コマンド

```bash
# Docker 経由で RabbitMQ にアクセス
docker-compose -f docker-compose.microservices.yml exec rabbitmq sh

# Queue の確認
rabbitmqctl list_queues

# Exchange の確認
rabbitmqctl list_exchanges

# Binding の確認
rabbitmqctl list_bindings

# 接続の確認
rabbitmqctl list_connections

# Consumer の確認
rabbitmqctl list_consumers
```

### 環境変数一覧

**.env ファイル例（ローカル開発）**:

```bash
# 財務会計サービス
FINANCIAL_ACCOUNTING_DATABASE_URL="postgresql://fa_user:fa_password@localhost:5432/financial_accounting"
FINANCIAL_ACCOUNTING_PORT=3001
RABBITMQ_URL="amqp://admin:admin@localhost:5672"

# 管理会計サービス
MANAGEMENT_ACCOUNTING_DATABASE_URL="postgresql://ma_user:ma_password@localhost:5433/management_accounting"
MANAGEMENT_ACCOUNTING_PORT=3002
RABBITMQ_URL="amqp://admin:admin@localhost:5672"

# RabbitMQ
RABBITMQ_DEFAULT_USER=admin
RABBITMQ_DEFAULT_PASS=admin
```

## サービスの停止とクリーンアップ

```bash
# app/typescript-ms ディレクトリで実行

# サービスの停止
docker-compose -f docker-compose.microservices.yml down

# ボリュームも含めて完全削除
docker-compose -f docker-compose.microservices.yml down -v

# 個別のサービスの再起動
docker-compose -f docker-compose.microservices.yml restart financial-accounting
docker-compose -f docker-compose.microservices.yml restart management-accounting
```

## トラブルシューティング

### サービスが起動しない

```bash
# コンテナの状態を確認
docker-compose -f docker-compose.microservices.yml ps

# サービスのログを確認
docker-compose -f docker-compose.microservices.yml logs [service-name]

# コンテナ内でコマンドを実行
docker-compose -f docker-compose.microservices.yml exec [service-name] sh
```

### データベース接続エラー

1. データベースコンテナが起動しているか確認
2. ヘルスチェックが成功しているか確認
3. 環境変数の DATABASE_URL が正しいか確認

### ポート競合

既存のサービスがポートを使用している場合、docker-compose.microservices.yml のポート設定を変更してください。

## 実装状況

### ✅ 完了済み

1. **API 実装の拡充**
   - ✅ 勘定科目 CRUD
   - ✅ 仕訳 CRUD
   - ✅ 残高計算
   - ✅ 財務諸表生成の基盤

2. **サービス間通信**
   - ✅ RabbitMQ によるイベント駆動アーキテクチャ
   - ✅ 財務会計からのイベント発行（EventPublisher）
   - ✅ 管理会計でのイベント購読（EventSubscriber）
   - ✅ 腐敗防止層（ACL）の実装

3. **テスト**
   - ✅ TestContainers による統合テスト
   - ✅ E2E マルチサービステスト
   - ✅ 単体・統合・E2E のテストピラミッド
   - ✅ 27 テスト全てパス

### 🚧 次のステップ

以下の機能を段階的に追加できます：

1. **認証・認可**
   - JWT ベースの認証
   - API Gateway での認証統合
   - RBAC（ロールベースアクセス制御）

2. **監視とログ**
   - Prometheus + Grafana によるメトリクス収集
   - ログ集約（ELK Stack または Loki）
   - 分散トレーシング（Jaeger / OpenTelemetry）

3. **エラーハンドリングと回復性**
   - デッドレターキュー（DLQ）
   - エクスポネンシャルバックオフ
   - サーキットブレーカー
   - リトライポリシー

4. **パフォーマンス最適化**
   - イベントバッチ処理
   - データベースインデックス最適化
   - コネクションプーリング
   - キャッシュ戦略（Redis）

5. **高度なパターン**
   - SAGA パターン（分散トランザクション）
   - CQRS（コマンドクエリ責務分離）
   - イベントソーシング

## 参考資料

### プロジェクトドキュメント

- [第8章：境界付けられたコンテキスト](docs/wiki/記事/データベース/実践データベース設計/財務会計/TypeScript.md#第8章境界付けられたコンテキスト)
- [腐敗防止層（ACL）実装ガイド](management-accounting-service/docs/ACL.md)
- [実装サマリー](management-accounting-service/docs/IMPLEMENTATION_SUMMARY.md)

### 書籍

- [ドメイン駆動設計](https://www.amazon.co.jp/dp/4798121967)
- [マイクロサービスパターン](https://www.amazon.co.jp/dp/4295008362)
- [実践ドメイン駆動設計](https://www.amazon.co.jp/dp/479813161X)
