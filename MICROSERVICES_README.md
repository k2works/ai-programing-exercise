# 財務会計システム - マイクロサービスアーキテクチャ

第8章：境界付けられたコンテキストに基づいたマイクロサービス実装

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

## セットアップ

### 前提条件

- Docker & Docker Compose
- Node.js 20+ (ローカル開発用)

### 1. サービスのビルドと起動

```bash
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

**財務会計サービス**:
```
postgresql://fa_user:fa_password@localhost:5432/financial_accounting
```

**管理会計サービス**:
```
postgresql://ma_user:ma_password@localhost:5433/management_accounting
```

## サービスの停止とクリーンアップ

```bash
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

## 次のステップ

現在の実装は最小限の構成です。以下の機能を段階的に追加できます：

1. **API 実装の拡充**
   - 勘定科目CRUD
   - 仕訳CRUD
   - 財務諸表生成

2. **サービス間通信**
   - RabbitMQ によるイベント駆動
   - 財務会計からのイベント発行
   - 管理会計でのイベント購読

3. **認証・認可**
   - JWT ベースの認証
   - API Gatewayでの認証統合

4. **監視とログ**
   - Prometheus + Grafana
   - ログ集約（ELK Stack）

5. **テスト**
   - TestContainers による統合テスト
   - エンドツーエンドテスト

## 参考資料

- [第8章：境界付けられたコンテキスト](docs/wiki/記事/データベース/実践データベース設計/財務会計/TypeScript.md#第8章境界付けられたコンテキスト)
- [ドメイン駆動設計](https://www.amazon.co.jp/dp/4798121967)
- [マイクロサービスパターン](https://www.amazon.co.jp/dp/4295008362)
