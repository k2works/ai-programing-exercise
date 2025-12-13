# 財務会計システム - マイクロサービス版（第9章）

ドメイン駆動設計（DDD）の**境界付けられたコンテキスト（Bounded Context）**パターンを適用した、マイクロサービスアーキテクチャによる財務会計システムです。

## アーキテクチャ概要

### 境界付けられたコンテキスト

本システムは、2つの境界付けられたコンテキストで構成されています：

1. **財務会計コンテキスト（Financial Accounting Context）**
   - 会計基準に基づく正確な財務記録と報告
   - 複式簿記の原則を保証
   - 監査証跡の維持
   - 外部報告の基礎データ

2. **管理会計コンテキスト（Management Accounting Context）**
   - 経営判断のための財務分析
   - 収益性・効率性・安全性の評価
   - 複数期間の比較分析
   - 意思決定支援

### コンテキストマップ

```mermaid
flowchart TD
    FA["財務会計コンテキスト<br/>(Financial Accounting Context)<br/>・会計基準に基づく記録<br/>・複式簿記の原則を保証<br/>・監査証跡の維持"]
    MA["管理会計コンテキスト<br/>(Management Accounting Context)<br/>・経営判断のための分析<br/>・収益性・効率性・安全性の評価<br/>・腐敗防止層(ACL)で財務会計と統合"]

    FA -->|"上流 (Upstream)<br/>仕訳データを提供"| MA

    style FA fill:#e1f5ff,stroke:#01579b,stroke-width:2px
    style MA fill:#fff3e0,stroke:#e65100,stroke-width:2px
```

### サービス構成

```mermaid
flowchart TD
    GW["API Gateway<br/>(Axum)<br/>Port: 8090"]
    FA["財務会計サービス<br/>Port: 8080"]
    MA["管理会計サービス<br/>Port: 8081"]
    RMQ["RabbitMQ<br/>(Events)<br/>Port: 5672/15672"]
    DB["PostgreSQL<br/>Port: 5432"]

    GW --> FA
    GW --> MA
    GW --> RMQ
    FA --> DB

    style GW fill:#b2dfdb,stroke:#00695c,stroke-width:2px
    style FA fill:#e1f5ff,stroke:#01579b,stroke-width:2px
    style MA fill:#fff3e0,stroke:#e65100,stroke-width:2px
    style RMQ fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    style DB fill:#e8f5e9,stroke:#1b5e20,stroke-width:2px
```

## ディレクトリ構造

```
accounting-service/
├── Cargo.toml                 # Workspace 設定
├── docker-compose.yml         # マルチサービス起動設定
├── README.md
│
├── shared/                    # 共有ライブラリ
│   ├── Cargo.toml
│   └── src/
│       ├── lib.rs
│       └── error.rs           # 共通エラー型
│
├── financial_accounting/      # 財務会計コンテキスト
│   ├── Cargo.toml
│   ├── Dockerfile
│   ├── src/
│   │   ├── main.rs           # Axum サーバー
│   │   ├── lib.rs
│   │   ├── domain/           # ドメインモデル
│   │   ├── application/      # アプリケーションサービス
│   │   ├── infrastructure/   # インフラストラクチャ
│   │   └── repositories/     # リポジトリ
│   └── migrations/           # データベースマイグレーション
│
├── management_accounting/     # 管理会計コンテキスト
│   ├── Cargo.toml
│   ├── Dockerfile
│   └── src/
│       ├── main.rs           # Axum サーバー
│       ├── lib.rs
│       ├── domain/
│       │   ├── mod.rs
│       │   └── financial_data.rs  # 管理会計のドメインモデル
│       ├── application/
│       │   ├── mod.rs
│       │   └── financial_analysis_service.rs
│       └── infrastructure/
│           └── adapters/
│               ├── mod.rs
│               └── financial_accounting_adapter.rs  # 腐敗防止層（ACL）
│
└── api_gateway/              # API Gateway
    ├── Cargo.toml
    ├── Dockerfile
    └── src/
        └── main.rs          # リバースプロキシ
```

## 技術スタック

- **言語**: Rust 1.83+
- **Web フレームワーク**: Axum 0.7
- **データベース**: PostgreSQL 16
- **メッセージブローカー**: RabbitMQ 3
- **コンテナ**: Docker, Docker Compose
- **非同期ランタイム**: Tokio
- **HTTP クライアント**: reqwest

## セットアップ

### 前提条件

- Docker
- Docker Compose
- Rust 1.83+ （ローカル開発用）
- [Just](https://github.com/casey/just) （タスクランナー、推奨）

#### Just のインストール

```bash
# macOS
brew install just

# Linux
curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin

# Cargo経由
cargo install just
```

### Just によるタスク管理（推奨）

Just を使用すると、複雑なコマンドを簡単に実行できます。

```bash
# 利用可能なタスク一覧を表示
just

# Docker Composeでサービスを起動
just up

# バックグラウンドで起動
just up-d

# ログを確認
just logs

# 特定のサービスのログのみ確認
just logs-financial
just logs-management
just logs-gateway

# サービスを停止
just down

# ボリュームも削除して完全にクリーンアップ
just down-v

# ワークスペース全体をビルド
just build

# 特定のサービスのみビルド
just build-financial
just build-management
just build-gateway

# 全テスト実行
just test

# コードフォーマット
just fmt

# Clippy による静的解析
just clippy
```

### ローカル開発用コマンド

```bash
# PostgreSQL と RabbitMQ のみ起動
just dev-db

# 財務会計サービスをローカル起動（別ターミナル）
just dev-financial

# 管理会計サービスをローカル起動（別ターミナル）
just dev-management

# API Gateway をローカル起動（別ターミナル）
just dev-gateway
```

### データベース操作

```bash
# マイグレーション実行
just migrate

# Seedデータ投入
just seed

# 日次残高集計
just aggregate

# Seedデータ検証
just verify-seed
```

### API テスト

```bash
# ヘルスチェック
just health-financial
just health-management
just health-gateway

# 財務分析APIテスト
just test-analysis

# 期間比較APIテスト
just test-comparison
```

### Docker Compose での起動（直接実行）

```bash
# サービスをビルド＆起動
docker-compose up --build

# バックグラウンドで起動
docker-compose up -d

# ログを確認
docker-compose logs -f

# 停止
docker-compose down

# ボリュームも削除して完全にクリーンアップ
docker-compose down -v
```

### サービスエンドポイント

起動後、以下のエンドポイントが利用可能です：

#### API Gateway (Port 8090)
- すべてのリクエストは API Gateway 経由でアクセス可能

#### 財務会計サービス (Port 8080)
- `GET /api/v1/accounts` - 勘定科目一覧
- `POST /api/v1/accounts` - 勘定科目作成
- `GET /api/v1/journals` - 仕訳一覧
- `POST /api/v1/journals` - 仕訳作成
- `GET /api/v1/financial/balance-sheet` - 貸借対照表
- `GET /api/v1/financial/income-statement` - 損益計算書
- `GET /api/v1/audit-logs` - 監査ログ

#### 管理会計サービス (Port 8081)
- `GET /api/v1/analysis/:fiscal_year` - 会計年度別財務分析
- `GET /api/v1/comparison?current_year=2023&previous_year=2022` - 期間比較分析

#### RabbitMQ Management UI (Port 15672)
- URL: http://localhost:15672
- ユーザー名: guest
- パスワード: guest

## ローカル開発（Just を使わない場合）

### 各サービスの個別起動

```bash
# PostgreSQL と RabbitMQ のみ起動
docker-compose up -d postgres-financial rabbitmq

# 財務会計サービスを起動
cd financial_accounting
DATABASE_URL=postgres://postgres:postgres@localhost/financial_accounting \
RABBITMQ_URL=amqp://guest:guest@localhost:5672 \
cargo run

# 管理会計サービスを起動（別ターミナル）
cd management_accounting
FINANCIAL_SERVICE_URL=http://localhost:8080 \
cargo run

# API Gateway を起動（別ターミナル）
cd api_gateway
FINANCIAL_SERVICE_URL=http://localhost:8080 \
MANAGEMENT_SERVICE_URL=http://localhost:8081 \
cargo run
```

### ビルド

```bash
# ワークスペース全体をビルド
cargo build --release

# 特定のクレートのみビルド
cargo build -p financial_accounting --release
cargo build -p management_accounting --release
cargo build -p api_gateway --release
```

### テスト

```bash
# 全テスト実行
cargo test

# 特定のクレートのテストのみ
cargo test -p management_accounting
```

## 境界付けられたコンテキストの設計ポイント

### 1. ユビキタス言語の範囲

各コンテキストは独自のユビキタス言語を持ちます：

**財務会計**: 仕訳、勘定科目、借方、貸方、会計期間、財務諸表
**管理会計**: 財務比率、営業利益率、総資産回転率、自己資本比率、トレンド分析

### 2. データモデルの分離

財務会計と管理会計は**異なる型**を使用します：

```rust
// 財務会計のドメインモデル
pub struct Journal {
    pub journal_date: NaiveDate,
    pub entries: Vec<JournalEntry>,
}

// 管理会計のドメインモデル
pub struct FinancialData {
    pub fiscal_year: i32,
    pub sales: Decimal,
    pub operating_profit: Decimal,
    // ...
}
```

### 3. 腐敗防止層（Anti-Corruption Layer: ACL）

管理会計コンテキストは、財務会計コンテキストのデータモデル変更から保護されます：

```rust
// management_accounting/src/infrastructure/adapters/financial_accounting_adapter.rs
pub struct FinancialAccountingAdapter {
    client: Client,
    base_url: String,
}

impl FinancialAccountingAdapter {
    // 財務会計の仕訳データを管理会計のドメインモデルに変換
    pub async fn fetch_financial_data_by_fiscal_year(
        &self,
        fiscal_year: i32,
    ) -> Result<FinancialData> {
        // ...
    }
}
```

### 4. Database per Service パターン

各サービスは独立したデータベースを持ちます（現在は財務会計のみ使用）：

- `postgres-financial`: 財務会計専用
- `postgres-management`: 管理会計専用（将来的にキャッシュ用）

## API 使用例

### 財務分析の取得

```bash
# 2023年度の財務比率を取得
curl http://localhost:8090/api/v1/analysis/2023

# レスポンス例
{
  "operating_profit_margin": "15.5",
  "total_asset_turnover": "1.2",
  "tangible_fixed_asset_turnover": "3.5",
  "current_ratio": "150.0",
  "quick_ratio": "120.0",
  "equity_ratio": "45.0"
}
```

### 期間比較分析

```bash
# 2023年度と2022年度を比較
curl "http://localhost:8090/api/v1/comparison?current_year=2023&previous_year=2022"

# レスポンス例
{
  "current_year": 2023,
  "previous_year": 2022,
  "current_data": {
    "fiscal_year": 2023,
    "sales": "10000000",
    "operating_profit": "1550000",
    // ...
  },
  "previous_data": {
    "fiscal_year": 2022,
    "sales": "9000000",
    "operating_profit": "1350000",
    // ...
  },
  "current_ratios": { /* ... */ },
  "previous_ratios": { /* ... */ }
}
```

## 第9章で実装した主要パターン

1. ✅ **境界付けられたコンテキスト（Bounded Context）**
   - 財務会計と管理会計を明確に分離
   - 各コンテキストは独自のドメインモデルを持つ

2. ✅ **コンテキストマップ（Context Map）**
   - 上流/下流の関係を明確化
   - 顧客/供給者パターンを適用

3. ✅ **腐敗防止層（Anti-Corruption Layer: ACL）**
   - FinancialAccountingAdapter による変換
   - コンテキスト間の疎結合を実現

4. ✅ **Database per Service**
   - 各サービスが独立したデータベースを持つ
   - スケーラビリティと自律性の向上

5. ✅ **API Gateway パターン**
   - 統一エントリーポイント
   - サービスディスカバリの簡素化

6. ✅ **Docker Compose によるマルチサービスデプロイ**
   - コンテナ化による環境の一貫性
   - マイクロサービスのローカル開発環境

## 今後の拡張

- [ ] gRPC によるサービス間通信
- [ ] サービスメッシュ（Istio/Linkerd）の導入
- [ ] 分散トレーシング（OpenTelemetry）
- [ ] API Gateway での認証・認可
- [ ] Kubernetes へのデプロイ
- [ ] 管理会計サービス用の独自データベース活用
- [ ] イベントソーシングとの統合強化

## ライセンス

MIT License
