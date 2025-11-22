# 財務会計システム - マイクロサービス版

本プロジェクトは、財務会計システムをマイクロサービスアーキテクチャで実装したものです。
境界付けられたコンテキスト（Bounded Context）に基づき、システムを独立したサービスに分割しています。

## アーキテクチャ概要

```
┌─────────────┐
│  クライアント  │
└──────┬──────┘
       │ HTTP
       ▼
┌─────────────┐
│ API Gateway │ (Port 8080)
│Spring Cloud │
│  Gateway    │
└──────┬──────┘
       │
       ├───────────────────────┬────────────────────────┐
       │                       │                        │
       ▼                       ▼                        ▼
┌──────────────┐      ┌───────────────┐      ┌────────────────┐
│  財務会計     │      │   管理会計      │      │   PostgreSQL   │
│  サービス     │◄─────│   サービス      │      │   (財務会計DB)  │
│ (Port 8081)  │      │  (Port 8082)   │      │                │
└──────┬───────┘      └───────────────┘      └────────────────┘
       │
       │
       ▼
┌──────────────┐
│ PostgreSQL   │
│ (財務会計DB)  │
└──────────────┘
```

## サービス構成

### 1. API Gateway (Port 8080)

**技術スタック**: Spring Cloud Gateway

**責務**: クライアントリクエストのルーティング

**提供機能**:
- ルーティング（パスベース）
- CORS 設定
- 認証・認可（将来実装予定）
- レート制限（将来実装予定）

**ルーティング設定**:
- `/api/v1/accounts/**` → 財務会計サービス
- `/api/v1/journals/**` → 財務会計サービス
- `/api/v1/financial-statements/**` → 財務会計サービス
- `/api/v1/financial-analysis/**` → 管理会計サービス
- `/api/v1/financial-ratios/**` → 管理会計サービス

### 2. Financial Accounting Service (Port 8081)

**技術スタック**: Spring Boot, MyBatis, PostgreSQL

**責務**: 複式簿記に基づく取引記録と財務諸表生成

**提供 API**:
- `POST /api/v1/accounts` - 勘定科目の作成
- `GET /api/v1/accounts` - 勘定科目の一覧取得
- `GET /api/v1/accounts/{code}` - 勘定科目の取得
- `POST /api/v1/journals` - 仕訳の作成
- `GET /api/v1/journals` - 仕訳の一覧取得
- `GET /api/v1/financial-statements/balance-sheet` - 貸借対照表の生成
- `GET /api/v1/financial-statements/income-statement` - 損益計算書の生成

**データベース**: PostgreSQL (financial_accounting)

### 3. Management Accounting Service (Port 8082)

**技術スタック**: Spring Boot, WebClient

**責務**: 財務分析と経営指標の計算

**提供 API**:
- `GET /api/v1/financial-analysis/{fiscalYear}` - 財務分析の実行
- `GET /api/v1/financial-analysis/compare` - 複数期間の比較分析
- `GET /api/v1/financial-ratios/{fiscalYear}` - 財務比率の計算

**特徴**: 財務会計サービスから財務データを取得し、分析結果を提供します（データベースは持たない）

## 開発環境セットアップ

### 必要な環境

- Java 21
- Gradle 8.x
- Docker & Docker Compose
- PostgreSQL 16

### ローカル開発

#### 1. PostgreSQL 起動

```bash
docker run -d \
  --name postgres-financial \
  -e POSTGRES_DB=financial_accounting \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  -p 5432:5432 \
  postgres:16-alpine
```

#### 2. サービス起動（個別）

```bash
# API Gateway
cd api-gateway
./gradlew bootRun

# Financial Accounting Service
cd financial-accounting-service
./gradlew bootRun

# Management Accounting Service
cd management-accounting-service
./gradlew bootRun
```

#### 3. Docker Compose で起動

```bash
# ビルド & 起動
docker-compose up --build

# バックグラウンドで起動
docker-compose up -d

# 停止
docker-compose down

# 停止 & ボリューム削除
docker-compose down -v
```

### テスト実行

```bash
# 全サービスのテスト実行
./gradlew test

# 特定サービスのテスト
./gradlew :financial-accounting-service:test
./gradlew :management-accounting-service:test
```

## API エンドポイント

### ヘルスチェック

```bash
# API Gateway
curl http://localhost:8080/actuator/health

# Financial Accounting Service
curl http://localhost:8081/actuator/health

# Management Accounting Service
curl http://localhost:8082/actuator/health
```

### API ドキュメント（Swagger UI）

- Financial Accounting Service: http://localhost:8081/swagger-ui.html
- Management Accounting Service: http://localhost:8082/swagger-ui.html

## プロジェクト構造

```
java-ms/
├── api-gateway/                    # API Gateway
│   ├── src/
│   │   └── main/
│   │       ├── java/
│   │       │   └── com/example/gateway/
│   │       │       └── ApiGatewayApplication.java
│   │       └── resources/
│   │           └── application.yml
│   └── build.gradle
│
├── financial-accounting-service/   # 財務会計サービス
│   ├── src/
│   │   ├── main/
│   │   │   ├── java/
│   │   │   │   └── com/example/financial/
│   │   │   │       ├── domain/         # ドメインモデル
│   │   │   │       ├── application/    # アプリケーション層
│   │   │   │       ├── infrastructure/ # インフラストラクチャ層
│   │   │   │       └── FinancialAccountingApplication.java
│   │   │   └── resources/
│   │   │       ├── db/migration/       # Flyway マイグレーション
│   │   │       ├── mapper/             # MyBatis Mapper XML
│   │   │       └── application.yml
│   │   └── test/
│   └── build.gradle
│
├── management-accounting-service/  # 管理会計サービス
│   ├── src/
│   │   ├── main/
│   │   │   ├── java/
│   │   │   │   └── com/example/management/
│   │   │   │       ├── domain/         # ドメインモデル
│   │   │   │       ├── application/    # アプリケーション層
│   │   │   │       ├── infrastructure/ # インフラストラクチャ層
│   │   │   │       └── ManagementAccountingApplication.java
│   │   │   └── resources/
│   │   │       └── application.yml
│   │   └── test/
│   └── build.gradle
│
├── build.gradle                    # ルートビルドファイル
├── settings.gradle                 # マルチプロジェクト設定
├── docker-compose.yml              # Docker Compose 設定
└── README.md
```

## 設計原則

### 境界付けられたコンテキスト（Bounded Context）

システムを以下の 2 つのコンテキストに分割：

1. **財務会計コンテキスト**
   - 責務: 会計基準に基づく正確な財務記録と報告
   - ユビキタス言語: 勘定科目、仕訳、残高、財務諸表

2. **管理会計コンテキスト**
   - 責務: 財務分析と経営指標の計算
   - ユビキタス言語: 財務分析、財務比率、トレンド分析

### Database per Service パターン

各サービスが独自のデータベースを持つことで：
- サービス間の疎結合を実現
- 独立したスケーリングが可能
- 技術スタックの自由な選択

### サービス間通信

- 同期通信: HTTP/REST (WebClient)
- 将来的には非同期通信（メッセージキュー）も検討

## トラブルシューティング

### ポート競合

デフォルトポートが使用中の場合、application.yml で変更できます：

```yaml
server:
  port: 8090  # 任意のポートに変更
```

### データベース接続エラー

PostgreSQL が起動しているか確認：

```bash
docker ps | grep postgres
```

### ビルドエラー

Gradle キャッシュをクリア：

```bash
./gradlew clean build --refresh-dependencies
```

## 参考資料

- [Spring Cloud Gateway](https://spring.io/projects/spring-cloud-gateway)
- [MyBatis](https://mybatis.org/mybatis-3/)
- [Flyway](https://flywaydb.org/)
- [Domain-Driven Design](https://www.domainlanguage.com/ddd/)

## ライセンス

MIT License
