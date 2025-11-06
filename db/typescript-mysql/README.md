# 販売管理システム (Sales Management DB)

## 概要

TypeScript と MySQL、Prisma ORM を使用した販売管理システムのデータベースおよび REST API 実装プロジェクトです。
TDD（テスト駆動開発）アプローチに基づき、レイヤードアーキテクチャで設計されています。

### 目的

- 実践的なデータベース設計の学習
- TypeScript による型安全な開発の実践
- TDD サイクル（Red-Green-Refactor）による品質担保
- REST API 設計のベストプラクティス習得
- Prisma ORM を使用した効率的なデータアクセス

### 主要機能

- **マスタデータ管理**: 部門、社員、商品、取引先（得意先・仕入先）
- **REST API**: Fastify による高速 Web API
- **API ドキュメント**: Swagger UI による対話的な API ドキュメント
- **バリデーション**: Zod による実行時型検証
- **テスト**: Vitest によるユニット・統合テスト
- **品質管理**: ESLint + Prettier による自動コード品質チェック

### 前提

| ソフトウェア | バージョン | 備考 |
| :----------- | :--------- | :--- |
| Node.js      | 24.x       | LTS 推奨 |
| MySQL        | 8.0        | Docker または ローカル |
| Docker       | 最新       | 任意（Docker Compose 使用時） |
| TypeScript   | 5.9.x      | |
| Prisma       | 6.18.x     | |

## 構成

- [クイックスタート](#クイックスタート)
- [構築](#構築)
- [配置](#配置)
- [運用](#運用)
- [開発](#開発)

## 詳細

### クイックスタート

#### Docker を使用する場合（推奨）

```bash
# リポジトリのクローン後、プロジェクトディレクトリに移動
cd db/typescript-mysql

# 依存パッケージのインストール
npm install

# データベースコンテナの起動
docker-compose up -d mysql

# Prisma マイグレーションの実行
npm run prisma:migrate

# シードデータの投入
npm run seed

# API サーバーの起動（開発モード）
npm run api:dev
```

ブラウザで `http://localhost:3000/docs` を開いて Swagger UI にアクセスできます。

#### ローカル MySQL を使用する場合

```bash
cd db/typescript-mysql

# .env ファイルの作成
cp .env.example .env
# .env ファイルを編集してローカル MySQL の接続情報を設定

npm install
npm run prisma:migrate
npm run seed
npm run api:dev
```

**[⬆ back to top](#構成)**

### 構築

#### 1. 依存パッケージのインストール

```bash
npm install
```

#### 2. 環境変数の設定

`.env` ファイルを作成し、データベース接続情報を設定します：

```env
# 本番用データベース
DATABASE_URL="mysql://user:password@localhost:3306/sales_management"

# テスト用データベース（任意）
TEST_DATABASE_URL="mysql://user:password@localhost:3306/sales_management_test"
```

#### 3. Docker Compose でデータベース起動（Docker 使用時）

```bash
docker-compose up -d mysql
```

#### 4. Prisma のセットアップ

```bash
# Prisma Client の生成
npm run prisma:generate

# マイグレーションの実行
npm run prisma:migrate

# シードデータの投入
npm run seed
```

**[⬆ back to top](#構成)**

### 配置

#### API サーバーの起動

**開発モード（ファイル変更を監視して自動再起動）**

```bash
npm run api:dev
```

**本番モード**

```bash
npm run api:start
```

#### SchemaSpyによるデータベースドキュメント生成

```bash
# SchemaSpyでドキュメント生成
npm run schemaspy

# 生成されたドキュメントの閲覧
npm run schemaspy:view
# http://localhost:8080 にアクセス
```

**[⬆ back to top](#構成)**

### 運用

#### ヘルスチェック

```bash
curl http://localhost:3000/health
```

#### ログ確認

API サーバーは Fastify のロガーを使用しています。開発モードではコンソールに詳細なログが出力されます。

#### データベースバックアップ

```bash
# Docker Compose 使用時
docker-compose exec mysql mysqldump -u user -p sales_management > backup.sql
```

**[⬆ back to top](#構成)**

### 開発

#### 開発フロー

このプロジェクトは TDD（テスト駆動開発）に従います：

1. **Red**: 失敗するテストを書く
2. **Green**: テストを通す最小限の実装
3. **Refactor**: コードを改善

#### 利用可能なコマンド

```bash
# テスト実行
npm test                    # 全テスト実行
npm run test:watch          # ウォッチモード
npm run test:coverage       # カバレッジ付き実行

# コード品質チェック
npm run lint                # ESLint チェック
npm run lint:fix            # ESLint 自動修正
npm run format              # Prettier フォーマット
npm run format:check        # Prettier チェック
npm run check               # すべてのチェック + 自動修正

# Gulp タスク
npm run guard               # ファイル監視 + 自動チェック
npm run watch               # ファイル変更監視

# Prisma
npm run prisma:migrate      # マイグレーション実行
npm run prisma:migrate:test # テスト DB にマイグレーション
npm run prisma:generate     # Prisma Client 生成
npm run seed                # シードデータ投入

# API サーバー
npm run api:dev             # 開発モード（ホットリロード）
npm run api:start           # 本番モード
```

#### プロジェクト構造

```
db/typescript-mysql/
├── prisma/
│   ├── schema.prisma       # Prisma スキーマ定義
│   └── seeds/              # シードデータ
├── src/
│   ├── api/                # API 層
│   │   ├── schemas/        # Zod バリデーションスキーマ
│   │   ├── domain/         # ドメイン層（データアクセス）
│   │   ├── service/        # サービス層（ビジネスロジック）
│   │   ├── application.ts  # Fastify アプリケーション
│   │   └── index.ts        # エントリーポイント
│   ├── seed.ts             # シードスクリプト
│   └── generated/          # 生成コード
├── tests/                  # テストファイル
├── gulpfile.js             # Gulp タスク定義
├── vitest.config.ts        # Vitest 設定
├── eslint.config.js        # ESLint 設定
└── docker-compose.yml      # Docker Compose 設定
```

#### レイヤードアーキテクチャ

```mermaid
graph TB
    subgraph Application["Application Layer (アプリケーション層)"]
        A1[HTTP エンドポイント]
        A2[リクエスト/レスポンス処理]
        A3[バリデーション - Zod]
        A4[Swagger UI]
    end

    subgraph Service["Service Layer (サービス層)"]
        S1[ビジネスロジック]
        S2[データ変換]
        S3[トランザクション管理]
    end

    subgraph Domain["Domain Layer (ドメイン層)"]
        D1[データベースアクセス]
        D2[CRUD 操作]
        D3[Prisma Client]
    end

    Application -->|呼び出し| Service
    Service -->|呼び出し| Domain
    Domain -->|MySQL| DB[(Database)]

    style Application fill:#87CEEB
    style Service fill:#90EE90
    style Domain fill:#FFFFE0
    style DB fill:#FFB6C1
```

#### API エンドポイント

詳細は Swagger UI（`http://localhost:3000/docs`）を参照してください。

**システム**
- `GET /` - API 情報
- `GET /health` - ヘルスチェック
- `GET /docs` - Swagger UI

**商品 API**
- `POST /products` - 商品作成
- `GET /products` - 商品一覧取得
- `GET /products/:id` - 商品詳細取得
- `PUT /products/:id` - 商品更新
- `DELETE /products/:id` - 商品削除

#### テスト戦略

- **ユニットテスト**: 各層（Schema, Domain, Service）を独立してテスト
- **統合テスト**: API エンドポイントのエンドツーエンドテスト
- **テストデータベース分離**: 本番 DB とテスト DB を分離

#### コーディング規約

- **コミットメッセージ**: [Conventional Commits](https://www.conventionalcommits.org/ja/) に従う
  - `feat:` 新機能
  - `fix:` バグ修正
  - `docs:` ドキュメント変更
  - `refactor:` リファクタリング
  - `test:` テスト追加・修正
  - `chore:` ビルド・補助ツール変更

- **コードスタイル**: ESLint + Prettier による自動フォーマット
- **命名規則**: TypeScript 標準に準拠（camelCase, PascalCase）

**[⬆ back to top](#構成)**

## 参照

- [Prisma Documentation](https://www.prisma.io/docs)
- [Fastify Documentation](https://www.fastify.io/docs/latest/)
- [Zod Documentation](https://zod.dev/)
- [Vitest Documentation](https://vitest.dev/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
