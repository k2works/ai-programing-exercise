# 財務会計システム（TypeScript版）

TDDで育てる財務会計システムのデータベース設計プロジェクト

## 概要

このプロジェクトは、テスト駆動開発（TDD）の原則をデータベース設計に応用し、小さな要求から始めて財務会計システムのデータベースを段階的に育てていく実践的なプロジェクトです。

## 前提条件

以下のツールがインストールされていることを確認してください。

- Node.js (v18以上推奨)
- npm (Node.jsに同梱)
- Docker & Docker Compose（推奨）
- Git

**または**

- PostgreSQL (v14以上推奨) または MySQL (v8.0以上推奨)

## セットアップ

### 1. 依存関係のインストール

```bash
npm install
```

必要なパッケージ：

- TypeScript
- Prisma & @prisma/client
- Zod & zod-prisma-types
- Vitest (テストフレームワーク)

### 2. データベースの起動

#### Docker を使用する場合（推奨）

PostgreSQL を起動：

```bash
docker-compose up -d postgres
```

MySQL を起動：

```bash
docker-compose up -d mysql
```

すべてのサービスを起動（PostgreSQL + MySQL + Adminer）：

```bash
docker-compose up -d
```

#### Adminer（データベース管理ツール）へのアクセス

- URL: http://localhost:8080
- **PostgreSQL の場合:**
  - システム: PostgreSQL
  - サーバ: postgres
  - ユーザ名: postgres
  - パスワード: postgres
  - データベース: accounting_system
- **MySQL の場合:**
  - システム: MySQL
  - サーバ: mysql
  - ユーザ名: user
  - パスワード: password
  - データベース: accounting_system

### 3. Prisma のセットアップ

```bash
# Prisma Client の生成
npx prisma generate

# マイグレーションの実行（モデル定義後）
npx prisma migrate dev
```

## 利用可能なコマンド

### テスト

```bash
# テストの実行
npm test

# テストのウォッチモード
npm run test:watch

# カバレッジレポートの生成
npm run test:coverage
```

### コード品質

```bash
# リント
npm run lint

# リント自動修正
npm run lint:fix

# フォーマット
npm run format

# フォーマットチェック
npm run format:check
```

### データベース

```bash
# Prisma Studio の起動（データベースGUI）
npx prisma studio

# マイグレーションの作成
npx prisma migrate dev --name <migration_name>

# マイグレーションのリセット
npx prisma migrate reset
```

### Docker Compose

```bash
# コンテナの停止
docker-compose stop

# コンテナの停止と削除
docker-compose down

# コンテナの停止、削除、ボリュームも削除
docker-compose down -v

# ログの確認
docker-compose logs -f postgres
docker-compose logs -f mysql

# コンテナの状態確認
docker-compose ps

# コンテナに接続
docker-compose exec postgres psql -U postgres -d accounting_system
docker-compose exec mysql mysql -u user -p accounting_system
```

## プロジェクト構造

```
.
├── docker/                  # Docker 初期化スクリプト
│   ├── postgres/
│   │   └── init/
│   └── mysql/
│       ├── init/
│       └── conf.d/
├── prisma/                  # Prisma スキーマ
│   └── schema.prisma
├── src/                     # ソースコード
│   ├── generated/          # 自動生成ファイル（.gitignoreに含まれる）
│   └── *.test.ts           # テストファイル
├── .env                     # 環境変数（.gitignoreに含まれる）
├── .env.example            # 環境変数のサンプル
├── docker-compose.yml      # Docker Compose 設定
├── package.json            # npm 設定
├── tsconfig.json           # TypeScript 設定
└── vitest.config.ts        # Vitest 設定
```

## 開発の進め方

1. **テストファースト**: 機能を実装する前に、まずテストを書く
2. **小さなステップ**: 一度に1つの機能に集中
3. **リファクタリング**: テストがグリーンになった後、コードを改善
4. **段階的な設計**: 必要に応じてデータベーススキーマを進化させる

## 参考リンク

- [Prisma ドキュメント](https://www.prisma.io/docs/)
- [Vitest ドキュメント](https://vitest.dev/)
- [Zod ドキュメント](https://zod.dev/)

## ライセンス

MIT
