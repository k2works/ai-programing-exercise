# 販売管理システム データベース設計（TypeScript版）

TDDで育てる販売管理システムのデータベース設計プロジェクトです。

## 前提条件

- Node.js (v18以上推奨)
- npm (Node.jsに同梱)
- Docker & Docker Compose（推奨）
- Git

**または**

- PostgreSQL (v14以上推奨) または MySQL (v8.0以上推奨)

## セットアップ

### 方法1: Docker Compose を使用する（推奨）

Docker を使用すると、データベース環境を簡単にセットアップできます。

#### 1. 依存パッケージのインストール

```bash
npm install
```

#### 2. 環境変数の設定

`.env.example` をコピーして `.env` を作成します（既に作成済みの場合はスキップ）。

```bash
cp .env.example .env
```

必要に応じて `.env` ファイルを編集します。デフォルトでは PostgreSQL を使用します。

```env
# PostgreSQL（デフォルト）
DB_TYPE=postgres
DATABASE_URL="postgresql://postgres:postgres@localhost:5432/sales_management?schema=public"

# MySQL を使用する場合は、以下のようにコメントを切り替えます
# DB_TYPE=mysql
# DATABASE_URL="mysql://user:password@localhost:3306/sales_management"
```

#### 3. データベースコンテナの起動

```bash
# PostgreSQL を起動
docker-compose up -d postgres

# または MySQL を起動
docker-compose up -d mysql

# すべてのサービスを起動（PostgreSQL + MySQL + Adminer）
docker-compose up -d
```

#### 4. データベースの確認

Adminer（データベース管理ツール）にアクセス：
- URL: http://localhost:8080
- PostgreSQL の場合:
  - システム: PostgreSQL
  - サーバ: postgres
  - ユーザ名: postgres
  - パスワード: postgres
  - データベース: sales_management
- MySQL の場合:
  - システム: MySQL
  - サーバ: mysql
  - ユーザ名: user
  - パスワード: password
  - データベース: sales_management

#### 5. Prismaのセットアップ

スキーマにモデルを追加したら、以下のコマンドを実行します。

```bash
# マイグレーション実行
npm run prisma:migrate

# Prisma Client生成
npm run prisma:generate
```

### 方法2: ローカルのデータベースを使用する

#### 1. 依存パッケージのインストール

```bash
npm install
```

#### 2. 環境変数の設定

`.env` ファイルを編集して、データベース接続情報を設定します。

```env
# PostgreSQL の場合
DATABASE_URL="postgresql://user:password@localhost:5432/sales_management?schema=public"

# MySQL の場合
DATABASE_URL="mysql://user:password@localhost:3306/sales_management"
```

#### 3. データベースのセットアップ

**PostgreSQL の場合:**

```bash
# PostgreSQLにログイン
psql -U postgres

# データベース作成
CREATE DATABASE sales_management;
```

**MySQL の場合:**

```bash
# MySQLにログイン
mysql -u root -p

# データベース作成
CREATE DATABASE sales_management CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
```

また、`prisma/schema.prisma` の `datasource db` を MySQL 用に変更します。

```prisma
datasource db {
  provider = "mysql"  // "postgresql" から "mysql" に変更
  url      = env("DATABASE_URL")
}
```

#### 4. Prismaのセットアップ

```bash
# マイグレーション実行
npm run prisma:migrate

# Prisma Client生成
npm run prisma:generate
```

## 開発コマンド

### テスト

```bash
# テスト実行
npm run test

# ウォッチモード
npm run test:watch

# カバレッジ
npm run test:coverage
```

### コード品質

```bash
# 静的コード解析
npm run lint

# 自動修正
npm run lint:fix

# フォーマット
npm run format

# フォーマットチェック
npm run format:check
```

### Gulp タスク

```bash
# 全体チェック（自動修正付き）
npm run check

# 自動監視モード
npm run guard

# Gulpタスク一覧
npm run gulp -- --tasks
```

### Prisma

```bash
# マイグレーション
npm run prisma:migrate

# Prisma Client生成
npm run prisma:generate

# Prisma Studio起動
npm run prisma:studio
```

### Zod によるスキーマ検証

Prisma スキーマから自動的に Zod バリデーションスキーマが生成されます。

```bash
# Zod スキーマの生成（prisma:generate と同時に実行されます）
npm run prisma:generate
```

生成された Zod スキーマは `src/generated/zod/` に出力されます。

**使用例:**

```typescript
import { UserSchema } from './generated/zod'

// バリデーション
const result = UserSchema.safeParse({
  name: 'John Doe',
  email: 'john@example.com'
})

if (result.success) {
  console.log('Valid data:', result.data)
} else {
  console.error('Validation errors:', result.error)
}
```

**生成されるスキーマの種類:**

- 基本スキーマ: `UserSchema`
- Partial スキーマ: `UserPartialSchema`
- Optional デフォルト値スキーマ: `UserOptionalDefaultsSchema`
- リレーション値スキーマ: `UserWithRelationsSchema`

### Docker Compose

```bash
# コンテナの起動
docker-compose up -d

# 特定のサービスのみ起動
docker-compose up -d postgres    # PostgreSQL のみ
docker-compose up -d mysql       # MySQL のみ
docker-compose up -d adminer     # Adminer のみ

# コンテナの停止
docker-compose stop

# コンテナの停止と削除
docker-compose down

# コンテナの停止、削除、ボリュームも削除
docker-compose down -v

# ログの確認
docker-compose logs -f

# 特定のサービスのログ確認
docker-compose logs -f postgres
docker-compose logs -f mysql

# コンテナの状態確認
docker-compose ps

# コンテナに接続
docker-compose exec postgres psql -U postgres -d sales_management
docker-compose exec mysql mysql -u user -p sales_management
```

## ディレクトリ構造

```
db/typescript/
├── docker/                      # Docker関連ファイル
│   ├── postgres/
│   │   └── init/
│   │       └── 01-init.sql     # PostgreSQL初期化スクリプト
│   └── mysql/
│       ├── init/
│       │   └── 01-init.sql     # MySQL初期化スクリプト
│       └── conf.d/
│           └── my.cnf           # MySQL設定ファイル
├── prisma/
│   └── schema.prisma            # Prismaスキーマ定義
├── src/
│   └── app.test.ts              # テストファイル
├── .env                         # 環境変数
├── .env.example                 # 環境変数サンプル
├── .gitignore                   # Git除外設定
├── .prettierrc                  # Prettier設定
├── docker-compose.yml           # Docker Compose設定
├── eslint.config.js             # ESLint設定
├── gulpfile.js                  # Gulp設定
├── package.json                 # パッケージ設定
├── README.md                    # このファイル
├── tsconfig.json                # TypeScript設定
└── vitest.config.ts             # Vitest設定
```

## 開発フロー

TDDサイクルに従って開発を進めます：

1. **Red（失敗）**: 失敗するテストを書く
2. **Green（成功）**: テストを通す最小限のコードを実装
3. **Refactor（改善）**: コードをリファクタリング

## 参考資料

- [実践データベース設計_TypeScript.md](../../docs/wiki/記事/データベース/実践データベース設計_TypeScript.md)
- [Prisma公式ドキュメント](https://www.prisma.io/docs)
- [Vitest公式ドキュメント](https://vitest.dev)
