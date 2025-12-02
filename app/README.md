# DDD書籍管理システム

ドメイン駆動設計（DDD）の実践入門を学ぶための書籍管理システムです。

## 概要

このプロジェクトは「つくりながら学ぶドメイン駆動設計実践入門」の記事を基に実装されたサンプルアプリケーションです。

### 主な機能

- 書籍の登録・取得
- 在庫管理
- ISBN重複チェック
- REST API提供
- Excel Script-Labクライアント

### アーキテクチャ

- **オニオンアーキテクチャ**を採用
- **ドメイン駆動設計（DDD）**のタクティカルパターンを実装
  - 値オブジェクト（Value Object）
  - エンティティ（Entity）
  - 集約（Aggregate）
  - ドメインサービス（Domain Service）
  - リポジトリ（Repository）
  - アプリケーションサービス（Application Service）
  - ドメインイベント（Domain Event）

### 技術スタック

- **言語**: TypeScript
- **ランタイム**: Node.js
- **フレームワーク**: Express.js
- **ORM**: Prisma v7
- **データベース**: PostgreSQL
- **テスト**: Jest
- **Linter**: ESLint
- **DIコンテナ**: tsyringe

## セットアップ

### 必要な環境

- Node.js 18以上
- Docker & Docker Compose
- npm

### インストール

```bash
# 依存関係のインストール
npm install

# データベースの起動
docker compose up -d

# Prisma Clientの生成
npm run db:generate

# マイグレーションの実行
npm run migrate:dev
```

### 環境変数

`.env`ファイルにデータベース接続情報を設定してください：

```env
DATABASE_URL="postgresql://postgres:password@localhost:5432/localdb"
```

## 使い方

### 開発サーバーの起動

```bash
npm run dev
```

APIサーバーが http://localhost:3000 で起動します。

### ビルド

```bash
npm run build
```

### 本番環境での起動

```bash
npm start
```

### テストの実行

```bash
npm test
```

### Lintの実行

```bash
npm run lint
```

### データベース操作

```bash
# マイグレーションの作成と適用（開発環境）
npm run migrate:dev

# マイグレーションの適用（本番環境）
npm run migrate:deploy

# データベースのリセット
npm run migrate:reset

# Prisma Studioの起動
npm run db:studio
```

## API エンドポイント

### 書籍の登録

```http
POST /book
Content-Type: application/json

{
  "isbn": "9784167158057",
  "title": "吾輩は猫である",
  "priceAmount": 770
}
```

### 書籍の取得

```http
GET /book/:isbn
```

**レスポンス例:**

```json
{
  "bookId": "9784167158057",
  "title": "吾輩は猫である",
  "price": 770,
  "stockId": "xxx",
  "quantityAvailable": 0,
  "status": "在庫切れ"
}
```

## Excel Script-Lab クライアント

Excelから書籍管理システムを操作できるクライアントを提供しています。

詳細は [script-lab/README.md](./script-lab/README.md) を参照してください。

### 主な機能

- 書籍の単一登録・取得
- シートデータからの一括登録
- シートのISBNからの一括取得
- シートへのデータ出力

## プロジェクト構成

```
app/
├── src/
│   ├── Domain/              # ドメイン層
│   │   ├── models/          # エンティティ・値オブジェクト
│   │   ├── services/        # ドメインサービス
│   │   └── shared/          # 共通クラス
│   ├── Application/         # アプリケーション層
│   │   ├── Book/            # 書籍アプリケーションサービス
│   │   └── shared/          # 共通インターフェース
│   ├── Infrastructure/      # インフラストラクチャ層
│   │   └── Prisma/          # Prismaリポジトリ実装
│   ├── Presentation/        # プレゼンテーション層
│   │   └── Express/         # Express REST API
│   └── Program.ts           # DIコンテナ設定
├── prisma/
│   ├── schema.prisma        # データベーススキーマ
│   └── migrations/          # マイグレーション履歴
├── script-lab/              # Excel Script-Labクライアント
├── docker-compose.yml       # データベース環境
├── eslint.config.js         # ESLint設定
├── jest.config.js           # Jest設定
├── tsconfig.json            # TypeScript設定
└── package.json
```

## 設計原則

### レイヤー依存関係

```
Presentation → Application → Domain
     ↓              ↓
Infrastructure ←────┘
```

- ドメイン層は他の層に依存しない
- アプリケーション層はドメイン層のみに依存
- インフラストラクチャ層とプレゼンテーション層は外側の層

### ESLintによる依存関係チェック

`eslint-plugin-import`の`no-restricted-paths`ルールにより、不正な層間依存を防止しています。

### DIコンテナ

tsynringeを使用してインフラストラクチャ層の実装を注入し、テスタビリティを向上させています。

## テスト戦略

- **単体テスト**: ドメインモデルのロジックをテスト
- **統合テスト**: アプリケーションサービスの動作をテスト
- **TDDサイクル**: Red → Green → Refactor

全56テストが実装されています。

## 参考資料

- [つくりながら学ぶドメイン駆動設計実践入門](../../docs/wiki/記事/開発/つくりながら学ぶドメイン駆動設計実践入門/)
- [Prisma Documentation](https://www.prisma.io/docs)
- [tsyringe](https://github.com/microsoft/tsyringe)

## ライセンス

ISC

## 作者

このプロジェクトは学習目的で作成されました。
